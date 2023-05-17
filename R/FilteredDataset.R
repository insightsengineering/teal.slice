# FilteredDataset abstract --------
#' @title `FilterStates` R6 class
#' @description
#' `FilteredDataset` is a class which renders/controls `FilterStates`(s)
#' Each `FilteredDataset` contains `filter_states` field - a `list` which contains one
#' (`data.frame`) or multiple (`MultiAssayExperiment`) `FilterStates` objects.
#' Each `FilterStates` is responsible for one filter/subset expression applied for specific
#' components of the dataset.
#' @keywords internal
FilteredDataset <- R6::R6Class( # nolint
  "FilteredDataset",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initializes this `FilteredDataset` object
    #'
    #' @param dataset (`data.frame` or `MultiAssayExperiment`)\cr
    #'  single dataset for which filters are rendered
    #' @param dataname (`character(1)`)\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   Vector with primary keys
    #' @param label (`character(1)`)\cr
    #'   Label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    initialize = function(dataset, dataname, keys = character(0), label = attr(dataset, "label"), metadata = NULL, ...) {
      logger::log_trace("Instantiating { class(self)[1] }, dataname: { dataname }")

      # dataset assertion in child classes
      check_simple_name(dataname)
      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert_character(label, null.ok = TRUE)
      teal.data::validate_metadata(metadata)
      logger::log_trace("Instantiating { class(self)[1] }, dataname: { dataname }")
      private$data <- dataset
      private$dataname <- dataname
      private$keys <- keys
      private$label <- if (is.null(label)) character(0) else label
      private$metadata <- metadata
      private$state_list <- reactiveVal()

      # function executing reactive call and returning data
      private$data_filtered_fun <- function(sid = "") {
        checkmate::assert_character(sid)
        if (identical(sid, integer(0))) {
          logger::log_trace("filtering data dataname: { private$dataname }")
        } else {
          logger::log_trace("filtering data dataname: { dataname }, sid: { sid }")
        }
        env <- new.env(parent = parent.env(globalenv()))
        env[[dataname]] <- private$data
        filter_call <- self$get_call(sid)
        eval_expr_with_msg(filter_call, env)
        get(x = dataname, envir = env)
      }

      private$data_filtered <- reactive(private$data_filtered_fun())
      logger::log_trace("Instantiated { class(self)[1] }, dataname: { private$dataname }")
      invisible(self)
    },

    # managing filter states -----

    # getters ----
    #' @description
    #' Gets a filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #'
    #' @param sid (`character`)\cr
    #'  when specified then method returns code containing filter conditions of
    #'  `FilterState` objects which `"sid"` attribute is different than this `sid` argument.
    #'
    #' @return filter `call` or `list` of filter calls
    get_call = function(sid = "") {
      filter_call <- NULL
      if (length(filter_call) == 0) {
        return(NULL)
      }
      filter_call
    },

        #' @description
    #' Remove one or more `FilterState`s from the `state_list` along with their UI elements.
    #'
    #' @param state (`teal_slices`)\cr
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")

      lapply(state, function(x) {
        state_id <- get_teal_slice_id(x)
        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removing filter, dataname: { x$dataname }; state_id: { state_id }"
        )
        private$state_list_remove(state_id = state_id)
        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removed filter, dataname: { x$dataname }; state_id: { state_id }"
        )
      })

      invisible(NULL)
    },

    #' @description
    #' Gets reactive values from active `FilterState` objects.
    #'
    #' Get active filter state from `FilterState` objects stored in `state_list`(s).
    #' The output is a list compatible with input to `self$set_filter_state`.
    #'
    #' @return `list` containing `list` per `FilterState` in the `state_list`
    #'
    get_filter_state = function() {
      slices <- unname(lapply(private$state_list(), function(x) x$get_state()))
      fs <- do.call(filter_settings, c(slices, list(count_type = private$count_type)))

      include_varnames <- private$include_varnames
      if (length(include_varnames)) {
        attr(fs, "include_varnames") <- structure(
          list(include_varnames),
          names = private$dataname
        )
      }

      exclude_varnames <- private$exclude_varnames
      if (length(exclude_varnames)) {
        attr(fs, "exclude_varnames") <- structure(
          list(exclude_varnames),
          names = private$dataname
        )
      }

      return(fs)
    },

    #' @description
    #' Sets active `FilterState` objects.
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @return function which throws an error
    set_filter_state = function(state) {
      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) {
        checkmate::assert_true(x$dataname == private$dataname, .var.name = "dataname matches private$dataname")
      })

      private$set_filterable_varnames(
        include_varnames = attr(state, "include_varnames")[[private$dataname]],
        exclude_varnames = attr(state, "exclude_varnames")[[private$dataname]]
      )

      count_type <- attr(state, "count_type")
      if (length(count_type)) {
        private$count_type <- count_type
      }

      # Drop teal_slices that refer to excluded variables.
      varnames <- slices_field(state, "varname")
      excluded_varnames <- NULL #setdiff(varnames, private$get_filterable_varnames())
      if (length(excluded_varnames)) {
        state <- slices_which(
          state,
          sprintf("!varname %%in%% c(%s)", toString(dQuote(excluded_varnames, q = FALSE)))
        )
        logger::log_warn("filters for columns: { toString(excluded_varnames) } excluded from { private$dataname }")
      }

      if (length(state) > 0) {
        private$set_filter_state_impl(
          state = state,
          data = private$data,
          data_reactive = private$data_filtered_fun
        )
      }

      logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")

      invisible(NULL)
    },

    #' @description
    #' Remove all `FilterState` objects from this `FilterStates` object.
    #'
    #' @return NULL
    #'
    clear_filter_states = function() {
      private$state_list_empty()
    },

    #' @description
    #' Gets the number of active `FilterState` objects in all `FilterStates` in this `FilteredDataset`.
    #' @return `integer(1)`
    get_filter_count = function() {
      length(self$get_filter_state())
    },

    #' @description
    #' Gets the name of the dataset
    #'
    #' @return `character(1)` as a name of this dataset
    get_dataname = function() {
      private$dataname
    },

    #' @description
    #' Gets the dataset object in this `FilteredDataset`
    #' @param filtered (`logical(1)`)\cr
    #'
    #' @return `data.frame` or `MultiAssayExperiment`, either raw
    #'  or as a reactive with current filters applied
    #'
    get_dataset = function(filtered = FALSE) {
      if (filtered) {
        private$data_filtered
      } else {
        private$data
      }
    },

    #' @description
    #' Gets the metadata for the dataset in this `FilteredDataset`
    #' @return named `list` or `NULL`
    get_metadata = function() {
      private$metadata
    },

    #' @description
    #' Get filter overview rows of a dataset
    #' The output shows the comparison between `filtered_dataset`
    #' function parameter and the dataset inside self
    #' @param filtered_dataset comparison object, of the same class
    #' as `self$get_dataset()`, if `NULL` then `self$get_dataset()`
    #' is used.
    #' @return (`data.frame`) matrix of observations and subjects
    get_filter_overview = function() {
      dataset <- self$get_dataset()
      data_filtered <- self$get_dataset(TRUE)
      data.frame(
        dataname = private$dataname,
        obs = nrow(dataset),
        obs_filtered = nrow(data_filtered)
      )
    },

    #' @description
    #' Gets the keys for the dataset of this `FilteredDataset`
    #' @return (`character`) the keys of dataset
    get_keys = function() {
      private$keys
    },

    #' @description
    #' Gets the dataset label
    #' @return (`character`) the dataset label
    get_dataset_label = function() {
      private$label
    },

    # modules ------
    #' @description
    #' UI module for dataset active filters
    #'
    #' UI module containing dataset active filters along with
    #' title and remove button.
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_active = function(id) {
      dataname <- self$get_dataname()
      checkmate::assert_string(dataname)

      ns <- NS(id)
      span(
        id = id,
        include_css_files("filter-panel"),
        div(
          id = ns("whole_ui"), # to hide it entirely
          fluidRow(
            column(
              width = 8,
              tags$span(dataname, class = "filter_panel_dataname")
            ),
            column(
              width = 4,
              tagList(
                actionLink(
                  ns("remove_filters"),
                  label = "",
                  icon = icon("circle-xmark", lib = "font-awesome"),
                  class = "remove pull-right"
                ),
                actionLink(
                  ns("collapse"),
                  label = "",
                  icon = icon("angle-down", lib = "font-awesome"),
                  class = "remove pull-right"
                )
              )
            )
          ),
          shinyjs::hidden(
            div(
              id = ns("filter_count_ui"),
              tagList(
                textOutput(ns("filter_count")),
                br()
              )
            )
          ),
          div(
            # id needed to insert and remove UI to filter single variable as needed
            # it is currently also used by the above module to entirely hide this panel
            id = ns("filters"),
            class = "parent-hideable-list-group",
            tagList(
              include_css_files(pattern = "filter-panel"),
              tags$div(
                id = ns("filters_container"),
                class = "accordion",
                `data-label` = ifelse(length(private$datalabel), "", paste0("> ", private$datalabel)),
                shiny::tagList(uiOutput(ns("filter_states"), inline = TRUE))
              )
            )
          )
        )
      )
    },

    #' @description
    #' Server module for a dataset active filters
    #'
    #' Server module managing a  active filters.
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    srv_active = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          dataname <- self$get_dataname()
          logger::log_trace("FilteredDataset$srv_active initializing, dataname: { dataname }")
          checkmate::assert_string(dataname)
          output$filter_count <- renderText(
            length(self$get_filter_state())
          )

          shiny::observeEvent(self$get_filter_state(), {
            shinyjs::hide("filter_count_ui")
            shinyjs::show("filters")
            shinyjs::toggle("remove_filters", condition = length(self$get_filter_state()) != 0)
            shinyjs::toggle("collapse", condition = length(self$get_filter_state()) != 0)
          })

          shiny::observeEvent(input$collapse, {
            shinyjs::toggle("filter_count_ui")
            shinyjs::toggle("filters")
            toggle_icon(session$ns("collapse"), c("fa-angle-right", "fa-angle-down"))
          })

          observeEvent(input$remove_filters, {
            logger::log_trace("FilteredDataset$srv_active@1 removing filters, dataname: { dataname }")
            self$clear_filter_states()
            logger::log_trace("FilteredDataset$srv_active@1 removed filters, dataname: { dataname }")
          })


          current_state <- reactive(private$state_list_get())
          previous_state <- reactiveVal(character(0))
          added_state_name <- reactiveVal(character(0))

          observeEvent(current_state(), {
            logger::log_trace("FilterStates$srv_active@1 determining added and removed filter states")
            added_state_name(setdiff(names(current_state()), names(previous_state())))
            previous_state(current_state())
          })

          output[["filter_states"]] <- shiny::renderUI({
            fstates <- current_state() # rerenders when queue changes / not when the state changes
            lapply(names(fstates), function(fname) {
              private$ui_card_module(id = session$ns(fname), fstates[[fname]])
            })
          })

          observeEvent(
            added_state_name(), # we want to call FilterState module only once when it's added
            ignoreNULL = TRUE,
            {
              fstates <- current_state()
              lapply(added_state_name(), function(fname) {
                private$srv_card_module(id = fname, element_id = fname, fs = fstates[[fname]])
              })
              added_state_name(character(0))
            }
          )

          logger::log_trace("FilteredDataset$srv_active initialized, dataname: { dataname }")

          NULL
        }
      )
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add = function(id) {
      stop("Pure virtual method")
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add` calls multiple modules
    #' of the same name from `FilterStates` as `MAEFilteredDataset`
    #' contains one `FilterStates` object for `colData` and one for each
    #' experiment.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #'
    #' @return `moduleServer` function which returns `NULL`
    #'
    srv_add = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("MAEFilteredDataset$srv_add initializing, dataname: { deparse1(self$get_dataname()) }")
          elems <- private$get_filter_states()
          elem_names <- names(private$get_filter_states())
          lapply(
            elem_names,
            function(elem_name) {
              NULL
              #elems[[elem_name]]$srv_add(elem_name)
            }
          )
          logger::log_trace("MAEFilteredDataset$srv_add initialized, dataname: { deparse1(self$get_dataname()) }")
          NULL
        }
      )
    }
  ),
  ## __Private Fields ====
  private = list(
    count_type = "none",
    data = NULL, # data.frame or MultiAssayExperiment
    data_filtered = NULL,
    data_filtered_fun = NULL, # function
    dataname = character(0),
    filter_states = list(),
    exclude_varnames = character(0),
    include_varnames = character(0),
    keys = character(0),
    label = character(0),
    metadata = NULL,
    state_list = NULL,

    # @description
    # Returns a list of `FilterState` objects stored in this `FilterStates`.
    #
    # @param state_id (`character(1)`)\cr
    #   name of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return `list` of `FilterState` objects
    #
    state_list_get = function(state_id = NULL) {
      checkmate::assert_string(state_id, null.ok = TRUE)

      if (is.null(state_id)) {
        private$state_list()
      } else {
        private$state_list()[[state_id]]
      }
    },

    # @description
    # Adds a new `FilterState` object to this `FilterStates`.\cr
    # Raises error if the length of `x` does not match the length of `state_id`.
    #
    # @param x (`FilterState`)\cr
    #   object to be added to filter state list
    # @param state_id (`character(1)`)\cr
    #   name of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return NULL
    #
    state_list_push = function(x, state_id) {
      logger::log_trace("{ class(self)[1] } pushing into state_list, dataname: { private$dataname }")
      checkmate::assert_string(state_id)
      checkmate::assert_multi_class(x, c("FilterState", "FilterStateExpr"))
      state <- stats::setNames(list(x), state_id)
      new_state_list <- c(
        shiny::isolate(private$state_list()),
        state
      )
      shiny::isolate(private$state_list(new_state_list))

      logger::log_trace("{ class(self)[1] } pushed into queue, dataname: { private$dataname }")
      invisible(NULL)
    },

    # @description
    # Removes a single filter state with all associated shiny elements:\cr
    # * specified `FilterState` from `private$state_list`
    # * UI card created for this filter
    # * observers tracking the selection and remove button
    #
    # @param state_id (`character`)\cr
    #   names of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return NULL
    #
    state_list_remove = function(state_id) { # todo: state_id as vector
      logger::log_trace("{ class(self)[1] } removing a filter, state_id: { state_id }")
      checkmate::assert_character(state_id)
      new_state_list <- shiny::isolate(private$state_list())
      if (is.element(state_id, names(new_state_list))) {
        new_state_list[[state_id]]$destroy_observers()
        new_state_list[[state_id]] <- NULL
        shiny::isolate(private$state_list(new_state_list))
        logger::log_trace("{ class(self)[1] } removed a filter, state_id: { state_id }")
      } else {
        warning(sprintf("\"%s\" not found in state list", state_id))
      }

      invisible(NULL)
    },

    # @description
    # Remove all `FilterState` objects from this `FilterStates` object.
    #
    # @return invisible NULL
    #
    state_list_empty = function() {
      logger::log_trace("{ class(self)[1] }$state_list_empty removing all filters for dataname: { private$dataname }")

      state_list <- shiny::isolate(private$state_list())
      for (state_id in names(state_list)) {
        private$state_list_remove(state_id)
      }

      logger::log_trace("{ class(self)[1] }$state_list_empty removed all filters for dataname: { private$dataname }")
      invisible(NULL)
    },

    # @description
    # Set filter state
    #
    # Utility method for `set_filter_state` to create or modify `FilterState` using a single
    #  `teal_slice`.
    # @param state (`teal_slices`)
    # @param data (`data.frame`, `matrix` or `DataFrame`)
    # @param data_reactive (`function`)
    #  function having `sid` as argument
    #
    # @return invisible NULL
    #
    set_filter_state_impl = function(state,
                                     data,
                                     data_reactive) {
      checkmate::assert_class(state, "teal_slices")
      #checkmate::assert_multi_class(data, c("data.frame", "matrix", "DataFrame", "HermesData"))
      checkmate::assert_function(data_reactive, args = "sid")
      if (length(state) == 0L) {
        return(invisible(NULL))
      }

      states_id <- vapply(state, get_teal_slice_id, character(1))
      state_list <- shiny::isolate(private$state_list_get())
      if (any(duplicated(states_id))) {
        stop(
          "Some of the teal_slice objects refer to the same filter. ",
          "Please specify different 'id' when calling filter_var or filter_expr"
        )
      }

      lapply(seq_along(state), function(i) {
        state_id <- states_id[i]
        if (state_id %in% names(state_list)) {
          # Modify existing filter states.
          state_list[[state_id]]$set_state(state[[i]])
        } else if (inherits(state[[i]], "teal_slice_expr")) {
          # create a new FilterStateExpr
          fstate <- do.call(init_filter_state_expr, state[[i]])
          private$state_list_push(x = fstate, state_id = state_id)
        } else {
          # create a new FilterState
          data <- pull_vector(data, state[[i]])
          arg_list <- list(
            x = data,
            # data_reactive is a function which eventually calls get_call(sid).
            # This chain of calls returns column from the data filtered by everything
            # but filter identified by the sid argument. FilterState then get x_reactive
            # and this no longer needs to be a function to pass sid. reactive in the FilterState
            # is also beneficial as it can be cached and retriger filter counts only if
            # returned vector is different.
            x_reactive = if (private$count_type == "none") {
              reactive(NULL)
            } else {
              reactive(pull_vector(data_reactive(state_id), state[[i]]))
            },
            extract_type = if (inherits(state[[i]], "teal_slice_mae_subjects")) {
              "list"
            } else {
              character(0)
            }
          )
          arg_list <- append(arg_list, state[[i]])
          fstate <- do.call(init_filter_state, arg_list)
          private$state_list_push(x = fstate, state_id = state_id)
        }
      })

      invisible(NULL)
    },

    # @description
    # Set the allowed filterable variables
    # @param include_varnames (`character`) Names of variables included in filtering.
    # @param exclude_varnames (`character`) Names of variables excluded from filtering.
    #
    # @details When retrieving the filtered variables only
    # those which have filtering supported (i.e. are of the permitted types).
    # Only one from `include_varnames` and `exclude_varnames` can be used in one call. When `exclude_varnames`
    # is called `include_varnames` is cleared - same otherwise.
    # are included.
    #
    # @return NULL invisibly
    set_filterable_varnames = function(include_varnames = character(0), exclude_varnames = character(0)) {
      if ((length(include_varnames) + length(exclude_varnames)) == 0L) {
        return(invisible(NULL))
      }
      checkmate::assert_character(include_varnames, any.missing = FALSE, min.len = 0L, null.ok = TRUE)
      checkmate::assert_character(exclude_varnames, any.missing = FALSE, min.len = 0L, null.ok = TRUE)
      if (length(include_varnames) && length(exclude_varnames)) {
        stop(
          "`include_varnames` and `exclude_varnames` has been both specified for",
          private$dataname,
          ". Only one per dataset is allowed.",
        )
      }
      supported_vars <- get_supported_filter_varnames(private$data)
      if (length(include_varnames)) {
        private$include_varnames <- intersect(include_varnames, supported_vars)
        private$exclude_varnames <- character(0)
      } else {
        private$exclude_varnames <- exclude_varnames
        private$include_varnames <- character(0)
      }
      invisible(NULL)
    },

      # @description
    # Get vector of filterable varnames
    #
    # @details
    #  These are the only columns which can be used in the filter panel
    #
    # @return character vector with names of the columns
    get_filterable_varnames = function() {
      if (length(private$include_varnames)) {
        private$include_varnames
      } else {
        supported_varnames <- get_supported_filter_varnames(private$data)
        setdiff(supported_varnames, private$exclude_varnames)
      }
    },

    #' UI wrapping a single `FilterState`
    #'
    #' This module contains a single `FilterState` card and remove (from the `ReactiveQueue`) button.
    #'
    #' return `moduleServer` function which returns `NULL`
    #' @keywords internal
    ui_card_module = function(id, fs) {
      ns <- NS(id)
      fs$ui(id = ns("content"))
    },

    #' Server module for a single `FilterState`
    #'
    #' Calls server from `FilterState` and observes remove (from the `ReactiveQueue`) button
    #' @keywords internal
    srv_card_module = function(id, element_id, fs) {
      moduleServer(id, function(input, output, session) {
        logger::log_trace("FilterStates$srv_card_module initializing, dataname: { private$dataname }")
        fs_callback <- fs$server(id = "content")
        observeEvent(
          eventExpr = fs_callback(), # when remove button is clicked in the FilterState ui
          once = TRUE, # remove button can be called once, should be destroyed afterwards
          handlerExpr = private$state_list_remove(element_id)
        )
      })
    }
  )
)
