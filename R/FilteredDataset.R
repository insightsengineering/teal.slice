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
    initialize = function(dataset, dataname, keys = character(0), label = attr(dataset, "label"), metadata = NULL) {
      logger::log_trace("Instantiating { class(self)[1] }, dataname: { dataname }")

      # dataset assertion in child classes
      check_simple_name(dataname)
      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert_character(label, null.ok = TRUE)
      teal.data::validate_metadata(metadata)

      logger::log_trace("Instantiating { class(self)[1] }, dataname: { dataname }")
      private$dataset <- dataset
      private$dataname <- dataname
      private$keys <- keys
      private$label <- if (is.null(label)) character(0) else label
      private$metadata <- metadata

      # function executing reactive call and returning data
      private$data_filtered_fun <- function(sid = "") {
        checkmate::assert_character(sid)
        if (identical(sid, integer(0))) {
          logger::log_trace("filtering data dataname: { private$dataname }")
        } else {
          logger::log_trace("filtering data dataname: { dataname }, sid: { sid }")
        }
        env <- new.env(parent = parent.env(globalenv()))
        env[[dataname]] <- private$dataset
        filter_call <- self$get_call(sid)
        eval_expr_with_msg(filter_call, env)
        get(x = dataname, envir = env)
      }

      private$data_filtered <- reactive(private$data_filtered_fun())
      logger::log_trace("Instantiated { class(self)[1] }, dataname: { private$dataname }")
      invisible(self)
    },

    #' @description
    #' Returns a formatted string representing this `FilteredDataset` object.
    #'
    #' @param show_all `logical(1)` passed to `format.teal_slice`
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(show_all = FALSE) {
      sprintf(
        "%s:\n%s",
        class(self)[1],
        format(self$get_filter_state(), show_all = show_all)
      )
    },

    #' @description
    #' Prints this `FilteredDataset` object.
    #'
    #' @param ... additional arguments
    #'
    print = function(...) {
      cat(shiny::isolate(self$format(...)), "\n")
    },

    #' @description
    #' Removes all active filter items applied to this dataset
    #' @return NULL
    clear_filter_states = function() {
      logger::log_trace("Removing all non-locked filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      lapply(
        private$get_filter_states(),
        function(filter_states) filter_states$clear_filter_states()
      )
      logger::log_trace("Removed all non-locked filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      NULL
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
      filter_call <- Filter(
        f = Negate(is.null),
        x = lapply(private$get_filter_states(), function(x) x$get_call(sid))
      )
      if (length(filter_call) == 0) {
        return(NULL)
      }
      filter_call
    },

    #' @description
    #' Gets states of all active `FilterState` objects
    #'
    #' @return A `teal_slices` object.
    #'
    get_filter_state = function() {
      states <- unname(lapply(private$get_filter_states(), function(x) x$get_filter_state()))
      states <- Filter(function(x) length(x) != 0L, states)
      do.call(c, states)
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`teal_slice`) object
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      stop("set_filter_state is an abstract class method.")
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
        private$dataset
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
      if_multiple_filter_states <- length(private$get_filter_states()) > 1
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
              lapply(
                names(private$get_filter_states()),
                function(x) {
                  tagList(private$get_filter_states()[[x]]$ui_active(id = ns(x)))
                }
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
            sprintf(
              "%d filter%s applied",
              self$get_filter_count(),
              if (self$get_filter_count() != 1) "s" else ""
            )
          )

          lapply(
            names(private$get_filter_states()),
            function(x) {
              private$get_filter_states()[[x]]$srv_active(id = x)
            }
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
            logger::log_trace("FilteredDataset$srv_active@1 removing all non-locked filters, dataname: { dataname }")
            self$clear_filter_states()
            logger::log_trace("FilteredDataset$srv_active@1 removed all non-locked filters, dataname: { dataname }")
          })

          logger::log_trace("FilteredDataset$initialized, dataname: { dataname }")

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
            function(elem_name) elems[[elem_name]]$srv_add(elem_name)
          )
          logger::log_trace("MAEFilteredDataset$srv_add initialized, dataname: { deparse1(self$get_dataname()) }")
          NULL
        }
      )
    }
  ),
  ## __Private Fields ====
  private = list(
    dataset = NULL, # data.frame or MultiAssayExperiment
    data_filtered = NULL,
    data_filtered_fun = NULL, # function
    filter_states = list(),
    dataname = character(0),
    keys = character(0),
    label = character(0),
    metadata = NULL,

    # Adds `FilterStates` to the `private$filter_states`.
    # `FilterStates` is added once for each element of the dataset.
    # @param filter_states (`FilterStates`)
    # @param id (`character(1)`)
    add_filter_states = function(filter_states, id) {
      checkmate::assert_class(filter_states, "FilterStates")
      checkmate::assert_string(id)
      x <- stats::setNames(list(filter_states), id)
      private$filter_states <- c(private$get_filter_states(), x)
    },

    # @description
    # Gets the active `FilterStates` objects.
    # @param id (`character(1)`, `character(0)`)\cr
    #   the id of the `private$filter_states` list element where `FilterStates` is kept.
    # @return `FilterStates` or `list` of `FilterStates` objects.
    get_filter_states = function() {
      private$filter_states
    }
  )
)
