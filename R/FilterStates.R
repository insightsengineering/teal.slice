#' @title `FilterStates` R6 class
#'
#' @description
#' Abstract class that manages adding and removing `FilterState` objects
#' and builds a \emph{subset expression}.
#'
#' A `FilterStates` object tracks all subsetting expressions
#' (logical predicates that limit observations) associated with a given dataset
#' and composes them into a single reproducible R expression
#' that will assign a subset of the original data to a new variable.
#' This expression is hereafter referred to as \emph{subset expression}.
#'
#' The \emph{subset expression} is constructed differently for different
#' classes of the underlying data object and `FilterStates` subclasses.
#' Currently implemented for `data.frame`, `matrix`,
#' `SummarizedExperiment`, and `MultiAssayExperiment`.
#'
#' @keywords internal
#'
#' @examples
#' library(shiny)
#' filter_states <- teal.slice:::DFFilterStates$new(
#'   data = data.frame(x = 1:2, sex = c("F", "M")),
#'   dataname = "data",
#'   varlabels = c(x = "x variable", sex = "Sex"),
#'   datalabel = character(0),
#'   keys = character(0)
#' )
#' filter_states$set_filter_state(
#'   filter_settings(
#'     filter_var(dataname = "data", varname = "x", selected = 1),
#'     filter_var(dataname = "data", varname = "sex", selected = "F")
#'   )
#' )
#' isolate(filter_states$get_filter_state())
#' isolate(filter_states$get_call())
#'
FilterStates <- R6::R6Class( # nolint
  classname = "FilterStates",

  # public members ----
  public = list(
    #' @description
    #' Initializes `FilterStates` object.
    #'
    #' Initializes `FilterStates` object by setting
    #' `dataname`, and `datalabel`.
    #'
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'   the R object which `subset` function is applied on.
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return an object of the same type as `data` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value
    #' @param excluded_varnames (`character`)\cr
    #'   names of variables that can \strong{not} be filtered on.
    #' @param count_type `character(1)`\cr
    #'   specifying how observations are tallied
    #'
    #' @return
    #' self invisibly
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = character(0),
                          excluded_varnames = character(0),
                          count_type = c("all", "none")) {
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_string(dataname)
      checkmate::assert_character(datalabel, max.len = 1, any.missing = FALSE)

      private$dataname <- dataname
      private$datalabel <- datalabel
      private$data <- data
      private$data_reactive <- data_reactive
      private$filterable_varnames <- setdiff(colnames(data), excluded_varnames)
      private$count_type <- match.arg(count_type)
      private$state_list <- reactiveVal(list())
      logger::log_trace("Instantiated { class(self)[1] }, dataname: { private$dataname }")
      invisible(self)
    },

    #' @description
    #' Returns the label of the dataset.
    #'
    #' @return `character(1)` the data label
    #'
    get_datalabel = function() {
      private$datalabel
    },

    #' @description
    #' Returns a formatted string representing this `FilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces prepended to each line of the output
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      formatted_states <- vapply(
        private$state_list_get(), function(state) state$format(indent = indent),
        USE.NAMES = FALSE, FUN.VALUE = character(1)
      )
      paste(formatted_states, collapse = "\n")
    },

    #' @description
    #' Filter call
    #'
    #' Builds \emph{subset expression} from condition calls stored in `FilterState`
    #' objects selection. The `lhs` of the expression is `private$dataname`.
    #' The `rhs` is a call to `self$get_fun()` with `private$dataname`
    #' as argument and a list of condition calls from `FilterState` objects
    #' stored in `private$state_list`.
    #' If no filters are applied, `NULL` is returned to avoid no-op calls such as `x <- x`.
    #'
    #' @param sid (`character`)\cr
    #'  when specified then method returns code containing filter conditions of
    #'  `FilterState` objects which `"sid"` attribute is different than this `sid` argument.
    #'
    #' @return `call` or `NULL`
    #'
    get_call = function(sid = "") {
      # state_list (list) names must be the same as argument of the function
      # for unnamed arguments state_list should have `arg = NULL`
      states_list <- private$state_list()
      args <- vapply(
        states_list,
        function(x) {
          arg <- x$get_state()$arg
          `if`(is.null(arg), "", arg) # converting NULL -> "" to enable tapply.
        },
        character(1)
      )

      filter_items <- tapply(
        X = states_list,
        INDEX = args,
        simplify = FALSE,
        function(items) {
          # removing empty filters and filters identified by sid
          nonempty_filter_idx <- vapply(items, function(x) x$is_any_filtered(), logical(1L))
          other_filter_idx <- !names(items) %in% sid
          filtered_items <- items[nonempty_filter_idx & other_filter_idx]

          calls <- lapply(
            filtered_items,
            function(state) {
              state$get_call(dataname = private$get_dataname_prefixed())
            }
          )
          calls_combine_by(calls, operator = "&")
        }
      )
      filter_items <- Filter(
        x = filter_items,
        f = Negate(is.null)
      )
      if (length(filter_items) > 0L) {
        filter_function <- str2lang(self$get_fun())
        data_name <- str2lang(private$get_dataname_prefixed())
        substitute(
          env = list(
            lhs = data_name,
            rhs = as.call(c(filter_function, c(list(data_name), filter_items)))
          ),
          expr = lhs <- rhs
        )
      } else {
        # return NULL to avoid no-op call
        NULL
      }
    },

    #' @description
    #' Prints this `FilterStates` object.
    #'
    #' @param ... additional arguments to this method
    print = function(...) {
      cat(shiny::isolate(self$format()), "\n")
    },

    #' @description
    #' Gets the name of the function used to filter the data in this `FilterStates`.
    #'
    #' Get name of function used to create the \emph{subset expression}.
    #' Defaults to "subset" but can be overridden by child class method.
    #'
    #' @return `character(1)` the name of the function
    #'
    get_fun = function() {
      "subset"
    },

    #' @description
    #' Gets the number of active `FilterState` objects in this `FilterStates` object.
    #'
    #' @return `integer(1)`
    #'
    get_filter_count = function() {
      length(self$get_filter_state())
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
      stop("Pure virtual method.")
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
      stop("Pure virtual method.")
    },

    #' @description
    #' Remove all `FilterState` objects from this `FilterStates` object.
    #'
    #' @return NULL
    #'
    clear_filter_states = function() {
      private$state_list_empty()
    },

    # shiny modules ----

    #' @description
    #' Shiny module UI
    #'
    #' Shiny UI element that stores `FilterState` UI elements.
    #' Populated with elements created with `renderUI` in the module server.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny element (module instance) id
    #'
    #' @return `shiny.tag`
    #'
    ui_active = function(id) {
      ns <- NS(id)
      private$cards_container_id <- ns("cards")
      tagList(
        include_css_files(pattern = "filter-panel"),
        tags$div(
          id = private$cards_container_id,
          class = "accordion",
          `data-label` = ifelse(private$datalabel == "", "", (paste0("> ", private$datalabel)))
        )
      )
    },

    #' @description
    #' Shiny module UI that adds a filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny element (module instance) id
    #'
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)
    #'   object which columns are used to choose filter variables.
    #' @return `shiny.tag`
    #'
    ui_add = function(id, data) {
      div("This object cannot be filtered")
    },

    #' @description
    #' Shiny module server that adds a filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns `NULL`
    #'
    srv_add = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          NULL
        }
      )
    }
  ),
  private = list(
    # private fields ----
    cards_container_id = character(0),
    card_ids = character(0),
    count_type = character(0), # specifies how observation numbers are displayed in filter cards,
    data = NULL, # data.frame, MAE, SE or matrix
    data_reactive = NULL, # reactive
    datalabel = character(0),
    dataname = NULL, # because it holds object of class name
    extract_type = character(0),
    filterable_varnames = character(0),
    ns = NULL, # shiny ns()
    observers = list(), # observers
    state_list = NULL, # list of `reactiveVal`s initialized by init methods of child classes,

    # private methods ----

    get_dataname_prefixed = function() {
      private$dataname
    },

    # Module to insert/remove `FilterState` UI
    #
    # This module adds the shiny UI of the `FilterState` object newly added
    # to state_list to the Active Filter Variables,
    # calls `FilterState` modules and creates an observer to remove state
    # parameter filter_state (`FilterState`).
    #
    # @param id (`character(1)`)\cr
    #   shiny module instance id
    # @param filter_state (`named list`)\cr
    #   should contain values of initial selections in the `FilterState`;
    #   `list` names must correspond to column names in `data`
    # @param state_id (`character(1)`)\cr
    #   name of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return `moduleServer` function which returns `NULL`
    #
    insert_filter_state_ui = function(id, filter_state, state_id) {
      checkmate::assert_multi_class(filter_state, c("FilterState", "FilterStateExpr"))
      checkmate::assert_character(state_id, len = 1)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            sprintf(
              "%s$insert_filter_state_ui, adding FilterState UI of state_id: %s; dataname: %s",
              class(self)[1],
              state_id,
              private$dataname
            )
          )

          # card_id of inserted card must be saved in private$card_ids as
          # it might be removed by the several events:
          #   - remove button in FilterStates module
          #   - remove button in FilteredDataset module
          #   - remove button in FilteredData module
          #   - API call remove_filter_state
          card_id <- session$ns("card")
          private$card_ids[state_id] <- card_id

          insertUI(
            selector = sprintf("#%s", private$cards_container_id),
            where = "beforeEnd",
            ui = filter_state$ui(card_id, private$cards_container_id)
          )
          # signal sent from filter_state when it is marked for removal
          remove_fs <- filter_state$server(id = "card")

          private$observers[[state_id]] <- observeEvent(
            ignoreInit = TRUE,
            ignoreNULL = TRUE,
            eventExpr = remove_fs(),
            handlerExpr = {
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1",
                "removing FilterState state_id: '{ state_id }',",
                "dataname: { private$dataname }"
              ))
              private$state_list_remove(state_id)
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1",
                "removed FilterState, state_id: '{ state_id }',",
                "dataname: { private$dataname }"
              ))
            }
          )

          logger::log_trace(
            sprintf(
              "%s$insert_filter_state_ui, added FilterState UI of state_id: %s; dataname: %s",
              class(self)[1],
              state_id,
              private$dataname
            )
          )
          NULL
        }
      )
    },

    # Remove shiny element. Method can be called from reactive session where
    # `observeEvent` for remove-filter-state is set and also from `FilteredDataset`
    # level, where shiny-session-namespace is different. That is why it's important
    # to remove shiny elements from anywhere. In `add_filter_state` `session$ns(NULL)`
    # is equivalent to `private$ns(state_id)`.
    # In addition, an unused reactive is being removed from input:
    # method searches input for the unique matches with the filter name
    # and then removes objects constructed with current card id + filter name.
    #
    remove_filter_state_ui = function(state_id, .input) {
      removeUI(selector = sprintf("#%s", private$card_ids[state_id]))
      private$card_ids <- private$card_ids[names(private$card_ids) != state_id]
      if (length(private$observers[[state_id]]) > 0) {
        private$observers[[state_id]]$destroy()
        private$observers[[state_id]] <- NULL
      }
      # Remove unused reactive from shiny input (leftover of removeUI).
      # This default behavior may change in the future, making this part obsolete.
      prefix <- paste0(gsub("cards$", "", private$cards_container_id))
      invisible(
        lapply(
          unique(grep(state_id, names(.input), value = TRUE)),
          function(i) {
            .subset2(.input, "impl")$.values$remove(paste0(prefix, i))
          }
        )
      )
    },

    # state_list methods ----

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
      checkmate::assert_multi_class(data, c("data.frame", "matrix", "DataFrame"))
      checkmate::assert_function(data_reactive, args = "sid")
      if (length(state) == 0L) {
        return(invisible(NULL))
      }

      states_id <- vapply(state, get_teal_slice_id, character(1))
      state_list <- shiny::isolate(private$state_list_get())

      lapply(seq_along(state), function(i) {
        state_id <- states_id[i]

        if (state_id[i] %in% names(state_list)) {
          # Modify existing filter states.
          do.call(state_list[[state_id]], state[[i]])

        } else if (inherits(state[[i]], "teal_slice_expr")) {
          # create a new FilterStateExpr
          fstate <- init_filter_state_expr(
            id = state[[i]]$id,
            title = state[[i]]$title,
            dataname = state[[i]]$dataname,
            expr = state[[i]]$expr,
            disabled = state[[i]]$disabled
          )
          private$state_list_push(x = fstate, state_id = state_id)

        } else {
          # create a new FilterState
          arg_list <- list(
            x = data[, state[[i]]$varname, drop = TRUE],
            # data_reactive is a function which eventually calls get_call(sid).
            # This chain of calls returns column from the data filtered by everything
            # but filter identified by the sid argument. FilterState then get x_reactive
            # and this no longer needs to be a function to pass sid. reactive in the FilterState
            # is also beneficial as it can be cached and retriger filter counts only if
            # returned vector is different.
            x_reactive = if (attr(state, "count_type") == "none") {
              reactive(NULL)
            } else {
              reactive(data_reactive(state_id)[, state[[i]]$varname, drop = TRUE])
            },
            extract_type = private$extract_type
          )
          arg_list <- append(arg_list, state[[i]])
          fstate <- do.call(init_filter_state, arg_list)
          private$state_list_push(x = fstate, state_id = state_id)
        }
      })

      invisible(NULL)
    }
  )
)
