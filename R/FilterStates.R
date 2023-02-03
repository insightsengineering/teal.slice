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
#'   input_dataname = "data",
#'   output_dataname = "data_filtered",
#'   varlabels = c(x = "x variable", SEX = "Sex"),
#'   datalabel = character(0),
#'   keys = character(0)
#' )
#' filter_state <- teal.slice:::RangeFilterState$new(
#'   c(NA, Inf, seq(1:10)),
#'   varname = "x",
#'   varlabel = "x variable",
#'   input_dataname = as.name("data"),
#'   extract_type = "list"
#' )
#' isolate(filter_state$set_selected(c(3L, 8L)))
#'
#' isolate(
#'   filter_states$state_list_push(
#'     x = filter_state,
#'     state_list_index = 1L,
#'     state_id = "x"
#'   ))
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
    #' `input_dataname`, `output_dataname`, and `datalabel`.
    #'
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'   the R object which `subset` function is applied on.
    #'
    #' @param data_reactive (`reactive`)\cr
    #'   should return an object constistent with the `FilterState` class.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters.
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on `rhs` of the expression
    #'   specified to the function argument attached to this `FilterStates`
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the `lhs` of the \emph{subset expression}
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value
    #'
    #' @return
    #' self invisibly
    #'
    initialize = function(data, data_reactive, input_dataname, output_dataname, datalabel) {
      checkmate::assert(
        checkmate::check_class(input_dataname, "call"),
        checkmate::check_class(input_dataname, "name"),
        checkmate::check_string(input_dataname)
      )
      checkmate::assert(
        checkmate::check_class(output_dataname, "call"),
        checkmate::check_class(output_dataname, "name"),
        checkmate::check_string(output_dataname)
      )
      checkmate::assert_character(datalabel, max.len = 1, any.missing = FALSE)

      char_to_name <- function(x) {
        if (is.character(x)) {
          as.name(x)
        } else {
          x
        }
      }

      private$input_dataname <- char_to_name(input_dataname)
      private$output_dataname <- char_to_name(output_dataname)
      private$datalabel <- datalabel
      private$data <- data
      private$data_reactive <- data_reactive

      logger::log_trace("Instantiated { class(self)[1] }, dataname: { deparse1(private$input_dataname) }")
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
    format = function(indent) {
      sprintf(paste(
        "%sThis is an instance of an abstract class.",
        "Use child class constructors to instantiate objects."),
        paste(rep(" ", indent), collapse = ""))
    },

    #' @description
    #' Filter call
    #'
    #' Builds \emph{subset expression} from condition calls stored in `FilterState`
    #' objects selection. The `lhs` of the expression is `private$output_dataname`.
    #' The `rhs` is a call to `self$get_fun()` with `private$input_dataname`
    #' as argument and a list of condition calls from `FilterState` objects
    #' stored in `private$state_list`.
    #' If `input_dataname` is the same as `output_dataname` and no filters are applied,
    #' `NULL` is returned to avoid no-op calls such as `x <- x`.
    #'
    #' @return `call` or `NULL`
    #'
    get_call = function() {
      # state_list (list) names must be the same as argument of the function
      # for ... list should be unnamed
      states_list <- private$state_list
      filter_items <- sapply(
        X = states_list,
        USE.NAMES = TRUE,
        simplify = FALSE,
        function(state_list) {
          items <- state_list()
          filtered_items <- Filter(f = function(x) x$is_any_filtered(), x = items)
          calls <- lapply(
            filtered_items,
            function(state) {
              state$get_call()
            }
          )
          if (length(calls) > 0) {
            calls_combine_by(
              operator = "&",
              calls = calls
            )
          }
        }
      )
      filter_items <- Filter(
        x = filter_items,
        f = Negate(is.null)
      )

      if (length(filter_items) > 0) {
        # below code translates to call by the names of filter_items
        rhs <- call_with_colon(
          self$get_fun(),
          private$input_dataname,
          unlist_args = filter_items
        )

        substitute(
          env = list(
            lhs = private$output_dataname,
            rhs = rhs
          ),
          expr = lhs <- rhs
        )
      } else if (!identical(private$output_dataname, private$input_dataname)) {
        substitute(
          env = list(
            lhs = private$output_dataname,
            rhs = private$input_dataname
          ),
          expr = lhs <- rhs
        )
      } else {
        # avoid no-op call
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

    # state_list methods ----

    #' @description
    #' Returns a list of `FilterState` objects stored in this `FilterStates`.
    #'
    #' @param state_list_index (`character(1)`, `integer(1)`)\cr
    #'   index on the list in `private$state_list` where filter states are kept
    #' @param state_id (`character(1)`)\cr
    #'   name of element in a filter state (which is a `reactiveVal` containing a list)
    #'
    #' @return `list` of `FilterState` objects
    #'
    state_list_get = function(state_list_index, state_id = NULL) {
      private$validate_state_list_exists(state_list_index)
      checkmate::assert_string(state_id, null.ok = TRUE)

      if (is.null(state_id)) {
        private$state_list[[state_list_index]]()
      } else {
        private$state_list[[state_list_index]]()[[state_id]]
      }
    },

    #' @description
    #' Adds a new `FilterState` object to this `FilterStates`.\cr
    #' Raises error if the length of `x` does not match the length of `state_id`.
    #'
    #' @param x (`FilterState`)\cr
    #'   object to be added to filter state list
    #' @param state_list_index (`character(1)`, `integer(1)`)\cr
    #'   index on the list in `private$state_list` where filter states are kept
    #' @param state_id (`character(1)`)\cr
    #'   name of element in a filter state (which is a `reactiveVal` containing a list)
    #'
    #' @return NULL
    #'
    state_list_push = function(x, state_list_index, state_id) {
      logger::log_trace(
        "{ class(self)[1] } pushing into state_list, dataname: { deparse1(private$input_dataname) }")
      private$validate_state_list_exists(state_list_index)
      checkmate::assert_string(state_id)

      states <- if (is.list(x)) {
        x
      } else {
        list(x)
      }

      state <- stats::setNames(states, state_id)
      new_state_list <- c(private$state_list[[state_list_index]](), state)
      private$state_list[[state_list_index]](new_state_list)

      logger::log_trace(
        "{ class(self)[1] } pushed into state_list, dataname: { deparse1(private$input_dataname) }")
      invisible(NULL)
    },

    #' @description
    #' Removes a single filter state with all associated shiny elements:\cr
    #' * specified `FilterState` from `private$state_list`
    #' * UI card created for this filter
    #' * observers tracking the selection and remove button
    #'
    #' @param state_list_index (`character(1)`, `integer(1)`)\cr
    #'   index on the list in `private$state_list` where filter states are kept
    #' @param state_id (`character(1)`)\cr
    #'   name of element in a filter state (which is a `reactiveVal` containing a list)
    #'
    #' @return NULL
    #'
    state_list_remove = function(state_list_index, state_id) {
      logger::log_trace(paste(
        "{ class(self)[1] } removing a filter from state_list { state_list_index },",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      private$validate_state_list_exists(state_list_index)
      checkmate::assert_string(state_id)
      checkmate::assert(
        checkmate::check_string(state_list_index),
        checkmate::check_int(state_list_index)
      )

      new_state_list <- private$state_list[[state_list_index]]()
      new_state_list[[state_id]] <- NULL
      private$state_list[[state_list_index]](new_state_list)

      logger::log_trace(paste(
        "{ class(self)[1] } removed from state_list { state_list_index },",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      invisible(NULL)
    },

    #' @description
    #' Remove all `FilterState` objects from this `FilterStates` object.
    #'
    #' @return NULL
    #'
    state_list_empty = function() {
      logger::log_trace(
        "{ class(self)[1] } emptying state_list, dataname: { deparse1(private$input_dataname) }")

      for (i in seq_along(private$state_list)) {
        private$state_list[[i]](list())
      }

      logger::log_trace(
        "{ class(self)[1] } emptied state_list, dataname: { deparse1(private$input_dataname) }")
      invisible(NULL)
    },

    #' @description
    #' Gets the number of active `FilterState` objects in this `FilterStates` object.
    #'
    #' @return `integer(1)`
    #'
    get_filter_count = function() {
      sum(vapply(private$state_list, function(state_list) {
        length(state_list())
      }, FUN.VALUE = integer(1)))
    },

    #' @description Remove a single `FilterState` from `state_list`.
    #'
    #' @param state_id (`character`)\cr
    #'   name of variable for which to remove `FilterState`
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(state_id) {
      stop("This variable can not be removed from the filter.")
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
    ui = function(id) {
      ns <- NS(id)
      datalabel <- self$get_datalabel()
      tags$div(
        class = "list-group hideable-list-group",
        `data-label` = ifelse(datalabel == "", "", datalabel), # todo: labels are not displayed for MAE - see filter-panel.css
        shiny::tagList(uiOutput(ns("filters")))
      )
    },

    #' @description
    #' Shiny server module.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns `NULL`
    #'
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(character(0))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$state_list_get(1L), {
            added_state_name(setdiff(names(self$state_list_get(1L)), names(previous_state())))
            previous_state(self$state_list_get(1L))
          })

          output[["filters"]] <- shiny::renderUI({
            fstates <- self$state_list_get(1L) # rerenders when queue changes / not when the state changes
            lapply(names(fstates), function(fname) {
              id <- sprintf("1L-%s", fname)
              private$ui_card_module(id = session$ns(id), fstates[[fname]])
            })
          })

          observeEvent(
            added_state_name(), # we want to call FilterState module only once when it's added
            ignoreNULL = TRUE,
            {
              fstates <- self$state_list_get(1L)
              lapply(added_state_name(), function(fname) {
                id <- sprintf("1L-%s", fname)
                private$srv_card_module(id = id, state_list_index = 1L, element_id = fname, fs = fstates[[fname]])
              })
              added_state_name(character(0))
            }
          )

          NULL
        }
      )
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
    #' Set the allowed filterable variables
    #' @param varnames (`character` or `NULL`) The variables which can be filtered
    #' See `self$get_filterable_varnames` for more details
    #'
    #' @details When retrieving the filtered variables only
    #' those which have filtering supported (i.e. are of the permitted types)
    #' are included.
    #'
    #' @return invisibly this `FilteredDataset`
    set_filterable_varnames = function(varnames) {
      return(invisible(self))
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
    ui_add_filter_state = function(id, data) {
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
    srv_add_filter_state = function(id) {
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
    data = NULL, # data.frame, MAE, SE or matrix
    data_reactive = NULL, # reactive
    datalabel = character(0),
    filterable_varnames = character(0),
    input_dataname = NULL, # because it holds object of class name
    output_dataname = NULL, # because it holds object of class name,
    ns = NULL, # shiny ns()
    observers = list(), # observers
    state_list = NULL, # list of `reactiveVal`s initialized by init methods of child classes

    # private methods ----

    #' UI wrapping a single `FilterState`
    #'
    #' This module contains a single `FilterState` card and remove (from the `ReactiveQueue`) button.
    #'
    #' return `moduleServer` function which returns `NULL`
    #' @keywords internal
    ui_card_module = function(id, fs) {
      ns <- NS(id)
      div(
        id = ns("card"),
        class = "list-group-item",
        fs$ui(id = ns("content"))
      )
    },

    #' Server module for a single `FilterState`
    #'
    #' Calls server from `FilterState` and observes remove (from the `ReactiveQueue`) button
    #' @keywords internal
    srv_card_module = function(id, state_list_index, element_id, fs) {
      moduleServer(id, function(input, output, session) {
        fs_callback <- fs$server(id = "content")
        observeEvent(
          eventExpr = fs_callback(), # when remove button is clicked in the FilterState ui
          ignoreInit = TRUE,
          ignoreNULL = TRUE, # observer should be triggered only if input$remove is true
          once = TRUE, # remove button can be called once, should be destroyed afterwards
          handlerExpr = {
            self$state_list_remove(state_list_index, element_id)
            # remove remainings: destroy fs observers, inputs etc.
          }
        )
      })
    },

    # Checks if the state_list of the given index was initialized in this `FilterStates`
    # @param state_list_index (character or integer)
    validate_state_list_exists = function(state_list_index) {
      checkmate::assert(
        checkmate::check_string(state_list_index),
        checkmate::check_int(state_list_index)
      )
      if (
        !(
          is.numeric(state_list_index) &&
          all(state_list_index <= length(private$state_list) && state_list_index > 0) ||
          is.character(state_list_index) && all(state_list_index %in% names(private$state_list))
        )
      ) {
        stop(
          paste(
            "Filter state list",
            state_list_index,
            "has not been initialized in FilterStates object belonging to the dataset",
            private$datalabel
          )
        )
      }
    }
  )
)
