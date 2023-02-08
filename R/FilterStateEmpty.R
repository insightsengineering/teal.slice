#' @name EmptyFilterState
#'
#' @title `FilterState` object for empty variable
#'
#' @description
#' `FilterState` subclass representing an empty variable.
#'
#' @docType class
#'
#' @keywords internal
#'
#' @examples
#' filter_state <- teal.slice:::EmptyFilterState$new(
#'   NA,
#'   varname = "x",
#'   dataname = "data",
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected(TRUE))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
#'
EmptyFilterState <- R6::R6Class( # nolint
  "EmptyFilterState",
  inherit = FilterState,

  # public methods ----
  public = list(
    #' @description
    #' Initialize `EmptyFilterState` object.
    #'
    #' @param x (`vector`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param dataname (`character(1)`)\cr
    #'   optional name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #'   whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
    #' }
    #'
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          dataname = NULL,
                          extract_type = character(0)) {
      super$initialize(x, varname, varlabel, dataname, extract_type)
      private$set_choices(list())
      self$set_selected(list())

      return(invisible(self))
    },

    #' @description
    #' Reports whether the current state filters out any values.(?)
    #'
    #' @return `logical(1)`
    #'
    is_any_filtered = function() {
      !isTRUE(self$get_keep_na())
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Uses internal reactive values, hence must be called
    #' in reactive or isolated context.
    #'
    #' @return `logical(1)`
    #'
    get_call = function() {
      filter_call <- if (isTRUE(self$get_keep_na())) {
        call("is.na", private$get_varname_prefixed())
      } else {
        FALSE
      }
    },

    #' @description
    #' Returns the filtering state.
    #'
    #' @return `list` containing values taken from the reactive fields:
    #' * `keep_na` (`logical(1)`) whether `NA` should be kept.
    #'
    get_state = function() {
      list(
        keep_na = self$get_keep_na()
      )
    },

    #' @description
    #' Set state.
    #'
    #' @param state (`list`)\cr
    #'  contains fields relevant for specific class:
    #' \itemize{
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' }
    #'
    #' @return NULL invisibly
    set_state = function(state) {
      if (!is.null(state$selected)) {
        stop(
          sprintf(
            "All values in variable '%s' are `NA`. Unable to apply filter values \n  %s",
            self$get_varname(),
            paste(state$selected, collapse = ", ")
          )
        )
      }
      stopifnot(is.list(state) && all(names(state) == "keep_na"))
      if (!is.null(state$keep_na)) {
        self$set_keep_na(state$keep_na)
      }
      invisible(NULL)
    }
  ),

  # private members ----
  private = list(
    # @description
    # UI Module for `EmptyFilterState`.
    # This UI element contains a checkbox input to filter or keep missing values.
    #
    # @param id (`character(1)`)\cr
    #   shiny element (module instance) id
    #
    ui_inputs = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          class = "relative",
          div(
            span("Variable contains missing values only"),
            private$keep_na_ui(ns("keep_na"))
          )
        )
      )
    },

    # @description
    # Controls state of the `keep_na` checkbox input.
    #
    # @param id (`character(1)`)\cr
    #   shiny module instance id
    #
    # @return `moduleServer` function which returns `NULL`
    #
    server_inputs = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          private$keep_na_srv("keep_na")
        }
      )
    }
  )
)
