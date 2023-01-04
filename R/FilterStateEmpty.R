#' @name EmptyFilterState
#' @title `FilterState` object for empty variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::EmptyFilterState$new(
#'   NA,
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected(TRUE))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
EmptyFilterState <- R6::R6Class( # nolint
  "EmptyFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize `EmptyFilterState` object
    #' @param x (`vector`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param input_dataname (`name` or `call`)\cr
    #'   name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      super$initialize(x, varname, varlabel, input_dataname, extract_type)
      private$set_choices(list())
      self$set_selected(list())

      return(invisible(self))
    },

    #' @description
    #' Answers the question of whether the current settings and values selected actually filters out any values.
    #' @return logical scalar
    is_any_filtered = function() {
      if (isTRUE(self$get_keep_na())) {
        FALSE
      } else {
        TRUE
      }
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
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
    get_state = function() {
      list(
        keep_na = self$get_keep_na()
      )
    },

    #' @description
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains checkbox input to
    #' filter or keep missing values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          class = "relative",
          div(
            span("Variable contains missing values only"),
            checkboxInput(
              ns("keep_na"),
              label_keep_na_count(private$na_count),
              value = FALSE
            )
          )
        )
      )
    },
    #' @description
    #' Controls selection of `keep_na` checkbox input
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          private$observe_keep_na(input)
          shiny::setBookmarkExclude("keep_na")
        }
      )
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' }
    set_state = function(state) {
      if (!is.null(state$selected)) {
        stop(
          sprintf(
            "All values in variable '%s' are `NA`. Unable to apply filter values \n  %s",
            self$get_varname(deparse = TRUE),
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
  )
)
