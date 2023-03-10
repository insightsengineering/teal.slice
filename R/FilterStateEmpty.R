#' @name EmptyFilterState
#' @title `FilterState` object for empty variable
#' @description `FilterState` subclass representing an empty variable
#' @docType class
#' @keywords internal
#'
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
    #' @param x_reactive (`reactive`)\cr
    #'   returning vector of the same type as `x`. Is used to update
    #'   counts following the change in values of the filtered dataset.
    #'   If it is set to `reactive(NULL)` then counts based on filtered
    #'   dataset are not shown.
    #' @param dataname (`character(1)`)\cr
    #'   optional name of dataset where `x` is taken from. Must be specified
    #'   if `extract_type` argument is not empty.
    #' @param varname (`character(1)`)\cr
    #'   name of the variable.
    #' @param choices (`vector`, unique(na.omit(x)))\cr
    #'   vector specifying allowed selection values
    #' @param selected (`atomic`, `NULL`)\cr
    #'   vector specifying selection
    #' @param varlabel (`character(0)`, `character(1)`)\cr
    #'   label of the variable (optional)
    #' @param keep_na (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep missing values
    #' @param keep_inf (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep infinite values
    #' @param fixed (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated fixed
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          dataname,
                          varname,
                          choices = unique(na.omit(x)),
                          selected = NULL,
                          varlabel = character(0),
                          keep_na = NULL,
                          keep_inf = NULL,
                          fixed = FALSE,
                          extract_type = character(0)) {

      checkmate::assert_class(x_reactive, 'reactive')
      super$initialize(
        x = x,
        x_reactive = x_reactive,
        dataname = dataname,
        varname = varname,
        choices = choices,
        selected = selected,
        varlabel = varlabel,
        keep_na = keep_na,
        keep_inf = keep_inf,
        fixed = fixed,
        extract_type = extract_type)

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
      if (private$is_disabled()) {
        FALSE
      } else {
        !isTRUE(self$get_keep_na())
      }
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
        substitute(!is.na(varname), list(varname = private$get_varname_prefixed()))
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
            private$varname,
            paste(state$selected, collapse = ", ")
          )
        )
      }
      stopifnot(is.list(state) && all(names(state) == "keep_na"))
      if (!is.null(state$keep_na) || private$is_disabled()) {
        self$set_keep_na(state$keep_na)
      }
      invisible(NULL)
    }
  ),

  # private members ----
  private = list(
    cache_state = function() {
      private$cache <- self$get_state()
      self$set_state(
        list(
          keep_na = NULL
        )
      )
    },
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

          observeEvent(private$is_disabled(), {
            shinyjs::toggleState(
              id = "keep_na-value",
              condition = !private$is_disabled()
            )
          })
        }
      )
    },

    # @description
    # Server module to display filter summary
    # Doesn't render anything
    content_summary = function(id) {
      tags$span("All empty")
    }
  )
)
