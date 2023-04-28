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
    #' @param choices (`atomic`, `NULL`)\cr
    #'   vector specifying allowed selection values
    #' @param selected (`atomic`, `NULL`)\cr
    #'   vector specifying selection
    #' @param keep_na (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep missing values
    #' @param keep_inf (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep infinite values
    #' @param fixed (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated fixed
    #' @param disabled (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated disabled
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
    #' }
    #' @param ... additional arguments to be saved as a list in `private$extras` field
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          dataname,
                          varname,
                          choices = NULL,
                          selected = NULL,
                          keep_na = NULL,
                          keep_inf = NULL,
                          fixed = FALSE,
                          disabled = FALSE,
                          extract_type = character(0),
                          ...) {
      args <- list(
        x = x,
        x_reactive = x_reactive,
        dataname = dataname,
        varname = varname,
        keep_na = keep_na,
        keep_inf = keep_inf,
        fixed = fixed,
        disabled = disabled,
        extract_type = extract_type
      )
      args <- append(args, list(...))
      do.call(super$initialize, args)

      private$set_choices(choices)
      private$set_selected(selected)

      invisible(self)
    },

    #' @description
    #' Reports whether the current state filters out any values.(?)
    #'
    #' @return `logical(1)`
    #'
    is_any_filtered = function() {
      if (private$is_disabled()) {
        FALSE
      } else if (private$is_choice_limited) {
        TRUE
      } else {
        !isTRUE(private$get_keep_na())
      }
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Uses internal reactive values, hence must be called
    #' in reactive or isolated context.
    #' @param dataname name of data set; defaults to `private$dataname`
    #' @return `logical(1)`
    #'
    get_call = function(dataname) {
      if (missing(dataname)) dataname <- private$dataname
      filter_call <- if (isTRUE(private$get_keep_na())) {
        call("is.na", private$get_varname_prefixed(dataname))
      } else {
        substitute(!is.na(varname), list(varname = private$get_varname_prefixed(dataname)))
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
        dataname = private$get_dataname(),
        varname = private$get_varname(),
        choices = private$choices,
        selected = private$get_selected(),
        keep_na = private$get_keep_na(),
        keep_inf = private$get_keep_inf(),
        fixed = private$fixed,
        disabled = private$is_disabled()
      )
    }
  ),

  # private members ----
  private = list(
    cache_state = function() {
      private$cache <- private$get_state()
      self$set_state(
        list(
          keep_na = NULL
        )
      )
    },

    set_choices = function(choices) {
      private$choices <- choices
      invisible(NULL)
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

    server_inputs_fixed = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          output$selection <- renderUI({
            div(
              class = "relative",
              div(
                span("Variable contains missing values only")
              )
            )
          })
          NULL
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
