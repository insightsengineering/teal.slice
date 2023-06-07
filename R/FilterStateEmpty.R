#' @name EmptyFilterState
#' @title `FilterState` object for empty variable
#' @description `FilterState` subclass representing an empty variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::EmptyFilterState$new(
#'   x = NA,
#'   slice = filter_var(varname = "x", dataname = "data"),
#'   extract_type = character(0)
#' )
#' shiny::isolate(filter_state$get_call())
#' filter_state$set_state(filter_var(dataname = "data", varname = "x", keep_na = TRUE))
#' shiny::isolate(filter_state$get_call())
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
    #' @param slice (`teal_slice`)\cr
    #'   object created using [filter_var()]. `teal_slice` is stored
    #'   in the class and `set_state` directly manipulates values within `teal_slice`. `get_state`
    #'   returns `teal_slice` object which can be reused in other places. Beware, that `teal_slice`
    #'   is an immutable object which means that changes in particular object are automatically
    #'   reflected in all places which refer to the same `teal_slice`.
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
                          extract_type = character(0),
                          slice) {
      shiny::isolate({
        super$initialize(
          x = x,
          x_reactive = x_reactive,
          slice = slice,
          extract_type = extract_type
        )
        private$set_choices(slice$choices)
        private$set_selected(slice$selected)
      })

      invisible(self)
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Uses internal reactive values, hence must be called
    #' in reactive or isolated context.
    #' @param dataname name of data set; defaults to `private$get_dataname()`
    #' @return `logical(1)`
    #'
    get_call = function(dataname) {
      if (isFALSE(private$is_any_filtered())) {
        return(NULL)
      }
      if (missing(dataname)) dataname <- private$get_dataname()
      filter_call <- if (isTRUE(private$get_keep_na())) {
        call("is.na", private$get_varname_prefixed(dataname))
      } else {
        substitute(!is.na(varname), list(varname = private$get_varname_prefixed(dataname)))
      }
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
      private$teal_slice$choices <- choices
      invisible(NULL)
    },


    # Reports whether the current state filters out any values.(?)
    #
    # @return `logical(1)`
    #
    is_any_filtered = function() {
      if (private$is_choice_limited) {
        TRUE
      } else {
        !isTRUE(private$get_keep_na())
      }
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
      shiny::isolate({
        fluidRow(
          div(
            class = "relative",
            div(
              span("Variable contains missing values only"),
              private$keep_na_ui(ns("keep_na"))
            )
          )
        )
      })
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
