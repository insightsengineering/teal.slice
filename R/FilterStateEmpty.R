# EmptyFilterState ------

#' @name EmptyFilterState
#' @docType class
#'
#' @title `FilterState` object for empty variables
#'
#' @description `FilterState` subclass representing an empty variable.
#'
#' @keywords internal
#'
EmptyFilterState <- R6::R6Class( # nolint
  "EmptyFilterState",
  inherit = FilterState,

  # public methods ----
  public = list(

    #' @description
    #' Initialize `EmptyFilterState` object.
    #'
    #' @param x (`vector`)
    #'   variable to be filtered,
    #' @param x_reactive (`reactive`)
    #'   returning vector of the same type as `x`. Is used to update
    #'   counts following the change in values of the filtered dataset.
    #'   If it is set to `reactive(NULL)` then counts based on filtered
    #'   dataset are not shown.
    #' @param slice (`teal_slice`)
    #'   specification of this filter state.
    #'   `teal_slice` is stored in the object and `set_state` directly manipulates values within `teal_slice`.
    #'   `get_state` returns `teal_slice` object which can be reused in other places.
    #'   Note that `teal_slice` is a `reactiveValues`, which means it has reference semantics, i.e.
    #'   changes made to an object are automatically reflected in all places that refer to the same `teal_slice`.
    #' @param extract_type (`character`)
    #'   specifying whether condition calls should be prefixed by `dataname`. Possible values:
    #' - `character(0)` (default) `varname` in the condition call will not be prefixed
    #' - `"list"` `varname` in the condition call will be returned as `<dataname>$<varname>`
    #' - `"matrix"` `varname` in the condition call will be returned as `<dataname>[, <varname>]`
    #'
    #' @return Object of class `EmptyFilterState`, invisibly.
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          extract_type = character(0),
                          slice) {
      isolate({
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
    #' Returns reproducible condition call for current selection relevant for selected variable type.
    #' Uses internal reactive values, hence must be called in reactive or isolated context.
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
    # @param id (`character(1)`) `shiny` module instance id.
    #
    ui_inputs = function(id) {
      ns <- NS(id)
      isolate({
        div(
          tags$span("Variable contains missing values only"),
          private$keep_na_ui(ns("keep_na"))
        )
      })
    },

    # @description
    # Controls state of the `keep_na` checkbox input.
    #
    # @param id (`character(1)`) `shiny` module instance id.
    #
    # @return `NULL`.
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
            tags$span("Variable contains missing values only")
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
