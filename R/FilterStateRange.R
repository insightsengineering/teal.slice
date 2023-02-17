#' @name RangeFilterState
#' @title `FilterState` object for numeric variable
#' @description Manages choosing a numeric range
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::RangeFilterState$new(
#'   c(NA, Inf, seq(1:10)),
#'   varname = "x",
#'   dataname = "data",
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected(c(3L, 8L)))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$set_keep_inf(TRUE))
#' isolate(filter_state$get_call())
RangeFilterState <- R6::R6Class( # nolint
  "RangeFilterState",
  inherit = FilterState,

  # public methods ----
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`numeric`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`, `name`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param dataname (`character(1)`)\cr
    #'   optional name of dataset where `x` is taken from
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
    #' }
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          dataname = NULL,
                          extract_type = character(0)) {
      checkmate::assert_numeric(x, all.missing = FALSE)
      if (!any(is.finite(x))) stop("\"x\" contains no finite values")

      super$initialize(x, varname, varlabel, dataname, extract_type)
      private$inf_count <- sum(is.infinite(x))
      private$is_integer <- checkmate::test_integerish(x)
      private$keep_inf <- reactiveVal(FALSE)

      x_range <- range(x, finite = TRUE)
      x_pretty <- pretty(x_range, 100L)

      if (identical(diff(x_range), 0)) {
        private$set_choices(x_range)
        private$slider_ticks <- signif(x_range, digits = 10)
        private$slider_step <- NULL
        self$set_selected(x_range)
      } else {
        private$set_choices(range(x_pretty))
        private$slider_ticks <- signif(x_pretty, digits = 10)
        private$slider_step <- signif(private$get_pretty_range_step(x_pretty), digits = 10)
        self$set_selected(range(x_pretty))
      }

      private$histogram_data <- if (sum(is.finite(x)) >= 2) {
        as.data.frame(
          stats::density(x, na.rm = TRUE, n = 100)[c("x", "y")] # 100 bins only
        )
      } else {
        data.frame(x = NA_real_, y = NA_real_)
      }

      return(invisible(self))
    },

    #' @description
    #' Returns a formatted string representing this `RangeFilterState`.
    #'
    #' @param indent (`numeric(1)`)
    #'        the number of spaces before after each new line character of the formatted string.
    #'        Default: 0
    #' @return `character(1)` the formatted string
    #'
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      vals <- self$get_selected()
      sprintf(
        "%sFiltering on: %s\n%1$s  Selected range: %s - %s\n%1$s  Include missing values: %s",
        format("", width = indent),
        private$varname,
        format(vals[1], nsmall = 3),
        format(vals[2], nsmall = 3),
        format(self$get_keep_na())
      )
    },

    #' @description
    #' Answers the question of whether the current settings and values selected actually filters out any values.
    #' @return logical scalar
    is_any_filtered = function() {
      if (!setequal(self$get_selected(), private$choices)) {
        TRUE
      } else if (!isTRUE(self$get_keep_inf()) && private$inf_count > 0) {
        TRUE
      } else if (!isTRUE(self$get_keep_na()) && private$na_count > 0) {
        TRUE
      } else {
        FALSE
      }
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= <min value> & <varname> <= <max value>` with
    #' optional `is.na(<varname>)` and `is.finite(<varname>)`.
    #' @return (`call`)
    get_call = function() {
      filter_call <- call_condition_range(
        varname = private$get_varname_prefixed(),
        range = self$get_selected()
      )

      filter_call <- private$add_keep_inf_call(filter_call)
      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' Returns current `keep_inf` selection
    #' @return (`logical(1)`)
    get_keep_inf = function() {
      private$keep_inf()
    },

    #' @description
    #' Returns the filtering state.
    #'
    #' @return `list` containing values taken from the reactive fields:
    #' * `selected` (`numeric(2)`) range of the filter.
    #' * `keep_na` (`logical(1)`) whether `NA` should be kept.
    #' * `keep_inf` (`logical(1)`)  whether `Inf` should be kept.
    get_state = function() {
      list(
        selected = self$get_selected(),
        keep_na = self$get_keep_na(),
        keep_inf = self$get_keep_inf()
      )
    },

    #' @description
    #' Set if `Inf` should be kept
    #' @param value (`logical(1)`)\cr
    #'  Value(s) which come from the filter selection. Value is set in `server`
    #'  modules after selecting check-box-input in the shiny interface. Values are set to
    #'  `private$keep_inf` which is reactive.
    set_keep_inf = function(value) {
      checkmate::assert_flag(value)
      private$keep_inf(value)
      logger::log_trace(
        sprintf(
          "%s$set_keep_inf of variable %s set to %s, dataname: %s.",
          class(self)[1],
          private$varname,
          value,
          private$dataname
        )
      )
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`selected`}{ defines initial selection}
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' \item{`keep_inf` (`logical`)}{ defines whether to keep or remove `Inf` values}
    #' }
    set_state = function(state) {
      stopifnot(is.list(state) && all(names(state) %in% c("selected", "keep_na", "keep_inf")))
      if (!is.null(state$keep_inf)) {
        self$set_keep_inf(state$keep_inf)
      }
      super$set_state(state[names(state) %in% c("selected", "keep_na")])
      invisible(NULL)
    },

    #' @description
    #' Sets the selected values of this `RangeFilterState`.
    #'
    #' @param value (`numeric(2)`) the two-elements array of the lower and upper bound
    #'   of the selected range. Must not contain NA values.
    #'
    #' @returns invisibly `NULL`
    #'
    #' @note Casts the passed object to `numeric` before validating the input
    #' making it possible to pass any object coercible to `numeric` to this method.
    #'
    #' @examples
    #' filter <- teal.slice:::RangeFilterState$new(c(1, 2, 3, 4), varname = "name")
    #' filter$set_selected(c(2, 3))
    #'
    set_selected = function(value) {
      super$set_selected(value)
    }
  ),

  # private fields----
  private = list(
    histogram_data = data.frame(),
    keep_inf = NULL, # because it holds reactiveVal
    inf_count = integer(0),
    is_integer = logical(0),
    slider_step = numeric(0), # step for the slider input widget, calculated from input data (x)
    slider_ticks = numeric(0), # allowed values for the slider input widget, calculated from input data (x)

    # private methods ----
    # Adds is.infinite(varname) before existing condition calls if keep_inf is selected
    # returns a call
    add_keep_inf_call = function(filter_call) {
      if (isTRUE(self$get_keep_inf())) {
        call(
          "|",
          call("is.infinite", private$get_varname_prefixed()),
          filter_call
        )
      } else {
        filter_call
      }
    },

    # @description gets pretty step size for range slider
    #  adaptation of shiny's method (see shiny/R/input-slider.R function findStepSize)
    # @param pretty_range (numeric(n)) vector of pretty values
    # @return numeric(1) pretty step size for the sliderInput
    get_pretty_range_step = function(pretty_range) {
      if (private$is_integer && diff(range(pretty_range) > 2)) {
        return(1L)
      } else {
        n_steps <- length(pretty_range) - 1
        return(
          signif(digits = 10, (max(pretty_range) - min(pretty_range)) / n_steps)
        )
      }
    },

    # overwrites superclass method
    validate_selection = function(value) {
      if (!is.numeric(value)) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a numeric",
            self$get_varname(),
            self$get_dataname()
          )
        )
      }
      invisible(NULL)
    },

    # overwrites superclass method
    # additionally adjusts progtammatic selection to existing slider ticks
    cast_and_validate = function(values) {
      if (!is.atomic(values)) stop("Values to set must be an atomic vector.")
      values <- as.numeric(values)
      if (any(is.na(values))) stop("The array of set values must contain values coercible to numeric.")
      if (length(values) != 2) stop("The array of set values must have length two.")

      values_adjusted <- contain_interval(values, private$slider_ticks)
      if (!identical(values, values_adjusted)) {
        logger::log_warn(sprintf(
          paste(
            "Programmatic range specification on %s was adjusted to existing slider ticks.",
            "It is now broader in order to contain the specified values."
          ),
          private$varname
        ))
      }
      values_adjusted
    },
    # for numeric ranges selecting out of bound values is allowed
    remove_out_of_bound_values = function(values) {
      values
    },

    # shiny modules ----

    # UI Module for `RangeFilterState`.
    # This UI element contains two values for `min` and `max`
    # of the range and two checkboxes whether to keep the `NA` or `Inf`  values.
    # @param id (`character(1)`)\cr
    #  id of shiny element
    ui_inputs = function(id) {
      ns <- NS(id)
      fluidRow(
        div(
          class = "filterPlotOverlayRange",
          plotOutput(ns("plot"), height = "100%")
        ),
        div(
          class = "filterRangeSlider",
          teal.widgets::optionalSliderInput(
            inputId = ns("selection"),
            label = NULL,
            min = private$choices[1],
            max = private$choices[2],
            value = isolate(private$selected()),
            step = private$slider_step,
            width = "100%"
          )
        ),
        private$keep_inf_ui(ns("keep_inf")),
        private$keep_na_ui(ns("keep_na"))
      )
    },

    # @description
    # Server module
    # @param id (`character(1)`)\cr
    #   an ID string that corresponds with the ID used to call the module's UI function.
    # return `moduleServer` function which returns `NULL`
    server_inputs = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("RangeFilterState$server initializing, dataname: { private$dataname }")

          output$plot <- renderPlot(
            bg = "transparent",
            height = 25,
            expr = {
              ggplot2::ggplot(private$histogram_data) +
                ggplot2::aes_string(x = "x", y = "y") +
                ggplot2::geom_area(
                  fill = grDevices::rgb(66 / 255, 139 / 255, 202 / 255),
                  color = NA,
                  alpha = 0.2
                ) +
                ggplot2::theme_void() +
                ggplot2::scale_y_continuous(expand = c(0, 0)) +
                ggplot2::scale_x_continuous(expand = c(0, 0))
            }
          )

          # this observer is needed in the situation when private$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          private$observers$selection_api <- observeEvent(
            ignoreNULL = FALSE,
            ignoreInit = TRUE,
            eventExpr = self$get_selected(),
            handlerExpr = {
              if (!isTRUE(all.equal(input$selection, self$get_selected()))) {
                updateSliderInput(
                  session = session,
                  inputId = "selection",
                  value = private$selected()
                )
              }
            }
          )

          private$observers$selection <- observeEvent(
            ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in `selectInput`
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = input$selection,
            handlerExpr = {
              if (!isTRUE(all.equal(input$selection, self$get_selected()))) {
                self$set_selected(input$selection)
              }
              logger::log_trace(
                sprintf(
                  "RangeFilterState$server@3 selection of variable %s changed, dataname: %s",
                  private$varname,
                  private$dataname
                )
              )
            }
          )

          private$keep_inf_srv("keep_inf")
          private$keep_na_srv("keep_na")

          logger::log_trace("RangeFilterState$server initialized, dataname: { private$dataname }")
          NULL
        }
      )
    },

    # @description
    # module displaying input to keep or remove Inf in the FilterState call
    # @param id `shiny` id parameter
    #  renders checkbox input only when variable from which FilterState has
    #  been created has some Inf values.
    keep_inf_ui = function(id) {
      ns <- NS(id)
      if (private$inf_count > 0) {
        checkboxInput(
          ns("value"),
          sprintf("Keep Inf (%s)", private$inf_count),
          value = self$get_keep_inf()
        )
      } else {
        NULL
      }
    },

    # @description
    # module to handle Inf values in the FilterState
    # @param shiny `id` parametr passed to moduleServer
    #  module sets `private$keep_inf` according to the selection.
    #  Module also updates a UI element if the `private$keep_inf` has been
    #  changed through the api
    keep_inf_srv = function(id) {
      moduleServer(id, function(input, output, session) {
        # this observer is needed in the situation when private$keep_na has been
        # changed directly by the api - then it's needed to rerender UI element
        # to show relevant values
        private$observers$keep_inf_api <- observeEvent(
          ignoreNULL = TRUE, # its not possible for range that NULL is selected
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = self$get_keep_inf(),
          handlerExpr = {
            if (!setequal(self$get_keep_inf(), input$value)) {
              updateCheckboxInput(
                inputId = "value",
                value = self$get_keep_inf()
              )
            }
          }
        )
        private$observers$keep_inf <- observeEvent(
          ignoreNULL = TRUE, # it's not possible for range that NULL is selected
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = input$value,
          handlerExpr = {
            keep_inf <- input$value
            self$set_keep_inf(keep_inf)
            logger::log_trace(
              sprintf(
                "%s$server keep_inf of variable %s set to: %s, dataname: %s",
                class(self)[1],
                private$varname,
                deparse1(input$value),
                private$dataname
              )
            )
          }
        )
        invisible(NULL)
      })
    }
  )
)
