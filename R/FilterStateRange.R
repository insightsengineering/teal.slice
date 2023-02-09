#' @name RangeFilterState
#' @title `FilterState` object for numeric variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::RangeFilterState$new(
#'   c(NA, Inf, seq(1:10)),
#'   varname = "x",
#'   input_dataname = as.name("data"),
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
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`numeric`)\cr
    #'   values of the variable used in filter
    #' @param x_reactive (`reactive`)\cr
    #'   a `reactive` returning a filtered vector or returning `NULL`. Is used to update
    #'   counts following the change in values of the filtered dataset. If the `reactive`
    #'   is `NULL` counts based on filtered dataset are not shown.
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
                          x_reactive,
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is.numeric(x))
      stopifnot(any(is.finite(x)))

      # validation on x_reactive here
      super$initialize(x, x_reactive, varname, varlabel, input_dataname, extract_type)
      var_range <- range(x, finite = TRUE)

      private$inf_filtered_count <- reactive(sum(is.infinite(x_reactive())))

      private$set_choices(var_range)
      self$set_selected(var_range)

      private$data_count <- length(x)

      private$inf_count <- sum(is.infinite(x))
      private$is_integer <- checkmate::test_integerish(x)
      private$keep_inf <- reactiveVal(FALSE)

      pretty_range_inputs <- private$get_pretty_range_inputs(x)

      private$unfiltered_histogram <- ggplot2::ggplot(data.frame(x = Filter(is.finite, x))) +
        ggplot2::geom_histogram(
          ggplot2::aes(x = x),
          bins = 100,
          fill = grDevices::rgb(211 / 255, 211 / 255, 211 / 255),
          color = grDevices::rgb(211 / 255, 211 / 255, 211 / 255)
        ) +
        ggplot2::theme_void() +
        ggplot2::coord_cartesian(
          expand = FALSE,
          xlim = c(pretty_range_inputs["min"], pretty_range_inputs["max"])
        )


      return(invisible(self))
    },

    #' @description
    #' Returns a formatted string representing this `LogicalFilterState`.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before after each new line character of the formatted string.
    #' Default: 0
    #' @return `character(1)` the formatted string
    #'
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      sprintf(
        "%sFiltering on: %s\n%1$s  Selected range: %s - %s\n%1$s  Include missing values: %s",
        format("", width = indent),
        self$get_varname(deparse = TRUE),
        format(self$get_selected(), nsmall = 3)[1],
        format(self$get_selected(), nsmall = 3)[2],
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
          deparse1(self$get_varname()),
          value,
          deparse1(private$input_dataname)
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
  private = list(
    unfiltered_histogram = NULL, # ggplot object
    data_count = 0, # number of values in unfiltered data - needed for scaling histogram
    keep_inf = NULL, # because it holds reactiveVal
    inf_count = integer(0),
    inf_filtered_count = NULL,
    is_integer = logical(0),

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

    # @description
    # formats range to pretty numbers to reduce decimal precision
    # @param values (numeric) initial range to be formatted
    # @return numeric(3) with names min, max, step - relevant for sliderInput
    get_pretty_range_inputs = function(values) {
      v_pretty_range <- pretty(values, n = 100)
      min <- min(v_pretty_range)
      max <- max(v_pretty_range)

      step <- private$get_pretty_range_step(min, max, v_pretty_range)

      c(
        min = min,
        max = max,
        step = step
      )
    },
    # @description gets pretty step size for range slider
    #  adaptation of shiny's method (see shiny/R/input-slider.R function findStepSize)
    # @param min (numeric) minimum of pretty values
    # @param max (numeric) maximum of pretty values
    # @param pretty_range (numeric(n)) vector of pretty values
    # @return numeric(1) pretty step size for the sliderInput
    get_pretty_range_step = function(min, max, pretty_range) {
      range <- max - min

      if (private$is_integer && range > 2) {
        return(1L)
      } else {
        n_steps <- length(pretty_range) - 1
        return(
          signif(digits = 10, (max - min) / n_steps)
        )
      }
    },
    validate_selection = function(value) {
      if (!is.numeric(value)) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a numeric",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }
      pre_msg <- sprintf(
        "data '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_range(value, private$choices, pre_msg = pre_msg)
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.numeric(values)
          if (any(is.na(values))) stop()
        },
        error = function(error) stop("The array of set values must contain values coercible to numeric.")
      )
      if (length(values) != 2) stop("The array of set values must have length two.")
      values
    },
    remove_out_of_bound_values = function(values) {
      if (values[1] < private$choices[1]) {
        warning(paste(
          "Value:", values[1], "is outside of the possible range for column", private$varname,
          "of dataset", private$input_dataname, "."
        ))
        values[1] <- private$choices[1]
      }

      if (values[2] > private$choices[2]) {
        warning(paste(
          "Value:", values[2], "is outside of the possible range for column", private$varname,
          "of dataset", private$input_dataname, "."
        ))
        values[2] <- private$choices[2]
      }
      values
    },
    get_inf_label = function() {
      sprintf(
        "Keep Inf (%s%s)",
        if (is.null(private$x_reactive())) "" else sprintf("%s/", private$inf_filtered_count()),
        private$inf_count
      )
    },

    # UI Module for `RangeFilterState`.
    # This UI element contains two values for `min` and `max`
    # of the range and two checkboxes whether to keep the `NA` or `Inf`  values.
    # @param id (`character(1)`)\cr
    #  id of shiny element
    ui_inputs = function(id) {
      ns <- NS(id)
      pretty_range_inputs <- private$get_pretty_range_inputs(private$choices)
      fluidRow(
        div(
          class = "filterPlotOverlayRange",
          plotOutput(ns("plot"), height = "100%"),
        ),
        div(
          class = "filterRangeSlider",
          teal.widgets::optionalSliderInput(
            inputId = ns("selection"),
            label = NULL,
            min = pretty_range_inputs["min"],
            max = pretty_range_inputs["max"],
            # on filter init without predefined value select "pretty" (wider) range
            value = isolate({
              if (identical(private$choices, self$get_selected())) {
                pretty_range_inputs[c("min", "max")]
              } else {
                self$get_selected()
              }
            }),
            width = "100%",
            step = pretty_range_inputs["step"]
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
          logger::log_trace("RangeFilterState$server initializing, dataname: { deparse1(private$input_dataname) }")

          output$plot <- renderPlot(
            bg = "transparent",
            height = 25,
            expr = {
              private$unfiltered_histogram +
                if (!is.null(private$x_reactive())) {
                  ggplot2::geom_histogram(
                    data = data.frame(x = Filter(is.finite, private$x_reactive())),
                    ggplot2::aes(x = x),
                    bins = 100,
                    fill = grDevices::rgb(173 / 255, 216 / 255, 230 / 255),
                    color = grDevices::rgb(173 / 255, 216 / 255, 230 / 255)
                  )
                } else {
                  NULL
                }
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
              logger::log_trace(
                sprintf(
                  "RangeFilterState$server@2 state of %s changed, dataname: %s",
                  deparse1(self$get_varname()),
                  deparse1(private$input_dataname)
                )
              )
              if (!setequal(self$get_selected(), input$selection)) {
                updateSliderInput(
                  session = session,
                  inputId = "selection",
                  value = private$selected()
                )
              }
            }
          )

          private$observers$selection <- observeEvent(
            ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in `selectInput`,
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = input$selection,
            handlerExpr = {
              # because we extended real range into rounded one we need to apply intersect(range_input, range_real)
              selection_state <- as.numeric(pmax(pmin(input$selection, private$choices[2]), private$choices[1]))
              if (!setequal(selection_state, self$get_selected())) {
                validate(
                  need(
                    input$selection[1] <= input$selection[2],
                    "Left range boundary should be lower than right"
                  )
                )
                self$set_selected(selection_state)
              }
              logger::log_trace(
                sprintf(
                  "RangeFilterState$server@3 selection of variable %s changed, dataname: %s",
                  deparse1(self$get_varname()),
                  deparse1(private$input_dataname)
                )
              )
            }
          )

          private$keep_inf_srv("keep_inf")
          private$keep_na_srv("keep_na")

          logger::log_trace("RangeFilterState$server initialized, dataname: { deparse1(private$input_dataname) }")
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
          isolate(private$get_inf_label()),
          value = isolate(self$get_keep_inf())
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
        observeEvent(private$inf_filtered_count(), {
          updateCheckboxInput(
            session,
            "value",
            label = private$get_inf_label(),
            value = self$get_keep_inf()
          )
        })

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
                label = private$get_inf_label(),
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
                deparse1(self$get_varname()),
                deparse1(input$value),
                deparse1(private$input_dataname)
              )
            )
          }
        )
        invisible(NULL)
      })
    },

    # @description
    # UI module to display filter summary
    # @param id `shiny` id parameter
    #  renders text describing selected range and
    #  if NA or Inf are included also
    ui_summary = function(id) {
      ns <- NS(id)
      uiOutput(ns("summary"), class = "filter-card-summary")
    },

    # @description
    # Server module to display filter summary
    # @param shiny `id` parametr passed to moduleServer
    #  renders text describing selected range and
    #  if NA or Inf are included also
    server_summary = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          output$summary <- renderUI({
            selected <- sprintf("%.4g", self$get_selected())
            min <- selected[1]
            max <- selected[2]
            tagList(
              tags$span(paste0(min, " - ", max)),
              if (self$get_keep_na()) tags$span("NA") else NULL,
              if (self$get_keep_inf()) tags$span("Inf") else NULL
            )
          })
        }
      )
    }
  )
)
