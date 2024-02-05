# RangeFilterState ------

#' @name RangeFilterState
#' @docType class
#'
#' @title `FilterState` object for numeric data
#'
#' @description Manages choosing a numeric range.
#'
#' @keywords internal
#'
RangeFilterState <- R6::R6Class( # nolint
  "RangeFilterState",
  inherit = FilterState,

  # public methods ----
  public = list(

    #' @description
    #' Initialize a `FilterState` object for range selection.
    #' @param x (`numeric`)
    #'   variable to be filtered.
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
    #' @return Object of class `RangeFilterState`, invisibly.
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          extract_type = character(0),
                          slice) {
      isolate({
        checkmate::assert_numeric(x, all.missing = FALSE)
        if (!any(is.finite(x))) stop("\"x\" contains no finite values")
        super$initialize(x = x, x_reactive = x_reactive, slice = slice, extract_type = extract_type)
        private$is_integer <- checkmate::test_integerish(x)
        private$inf_count <- sum(is.infinite(x))
        private$inf_filtered_count <- reactive(
          if (!is.null(private$x_reactive())) sum(is.infinite(private$x_reactive()))
        )

        checkmate::assert_numeric(slice$choices, null.ok = TRUE)
        if (is.null(slice$keep_inf) && any(is.infinite(x))) slice$keep_inf <- TRUE

        private$set_choices(slice$choices)
        if (is.null(slice$selected)) slice$selected <- slice$choices
        private$set_selected(slice$selected)

        private$is_integer <- checkmate::test_integerish(x)
        private$inf_filtered_count <- reactive(
          if (!is.null(private$x_reactive())) sum(is.infinite(private$x_reactive()))
        )
        private$inf_count <- sum(is.infinite(x))

        private$plot_data <- list(
          type = "histogram",
          nbinsx = 50,
          x = Filter(Negate(is.na), Filter(is.finite, private$x)),
          color = I(fetch_bs_color("secondary")),
          alpha = 0.2,
          bingroup = 1,
          showlegend = FALSE,
          hoverinfo = "none"
        )
        private$plot_mask <- list(list(
          type = "rect", fillcolor = rgb(1, 1, 1, .65), line = list(width = 0),
          x0 = -0.5, x1 = 1.5, y0 = -0.5, y1 = 1.5, xref = "paper", yref = "paper"
        ))
        private$plot_layout <- reactive({
          shapes <- private$get_shape_properties(private$get_selected())
          list(
            barmode = "overlay",
            xaxis = list(
              range = private$get_choices() * c(0.995, 1.005),
              rangeslider = list(thickness = 0),
              showticklabels = TRUE,
              ticks = "outside",
              ticklen = 1.5,
              tickmode = "auto",
              nticks = 10
            ),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE),
            margin = list(b = 17, l = 0, r = 0, t = 0, autoexpand = FALSE),
            plot_bgcolor = "#FFFFFF00",
            paper_bgcolor = "#FFFFFF00",
            shapes = shapes
          )
        })
        private$plot_config <- reactive({
          list(
            doubleClick = "reset",
            displayModeBar = FALSE,
            edits = list(shapePosition = TRUE)
          )
        })
        private$plot_filtered <- reactive({
          finite_values <- Filter(is.finite, private$x_reactive())
          if (!identical(finite_values, numeric(0))) {
            list(
              x = finite_values,
              bingroup = 1,
              color = I(fetch_bs_color("primary"))
            )
          }
        })
        invisible(self)
      })
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= <min value> & <varname> <= <max value>` with
    #' optional `is.na(<varname>)` and `is.finite(<varname>)`.
    #' @param dataname name of data set; defaults to `private$get_dataname()`
    #' @return `call`
    #'
    get_call = function(dataname) {
      if (isFALSE(private$is_any_filtered())) {
        return(NULL)
      }
      if (missing(dataname)) dataname <- private$get_dataname()
      varname <- private$get_varname_prefixed(dataname)
      filter_call <-
        call(
          "&",
          call(">=", varname, private$get_selected()[1L]),
          call("<=", varname, private$get_selected()[2L])
        )
      private$add_keep_na_call(private$add_keep_inf_call(filter_call, varname), varname)
    },

    #' @description
    #' Returns current `keep_inf` selection.
    #' @return `logical(1)`
    get_keep_inf = function() {
      private$teal_slice$keep_inf
    }
  ),

  # private fields----
  private = list(
    inf_count = integer(0),
    inf_filtered_count = NULL,
    is_integer = logical(0),
    numeric_step = numeric(0), # step for the slider input widget, calculated from input data (x)
    plot_data = NULL,
    plot_mask = list(),
    plot_layout = NULL,
    plot_config = NULL,
    plot_filtered = NULL,

    # private methods ----

    set_choices = function(choices) {
      x <- private$x[is.finite(private$x)]
      if (is.null(choices)) {
        choices <- range(x)
      } else {
        choices_adjusted <- c(max(choices[1L], min(x)), min(choices[2L], max(x)))
        if (any(choices != choices_adjusted)) {
          warning(sprintf(
            "Choices adjusted (some values outside of variable range). Varname: %s, dataname: %s.",
            private$get_varname(), private$get_dataname()
          ))
          choices <- choices_adjusted
        }
        if (choices[1L] > choices[2L]) {
          warning(sprintf(
            "Invalid choices: lower is higher / equal to upper, or not in range of variable values.
            Setting defaults. Varname: %s, dataname: %s.",
            private$get_varname(), private$get_dataname()
          ))
          choices <- range(x)
        }
      }

      private$set_is_choice_limited(private$x, choices)
      private$x <- private$x[
        (private$x >= choices[1L] & private$x <= choices[2L]) | is.na(private$x) | !is.finite(private$x)
      ]

      x_range <- range(private$x, finite = TRUE)

      # Required for displaying ticks on the slider, can modify choices!
      if (identical(diff(x_range), 0)) {
        choices <- x_range
      } else {
        x_pretty <- pretty(x_range, 100L)
        choices <- range(x_pretty)
        private$numeric_step <- signif(private$get_pretty_range_step(x_pretty), digits = 10)
      }
      private$teal_slice$choices <- choices
      invisible(NULL)
    },

    # @description
    # Check whether the initial choices filter out some values of x and set the flag in case.
    set_is_choice_limited = function(xl, choices) {
      xl <- xl[!is.na(xl)]
      xl <- xl[is.finite(xl)]
      private$is_choice_limited <- (any(xl < choices[1L]) | any(xl > choices[2L]))
      invisible(NULL)
    },

    # Adds is.infinite(varname) before existing condition calls if keep_inf is selected
    # returns a call
    add_keep_inf_call = function(filter_call, varname) {
      if (isTRUE(private$get_keep_inf())) {
        call("|", call("is.infinite", varname), filter_call)
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
        return(signif(digits = 10, (max(pretty_range) - min(pretty_range)) / n_steps))
      }
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.numeric(values)
          if (anyNA(values)) stop()
          values
        },
        error = function(e) stop("Vector of set values must contain values coercible to numeric")
      )
    },
    # Also validates that selection is sorted.
    check_length = function(values) {
      if (length(values) != 2L) stop("Vector of set values must have length two.")
      if (values[1L] > values[2L]) stop("Vector of set values must be sorted.")
      values
    },
    # Trim selection to limits imposed by private$get_choices()
    remove_out_of_bounds_values = function(values) {
      if (values[1L] < private$get_choices()[1L]) values[1L] <- private$get_choices()[1L]
      if (values[2L] > private$get_choices()[2L]) values[2L] <- private$get_choices()[2L]
      values
    },

    # Answers the question of whether the current settings and values selected actually filters out any values.
    # @return logical scalar
    is_any_filtered = function() {
      if (private$is_choice_limited) {
        TRUE
      } else if (!isTRUE(all.equal(private$get_selected(), private$get_choices()))) {
        TRUE
      } else if (!isTRUE(private$get_keep_inf()) && private$inf_count > 0) {
        TRUE
      } else if (!isTRUE(private$get_keep_na()) && private$na_count > 0) {
        TRUE
      } else {
        FALSE
      }
    },

    # obtain shape determination for histogram
    # returns a list that is passed to plotly's layout.shapes property
    get_shape_properties = function(values) {
      list(
        list(type = "line", x0 = values[1], x1 = values[1], y0 = -100, y1 = 100, yref = "paper"),
        list(type = "line", x0 = values[2], x1 = values[2], y0 = -100, y1 = 100, yref = "paper")
      )
    },

    # shiny modules ----

    # UI Module for `RangeFilterState`.
    # This UI element contains two values for `min` and `max`
    # of the range and two checkboxes whether to keep the `NA` or `Inf`  values.
    # @param id (`character(1)`) `shiny` module instance id.
    ui_inputs = function(id) {
      ns <- NS(id)
      isolate({
        ui_input <- shinyWidgets::numericRangeInput(
          inputId = ns("selection_manual"),
          label = NULL,
          min = private$get_choices()[1L],
          max = private$get_choices()[2L],
          value = private$get_selected(),
          step = private$numeric_step,
          width = "100%"
        )
        tagList(
          div(
            class = "choices_state",
            tags$head(tags$script(
              # Inline JS code for popover functionality.
              # Adding the script inline because when added from a file with include_js_files(),
              # it only works in the first info_button instance and not others.
              HTML(
                '$(document).ready(function() {
                  $("[data-toggle=\'popover\']").popover();

                  $(document).on("click", function (e) {
                    if (!$("[data-toggle=\'popover\']").is(e.target) &&
                        $("[data-toggle=\'popover\']").has(e.target).length === 0 &&
                        $(".popover").has(e.target).length === 0) {
                      $("[data-toggle=\'popover\']").popover("hide");
                    }
                  });
                });'
              )
            )),
            div(
              actionLink(
                ns("plotly_info"),
                label = NULL,
                icon = icon("question-circle"),
                "data-toggle" = "popover",
                "data-html" = "true",
                "data-placement" = "left",
                "data-trigger" = "click",
                "data-title" = "Plot actions",
                "data-content" = "<p>
                                  Drag vertical lines to set selection.<br>
                                  Drag across plot to zoom in.<br>
                                  Drag axis to pan.<br>
                                  Double click to zoom out."
              ),
              style = "text-align: right; font-size: 0.7em; margin-bottom: -1em; position: relative; z-index: 9;"
            ),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("plot"), height = "50px"),
              type = 4,
              size = 0.25,
              hide.ui = FALSE
            ),
            ui_input
          ),
          div(
            class = "filter-card-body-keep-na-inf",
            private$keep_inf_ui(ns("keep_inf")),
            private$keep_na_ui(ns("keep_na"))
          )
        )
      })
    },

    # @description
    # Server module
    # @param id (`character(1)`) `shiny` module instance id.
    # return `NULL`.
    server_inputs = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("RangeFilterState$server initializing, id: { private$get_id() }")

          # Capture manual input with debounce.
          selection_manual <- debounce(reactive(input$selection_manual), 200)

          # Prepare for histogram construction.
          plot_data <- c(private$plot_data, source = session$ns("histogram_plot"))

          # Display histogram, adding a second trace that contains filtered data.
          output$plot <- plotly::renderPlotly({
            histogram <- do.call(plotly::plot_ly, plot_data)
            histogram <- do.call(plotly::layout, c(list(p = histogram), private$plot_layout()))
            histogram <- do.call(plotly::config, c(list(p = histogram), private$plot_config()))
            histogram <- do.call(plotly::add_histogram, c(list(p = histogram), private$plot_filtered()))
            histogram
          })

          # Dragging shapes (lines) on plot updates selection.
          private$observers$relayout <-
            observeEvent(
              ignoreNULL = FALSE,
              ignoreInit = TRUE,
              eventExpr = plotly::event_data("plotly_relayout", source = session$ns("histogram_plot")),
              handlerExpr = {
                logger::log_trace("RangeFilterState$server@1 selection changed, id: { private$get_id() }")
                event <- plotly::event_data("plotly_relayout", source = session$ns("histogram_plot"))
                if (any(grepl("shapes", names(event)))) {
                  line_positions <- private$get_selected()
                  if (any(grepl("shapes[0]", names(event), fixed = TRUE))) {
                    line_positions[1] <- event[["shapes[0].x0"]]
                  } else if (any(grepl("shapes[1]", names(event), fixed = TRUE))) {
                    line_positions[2] <- event[["shapes[1].x0"]]
                  }
                  # If one line was dragged past the other, abort action and reset lines.
                  if (line_positions[1] > line_positions[2]) {
                    showNotification(
                      "Numeric range start value must be less than end value.",
                      type = "warning"
                    )
                    plotly::plotlyProxyInvoke(
                      plotly::plotlyProxy("plot"),
                      "relayout",
                      shapes = private$get_shape_properties(private$get_selected())
                    )
                    return(NULL)
                  }

                  private$set_selected(signif(line_positions, digits = 4L))
                }
              }
            )

          # Change in selection updates shapes (lines) on plot and numeric input.
          private$observers$selection_api <-
            observeEvent(
              ignoreNULL = FALSE,
              ignoreInit = TRUE,
              eventExpr = private$get_selected(),
              handlerExpr = {
                logger::log_trace("RangeFilterState$server@2 state changed, id: {private$get_id() }")
                if (!isTRUE(all.equal(private$get_selected(), selection_manual()))) {
                  shinyWidgets::updateNumericRangeInput(
                    session = session,
                    inputId = "selection_manual",
                    value = private$get_selected()
                  )
                }
              }
            )

          # Manual input updates selection.
          private$observers$selection_manual <- observeEvent(
            ignoreNULL = FALSE,
            ignoreInit = TRUE,
            eventExpr = selection_manual(),
            handlerExpr = {
              selection <- selection_manual()
              # Abort and reset if non-numeric values is entered.
              if (any(is.na(selection))) {
                showNotification(
                  "Numeric range values must be numbers.",
                  type = "warning"
                )
                shinyWidgets::updateNumericRangeInput(
                  session = session,
                  inputId = "selection_manual",
                  value = private$get_selected()
                )
                return(NULL)
              }

              # Abort and reset if reversed choices are specified.
              if (selection[1] > selection[2]) {
                showNotification(
                  "Numeric range start value must be less than end value.",
                  type = "warning"
                )
                shinyWidgets::updateNumericRangeInput(
                  session = session,
                  inputId = "selection_manual",
                  value = private$get_selected()
                )
                return(NULL)
              }


              if (!isTRUE(all.equal(selection, private$get_selected()))) {
                logger::log_trace("RangeFilterState$server@3 manual selection changed, id: { private$get_id() }")
                private$set_selected(selection)
              }
            }
          )

          private$keep_inf_srv("keep_inf")
          private$keep_na_srv("keep_na")

          logger::log_trace("RangeFilterState$server initialized, id: { private$get_id() }")
          NULL
        }
      )
    },
    server_inputs_fixed = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("RangeFilterState$server initializing, id: { private$get_id() }")

          plot_config <- private$plot_config()
          plot_config$staticPlot <- TRUE

          output$plot <- plotly::renderPlotly({
            histogram <- do.call(plotly::plot_ly, private$plot_data)
            histogram <- do.call(plotly::layout, c(list(p = histogram), private$plot_layout()))
            histogram <- do.call(plotly::config, c(list(p = histogram), plot_config))
            histogram <- do.call(plotly::add_histogram, c(list(p = histogram), private$plot_filtered()))
            histogram
          })

          output$selection <- renderUI({
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(session$ns("plot"), height = "50px"),
              type = 4,
              size = 0.25
            )
          })

          logger::log_trace("RangeFilterState$server initialized, id: { private$get_id() }")
          NULL
        }
      )
    },

    # @description
    # Server module to display filter summary
    #  renders text describing selected range and
    #  if NA or Inf are included also
    # @return `shiny.tag` to include in the `ui_summary`
    content_summary = function() {
      selection <- private$get_selected()
      tagList(
        tags$span(HTML(selection[1], "&ndash;", selection[2]), class = "filter-card-summary-value"),
        tags$span(
          class = "filter-card-summary-controls",
          if (private$na_count > 0) {
            tags$span("NA", if (isTRUE(private$get_keep_na())) icon("check") else icon("xmark"))
          },
          if (private$inf_count > 0) {
            tags$span("Inf", if (isTRUE(private$get_keep_inf())) icon("check") else icon("xmark"))
          }
        )
      )
    },

    # @description
    # Module displaying input to keep or remove NA in the `FilterState` call.
    # Renders a checkbox input only when variable with which the `FilterState` has been created contains Infs.
    # @param id (`character(1)`) `shiny` module instance id.
    keep_inf_ui = function(id) {
      ns <- NS(id)

      if (private$inf_count > 0) {
        countmax <- private$na_count
        countnow <- isolate(private$filtered_na_count())
        ui_input <- checkboxInput(
          inputId = ns("value"),
          label = tags$span(
            id = ns("count_label"),
            make_count_text(
              label = "Keep Inf",
              countmax = countmax,
              countnow = countnow
            )
          ),
          value = isolate(private$get_keep_inf())
        )
        div(
          uiOutput(ns("trigger_visible"), inline = TRUE),
          ui_input
        )
      } else {
        NULL
      }
    },

    # @description
    # Module to handle Inf values in the FilterState
    # Sets `private$slice$keep_inf` according to the selection
    # and updates the relevant UI element if `private$slice$keep_inf` has been changed by the api.
    # @param id (`character(1)`) `shiny` module instance id.
    # @return `NULL`.
    keep_inf_srv = function(id) {
      moduleServer(id, function(input, output, session) {
        # 1. renderUI is used here as an observer which triggers only if output is visible
        #  and if the reactive changes - reactive triggers only if the output is visible.
        # 2. We want to trigger change of the labels only if reactive count changes (not underlying data)
        output$trigger_visible <- renderUI({
          updateCountText(
            inputId = "count_label",
            label = "Keep Inf",
            countmax = private$inf_count,
            countnow = private$inf_filtered_count()
          )
          NULL
        })

        # this observer is needed in the situation when private$teal_slice$keep_inf has been
        # changed directly by the api - then it's needed to rerender UI element
        # to show relevant values
        private$observers$keep_inf_api <- observeEvent(
          ignoreNULL = TRUE, # its not possible for range that NULL is selected
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = private$get_keep_inf(),
          handlerExpr = {
            if (!setequal(private$get_keep_inf(), input$value)) {
              logger::log_trace("RangeFilterState$keep_inf_srv@1 changed reactive value, id: { private$get_id() }")
              updateCheckboxInput(
                inputId = "value",
                value = private$get_keep_inf()
              )
            }
          }
        )

        private$observers$keep_inf <- observeEvent(
          ignoreNULL = TRUE, # it's not possible for range that NULL is selected
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = input$value,
          handlerExpr = {
            logger::log_trace("FilterState$keep_na_srv@2 changed input, id: { private$get_id() }")
            keep_inf <- input$value
            private$set_keep_inf(keep_inf)
          }
        )

        invisible(NULL)
      })
    }
  )
)
