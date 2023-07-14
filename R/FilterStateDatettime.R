#' @rdname DatetimeFilterState
#' @title `FilterState` object for `POSIXct` variable
#' @description  Manages choosing a range of date-times
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::DatetimeFilterState$new(
#'   x = c(Sys.time() + seq(0, by = 3600, length.out = 10), NA),
#'   slice = teal_slice(varname = "x", dataname = "data"),
#'   extract_type = character(0)
#' )
#' shiny::isolate(filter_state$get_call())
#' filter_state$set_state(
#'   teal_slice(
#'     dataname = "data",
#'     varname = "x",
#'     selected = c(Sys.time() + 3L, Sys.time() + 8L),
#'     keep_na = TRUE
#'   )
#' )
#' shiny::isolate(filter_state$get_call())
#'
#' # working filter in an app
#' library(shiny)
#' library(shinyjs)
#'
#' datetimes <- as.POSIXct(c("2012-01-01 12:00:00", "2020-01-01 12:00:00"))
#' data_datetime <- c(seq(from = datetimes[1], to = datetimes[2], length.out = 100), NA)
#' fs <- teal.slice:::DatetimeFilterState$new(
#'   x = data_datetime,
#'   slice = teal_slice(
#'     varname = "x", dataname = "data", selected = data_datetime[c(47, 98)], keep_na = TRUE
#'   )
#' )
#'
#' ui <- fluidPage(
#'   useShinyjs(),
#'   teal.slice:::include_css_files(pattern = "filter-panel"),
#'   teal.slice:::include_js_files(pattern = "count-bar-labels"),
#'   column(4, div(
#'     h4("DatetimeFilterState"),
#'     fs$ui("fs")
#'   )),
#'   column(4, div(
#'     id = "outputs", # div id is needed for toggling the element
#'     h4("Condition (i.e. call)"), # display the subsetting call generated by this FilterState
#'     textOutput("condition_datetime"), br(),
#'     h4("Unformatted state"), # display raw filter state
#'     textOutput("unformatted_datetime"), br(),
#'     h4("Formatted state"), # display human readable filter state
#'     textOutput("formatted_datetime"), br()
#'   )),
#'   column(4, div(
#'     h4("Programmatic filter control"),
#'     actionButton("button1_datetime", "set drop NA", width = "100%"), br(),
#'     actionButton("button2_datetime", "set keep NA", width = "100%"), br(),
#'     actionButton("button3_datetime", "set a range", width = "100%"), br(),
#'     actionButton("button4_datetime", "set full range", width = "100%"), br(),
#'     actionButton("button0_datetime", "set initial state", width = "100%"), br()
#'   ))
#' )
#'
#' server <- function(input, output, session) {
#'   fs$server("fs")
#'   output$condition_datetime <- renderPrint(fs$get_call())
#'   output$formatted_datetime <- renderText(fs$format())
#'   output$unformatted_datetime <- renderPrint(fs$get_state())
#'   # modify filter state programmatically
#'   observeEvent(
#'     input$button1_datetime,
#'     fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = FALSE))
#'   )
#'   observeEvent(
#'     input$button2_datetime,
#'     fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = TRUE))
#'   )
#'   observeEvent(
#'     input$button3_datetime,
#'     fs$set_state(
#'       teal_slice(dataname = "data", varname = "x", selected = data_datetime[c(34, 56)])
#'     )
#'   )
#'   observeEvent(
#'     input$button4_datetime,
#'     fs$set_state(
#'       teal_slice(dataname = "data", varname = "x", selected = datetimes)
#'     )
#'   )
#'   observeEvent(
#'     input$button0_datetime,
#'     fs$set_state(
#'       teal_slice(
#'         dataname = "data", varname = "x", selected = data_datetime[c(47, 98)], keep_na = TRUE
#'       )
#'     )
#'   )
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
DatetimeFilterState <- R6::R6Class( # nolint
  "DatetimeFilterState",
  inherit = FilterState,

  # public methods ----

  public = list(

    #' @description
    #' Initialize a `FilterState` object. This class
    #' has an extra field, `private$timezone`, which is set to `Sys.timezone()` by
    #' default. However, in case when using this module in `teal` app, one needs
    #' timezone of the app user. App user timezone is taken from `session$userData$timezone`
    #' and is set only if object is initialized in `shiny`.
    #'
    #' @param x (`POSIXct` or `POSIXlt`)\cr
    #'   values of the variable used in filter
    #' @param x_reactive (`reactive`)\cr
    #'   returning vector of the same type as `x`. Is used to update
    #'   counts following the change in values of the filtered dataset.
    #'   If it is set to `reactive(NULL)` then counts based on filtered
    #'   dataset are not shown.
    #' @param slice (`teal_slice`)\cr
    #'   object created using [teal_slice()]. `teal_slice` is stored
    #'   in the class and `set_state` directly manipulates values within `teal_slice`. `get_state`
    #'   returns `teal_slice` object which can be reused in other places. Beware, that `teal_slice`
    #'   is a `reactiveValues` which means that changes in particular object are automatically
    #'   reflected in all places which refer to the same `teal_slice`.
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by `dataname`. Possible values:
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
        checkmate::assert_multi_class(x, c("POSIXct", "POSIXlt"))
        checkmate::assert_class(x_reactive, "reactive")

        super$initialize(
          x = x,
          x_reactive = x_reactive,
          slice = slice,
          extract_type = extract_type
        )
        checkmate::assert_multi_class(slice$choices, c("POSIXct", "POSIXlt"), null.ok = TRUE)
        private$set_choices(slice$choices)
        if (is.null(slice$selected)) slice$selected <- slice$choices
        private$set_selected(slice$selected)
      })

      invisible(self)
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= as.POSIXct(<min>) & <varname> <= <max>)`
    #' with optional `is.na(<varname>)`.
    #' @param dataname name of data set; defaults to `private$get_dataname()`
    #' @return (`call`)
    #'
    get_call = function(dataname) {
      if (isFALSE(private$is_any_filtered())) {
        return(NULL)
      }
      if (missing(dataname)) dataname <- private$get_dataname()
      choices <- private$get_selected()
      tzone <- Find(function(x) x != "", attr(as.POSIXlt(choices), "tzone"))
      class <- class(choices)[1L]
      date_fun <- as.name(
        switch(class,
          "POSIXct" = "as.POSIXct",
          "POSIXlt" = "as.POSIXlt"
        )
      )
      choices <- as.character(choices + c(0, 1))
      filter_call <-
        call(
          "&",
          call(
            ">=",
            private$get_varname_prefixed(dataname),
            as.call(list(date_fun, choices[1L], tz = tzone))
          ),
          call(
            "<",
            private$get_varname_prefixed(dataname),
            as.call(list(date_fun, choices[2L], tz = tzone))
          )
        )
      private$add_keep_na_call(filter_call, dataname)
    }
  ),

  # private members ----

  private = list(
    # private methods ----
    set_choices = function(choices) {
      if (is.null(choices)) {
        choices <- as.POSIXct(trunc(range(private$x, na.rm = TRUE), units = "secs"))
      } else {
        choices <- as.POSIXct(choices, units = "secs")
        choices_adjusted <- c(
          max(choices[1L], min(as.POSIXct(private$x), na.rm = TRUE)),
          min(choices[2L], max(as.POSIXct(private$x), na.rm = TRUE))
        )
        if (any(choices != choices_adjusted)) {
          warning(sprintf(
            "Choices adjusted (some values outside of variable range). Varname: %s, dataname: %s.",
            private$get_varname(), private$get_dataname()
          ))
          choices <- choices_adjusted
        }
        if (choices[1L] >= choices[2L]) {
          warning(sprintf(
            "Invalid choices: lower is higher / equal to upper, or not in range of variable values.
            Setting defaults. Varname: %s, dataname: %s.",
            private$get_varname(), private$get_dataname()
          ))
          choices <- range(private$x, na.rm = TRUE)
        }
      }

      private$set_is_choice_limited(private$x, choices)
      private$x <- private$x[
        (as.POSIXct(trunc(private$x, units = "secs")) >= choices[1L] &
          as.POSIXct(trunc(private$x, units = "secs")) <= choices[2L]) | is.na(private$x)
      ]
      private$teal_slice$choices <- choices
      invisible(NULL)
    },

    # @description
    # Check whether the initial choices filter out some values of x and set the flag in case.
    set_is_choice_limited = function(xl, choices = NULL) {
      private$is_choice_limited <- (any(xl < choices[1L], na.rm = TRUE) | any(xl > choices[2L], na.rm = TRUE))
      invisible(NULL)
    },
    validate_selection = function(value) {
      if (!(is(value, "POSIXct") || is(value, "POSIXlt"))) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a POSIXct or POSIXlt",
            private$get_varname(),
            private$get_dataname()
          )
        )
      }

      pre_msg <- sprintf(
        "dataset '%s', variable '%s': ",
        private$get_dataname(),
        private$get_varname()
      )
      check_in_range(value, private$get_choices(), pre_msg = pre_msg)
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.POSIXct(values, origin = "1970-01-01 00:00:00")
          if (any(is.na(values))) stop()
        },
        error = function(error) stop("The array of set values must contain values coercible to POSIX.")
      )
      if (length(values) != 2) stop("The array of set values must have length two.")
      values
    },
    remove_out_of_bound_values = function(values) {
      choices <- private$get_choices()
      if (values[1] < choices[1L] || values[1] > choices[2L]) {
        warning(
          sprintf(
            "Value: %s is outside of the range for the column '%s' in dataset '%s', setting minimum possible value.",
            values[1], private$get_varname(), toString(private$get_dataname())
          )
        )
        values[1] <- choices[1L]
      }

      if (values[2] > choices[2L] | values[2] < choices[1L]) {
        warning(
          sprintf(
            "Value: '%s' is outside of the range for the column '%s' in dataset '%s', setting maximum possible value.",
            values[2], private$get_varname(), toString(private$get_dataname())
          )
        )
        values[2] <- choices[2L]
      }

      if (values[1] > values[2]) {
        warning(
          sprintf(
            "Start date '%s' is set after the end date '%s', the values will be replaced by a default datetime range.",
            values[1], values[2]
          )
        )
        values <- c(choices[1L], choices[2L])
      }
      values
    },

    # shiny modules ----

    # @description
    # UI Module for `DatetimeFilterState`.
    # This UI element contains two date-time selections for `min` and `max`
    # of the range and a checkbox whether to keep the `NA` values.
    # @param id (`character(1)`)\cr
    #  id of shiny element
    ui_inputs = function(id) {
      ns <- NS(id)

      shiny::isolate({
        ui_input_1 <- shinyWidgets::airDatepickerInput(
          inputId = ns("selection_start"),
          value = private$get_selected()[1],
          startView = private$get_selected()[1],
          timepicker = TRUE,
          minDate = private$get_choices()[1L],
          maxDate = private$get_choices()[2L],
          update_on = "close",
          addon = "none",
          position = "bottom right"
        )
        ui_input_2 <- shinyWidgets::airDatepickerInput(
          inputId = ns("selection_end"),
          value = private$get_selected()[2],
          startView = private$get_selected()[2],
          timepicker = TRUE,
          minDate = private$get_choices()[1L],
          maxDate = private$get_choices()[2L],
          update_on = "close",
          addon = "none",
          position = "bottom right"
        )
        ui_reset_1 <- actionButton(
          class = "date_reset_button",
          inputId = ns("start_date_reset"),
          label = NULL,
          icon = icon("fas fa-undo")
        )
        ui_reset_2 <- actionButton(
          class = "date_reset_button",
          inputId = ns("end_date_reset"),
          label = NULL,
          icon = icon("fas fa-undo")
        )
        ui_input_1$children[[2]]$attribs <- c(ui_input_1$children[[2]]$attribs, list(class = "input-sm"))
        ui_input_2$children[[2]]$attribs <- c(ui_input_2$children[[2]]$attribs, list(class = "input-sm"))

        div(
          div(
            class = "flex",
            ui_reset_1,
            div(
              class = "flex w-80 filter_datelike_input",
              div(class = "w-45 text-center", ui_input_1),
              span(
                class = "input-group-addon w-10",
                span(class = "input-group-text w-100 justify-content-center", "to"),
                title = "Times are displayed in the local timezone and are converted to UTC in the analysis"
              ),
              div(class = "w-45 text-center", ui_input_2)
            ),
            ui_reset_2
          ),
          private$keep_na_ui(ns("keep_na"))
        )
      })
    },

    # @description
    # Server module
    # @param id (`character(1)`)\cr
    #   an ID string that corresponds with the ID used to call the module's UI function.
    # @return `moduleServer` function which returns `NULL`
    server_inputs = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("DatetimeFilterState$server initializing, id: { private$get_id() }")
          # this observer is needed in the situation when teal_slice$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          private$observers$selection_api <- observeEvent(
            ignoreNULL = TRUE, # dates needs to be selected
            ignoreInit = TRUE, # on init selected == default, so no need to trigger
            eventExpr = private$get_selected(),
            handlerExpr = {
              start_date <- input$selection_start
              end_date <- input$selection_end
              if (!all(private$get_selected() == c(start_date, end_date))) {
                logger::log_trace("DatetimeFilterState$server@1 state changed, id: { private$get_id() }")
                if (private$get_selected()[1] != start_date) {
                  shinyWidgets::updateAirDateInput(
                    session = session,
                    inputId = "selection_start",
                    value = private$get_selected()[1]
                  )
                }

                if (private$get_selected()[2] != end_date) {
                  shinyWidgets::updateAirDateInput(
                    session = session,
                    inputId = "selection_end",
                    value = private$get_selected()[2]
                  )
                }
              }
            }
          )


          private$observers$selection_start <- observeEvent(
            ignoreNULL = TRUE, # dates needs to be selected
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = input$selection_start,
            handlerExpr = {
              logger::log_trace("DatetimeFilterState$server@2 selection changed, id: { private$get_id() }")
              start_date <- input$selection_start
              end_date <- private$get_selected()[[2]]
              tzone <- Find(function(x) x != "", attr(as.POSIXlt(private$get_choices()), "tzone"))
              attr(start_date, "tzone") <- tzone

              if (start_date > end_date) {
                showNotification(
                  "Start date must not be greater than the end date. Ignoring selection.",
                  type = "warning"
                )
                shinyWidgets::updateAirDateInput(
                  session = session,
                  inputId = "selection_start",
                  value = private$get_selected()[1] # sets back to latest selected value
                )
                return(NULL)
              }

              private$set_selected(c(start_date, end_date))
            }
          )

          private$observers$selection_end <- observeEvent(
            ignoreNULL = TRUE, # dates needs to be selected
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = input$selection_end,
            handlerExpr = {
              start_date <- private$get_selected()[1]
              end_date <- input$selection_end
              tzone <- Find(function(x) x != "", attr(as.POSIXlt(private$get_choices()), "tzone"))
              attr(end_date, "tzone") <- tzone

              if (start_date > end_date) {
                showNotification(
                  "End date must not be lower than the start date. Ignoring selection.",
                  type = "warning"
                )
                shinyWidgets::updateAirDateInput(
                  session = session,
                  inputId = "selection_end",
                  value = private$get_selected()[2] # sets back to latest selected value
                )
                return(NULL)
              }

              private$set_selected(c(start_date, end_date))
              logger::log_trace("DatetimeFilterState$server@2 selection changed, id: { private$get_id() }")
            }
          )

          private$keep_na_srv("keep_na")

          private$observers$reset1 <- observeEvent(
            ignoreInit = TRUE, # reset button shouldn't be trigger on init
            ignoreNULL = TRUE, # it's impossible and wrong to set default to NULL
            input$start_date_reset,
            {
              shinyWidgets::updateAirDateInput(
                session = session,
                inputId = "selection_start",
                value = private$get_choices()[1L]
              )
              logger::log_trace("DatetimeFilterState$server@2 reset start date, id: { private$get_id() }")
            }
          )
          private$observers$reset2 <- observeEvent(
            ignoreInit = TRUE, # reset button shouldn't be trigger on init
            ignoreNULL = TRUE, # it's impossible and wrong to set default to NULL
            input$end_date_reset,
            {
              shinyWidgets::updateAirDateInput(
                session = session,
                inputId = "selection_end",
                value = private$get_choices()[2L]
              )
              logger::log_trace("DatetimeFilterState$server@3 reset end date, id: { private$get_id() }")
            }
          )

          logger::log_trace("DatetimeFilterState$server initialized, id: { private$get_id() }")
          NULL
        }
      )
    },
    server_inputs_fixed = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("DatetimeFilterState$server initializing, id: { private$get_id() }")

          output$selection <- renderUI({
            vals <- format(private$get_selected(), usetz = TRUE, nsmall = 3)
            div(
              div(icon("clock"), vals[1]),
              div(span(" - "), icon("clock"), vals[2])
            )
          })

          logger::log_trace("DatetimeFilterState$server initialized, id: { private$get_id() }")
          NULL
        }
      )
    },

    # @description
    # UI module to display filter summary
    #  renders text describing selected date range and
    #  if NA are included also
    content_summary = function(id) {
      selected <- format(private$get_selected(), "%Y-%m-%d %H:%M:%S")
      min <- selected[1]
      max <- selected[2]
      tagList(
        tags$span(
          class = "filter-card-summary-value",
          shiny::HTML(min, "&ndash;", max)
        ),
        tags$span(
          class = "filter-card-summary-controls",
          if (isTRUE(private$get_keep_na()) && private$na_count > 0) {
            tags$span(
              class = "filter-card-summary-na",
              "NA",
              shiny::icon("check")
            )
          } else if (isFALSE(private$get_keep_na()) && private$na_count > 0) {
            tags$span(
              class = "filter-card-summary-na",
              "NA",
              shiny::icon("xmark")
            )
          } else {
            NULL
          }
        )
      )
    }
  )
)