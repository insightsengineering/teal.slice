#' @rdname DatetimeFilterState
#' @title `FilterState` object for `POSIXct` variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::DatetimeFilterState$new(
#'   c(Sys.time() + seq(0, by = 3600, length.out = 10), NA),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#'
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected(c(Sys.time() + 3L, Sys.time() + 8L)))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
DatetimeFilterState <- R6::R6Class( # nolint
  "DatetimeFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object. This class
    #' has an extra field, `private$timezone`, which is set to `Sys.timezone()` by
    #' default. However, in case when using this module in `teal` app, one needs
    #' timezone of the app user. App user timezone is taken from `session$userData$timezone`
    #' and is set only if object is initialized in `shiny`.
    #' @param x (`POSIXct` or `POSIXlt`)\cr
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
      stopifnot(is(x, "POSIXct") || is(x, "POSIXlt"))
      super$initialize(x, varname, varlabel, input_dataname, extract_type)

      var_range <- range(x, na.rm = TRUE)
      private$set_choices(var_range)
      self$set_selected(var_range)

      if (shiny::isRunning()) {
        session <- getDefaultReactiveDomain()
        if (!is.null(session$userData$timezone)) {
          private$timezone <- session$userData$timezone
        }
      } else if (isTRUE(attr(x, "tz") != "")) {
        private$timezone <- attr(x, "tz")
      }

      return(invisible(self))
    },

    #' @description
    #' Returns a formatted string representing this `DatetimeFilterState`.
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
      } else if (!isTRUE(self$get_keep_na()) && private$na_count > 0) {
        TRUE
      } else {
        FALSE
      }
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> >= as.POSIXct(<min>, tz = <timezone>) & <varname> <= <max>, tz = <timezone>)`
    #' with optional `is.na(<varname>)`.
    get_call = function() {
      filter_call <- call_condition_range_posixct(
        varname = private$get_varname_prefixed(),
        range = self$get_selected(),
        timezone = private$timezone
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' Sets the selected time frame of this `DatetimeFilterState`.
    #'
    #' @param value (`POSIX(2)`) the lower and the upper bound of the selected
    #'   time frame. Must not contain NA values.
    #'
    #' @return invisibly `NULL`.
    #'
    #' @note Casts the passed object to `POSIXct` before validating the input
    #' making it possible to pass any object coercible to `POSIXct` to this method.
    #'
    #' @examples
    #' date <- as.POSIXct(1, origin = "01/01/1970")
    #' filter <- teal.slice:::DatetimeFilterState$new(
    #'   c(date, date + 1, date + 2, date + 3),
    #'   varname = "name"
    #' )
    #' filter$set_selected(c(date + 1, date + 2))
    set_selected = function(value) {
      super$set_selected(value)
    }
  ),
  private = list(
    timezone = Sys.timezone(),
    validate_selection = function(value) {
      if (!(is(value, "POSIXct") || is(value, "POSIXlt"))) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a POSIXct or POSIXlt",
            self$get_varname(deparse = TRUE),
            self$get_dataname(deparse = TRUE)
          )
        )
      }

      pre_msg <- sprintf(
        "dataset '%s', variable '%s': ",
        self$get_dataname(deparse = TRUE),
        self$get_varname(deparse = TRUE)
      )
      check_in_range(value, private$choices, pre_msg = pre_msg)
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.POSIXct(values)
          if (any(is.na(values))) stop()
        },
        error = function(error) stop("The array of set values must contain values coercible to POSIX.")
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

    # @description
    # UI Module for `DatetimeFilterState`.
    # This UI element contains two date-time selections for `min` and `max`
    # of the range and a checkbox whether to keep the `NA` values.
    # @param id (`character(1)`)\cr
    #  id of shiny element
    ui_inputs = function(id) {
      ns <- NS(id)
      div(
        div(
          class = "flex",
          actionButton(
            class = "date_reset_button",
            inputId = ns("start_date_reset"),
            label = NULL,
            icon = icon("fas fa-undo")
          ),
          div(
            class = "flex w-80 filter_datelike_input",
            div(class = "w-45 text-center", {
              x <- shinyWidgets::airDatepickerInput(
                inputId = ns("selection_start"),
                value = isolate(self$get_selected())[1],
                startView = isolate(self$get_selected())[1],
                timepicker = TRUE,
                minDate = private$choices[1],
                maxDate = private$choices[2],
                update_on = "close",
                addon = "none",
                position = "bottom right"
              )
              x$children[[2]]$attribs <- c(x$children[[2]]$attribs, list(class = "input-sm"))
              x
            }),
            span(
              class = "input-group-addon w-10",
              span(class = "input-group-text w-100 justify-content-center", "to"),
              title = "Times are displayed in the local timezone and are converted to UTC in the analysis"
            ),
            div(class = "w-45 text-center", {
              x <- shinyWidgets::airDatepickerInput(
                inputId = ns("selection_end"),
                value = isolate(self$get_selected())[2],
                startView = isolate(self$get_selected())[2],
                timepicker = TRUE,
                minDate = private$choices[1],
                maxDate = private$choices[2],
                update_on = "close",
                addon = "none",
                position = "bottom right"
              )
              x$children[[2]]$attribs <- c(x$children[[2]]$attribs, list(class = "input-sm"))
              x
            })
          ),
          actionButton(
            class = "date_reset_button",
            inputId = ns("end_date_reset"),
            label = NULL,
            icon = icon("fas fa-undo")
          )
        ),
        if (private$na_count > 0) {
          checkboxInput(
            ns("keep_na"),
            label_keep_na_count(private$na_count),
            value = isolate(self$get_keep_na())
          )
        } else {
          NULL
        }
      )
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
          logger::log_trace("DatetimeFilterState$server initializing, dataname: { deparse1(private$input_dataname) }")

          private$observers$selection <- observeEvent(
            ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in `selectInput`,
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = {
              input$selection_start
              input$selection_end
            },
            handlerExpr = {
              start_date <- input$selection_start
              end_date <- input$selection_end

              if (start_date < private$choices[1]) {
                start_date <- private$choices[1]
              }

              if (end_date > private$choices[2]) {
                end_date <- private$choices[2]
              }


              self$set_selected(c(start_date, end_date))
              logger::log_trace(sprintf(
                "DatetimeFilterState$server@2 selection of variable %s changed, dataname: %s",
                deparse1(self$get_varname()),
                deparse1(private$input_dataname)
              ))
            }
          )

          private$observe_keep_na(input)

          private$observers$reset1 <- observeEvent(input$start_date_reset, {
            shinyWidgets::updateAirDateInput(
              session = session,
              inputId = "selection_start",
              value = private$choices[1]
            )
            logger::log_trace(sprintf(
              "DatetimeFilterState$server@2 reset start date of variable %s, dataname: %s",
              deparse1(self$get_varname()),
              deparse1(private$input_dataname)
            ))
          })
          private$observers$reset2 <- observeEvent(input$end_date_reset, {
            shinyWidgets::updateAirDateInput(
              session = session,
              inputId = "selection_end",
              value = private$choices[2]
            )
            logger::log_trace(sprintf(
              "DatetimeFilterState$server@3 reset end date of variable %s, dataname: %s",
              deparse1(self$get_varname()),
              deparse1(private$input_dataname)
            ))
          })
          logger::log_trace("DatetimeFilterState$server initialized, dataname: { deparse1(private$input_dataname) }")
          NULL
        }
      )
    }
  )
)
