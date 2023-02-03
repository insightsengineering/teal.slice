#' @name DateFilterState
#' @title `FilterState` object for Date variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::DateFilterState$new(
#'   c(Sys.Date() + seq(1:10), NA),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#'
#' isolate(filter_state$set_selected(c(Sys.Date() + 3L, Sys.Date() + 8L)))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
DateFilterState <- R6::R6Class( # nolint
  "DateFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`Date`)\cr
    #'   values of the variable used in filter
    #' @param x_reactive (`reactive`)\cr
    #'   a `reactive` returning a filtered vector. Is used to update
    #'   counts following the change in values of the filtered dataset.
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
      stopifnot(is(x, "Date"))

      #validation on x_reactive here
      super$initialize(x, x_reactive, varname, varlabel, input_dataname, extract_type)

      var_range <- range(x, na.rm = TRUE)
      private$set_choices(var_range)
      self$set_selected(var_range)

      return(invisible(self))
    },

    #' @description
    #' Returns a formatted string representing this `DateFilterState`.
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
        format(self$get_selected()[1], nsmall = 3),
        format(self$get_selected()[2], nsmall = 3),
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
    #' `<varname> >= <min value> & <varname> <= <max value>` with
    #' optional `is.na(<varname>)`.
    #' @return (`call`)
    get_call = function() {
      filter_call <- call_condition_range_date(
        varname = private$get_varname_prefixed(),
        range = self$get_selected()
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' Sets the selected time frame of this `DateFilterState`.
    #'
    #' @param value (`Date(2)`) the lower and the upper bound of the selected
    #'   time frame. Must not contain NA values.
    #'
    #' @return invisibly `NULL`.
    #'
    #' @note Casts the passed object to `Date` before validating the input
    #' making it possible to pass any object coercible to `Date` to this method.
    #'
    #' @examples
    #' date <- as.Date("13/09/2021")
    #' filter <- teal.slice:::DateFilterState$new(
    #'   c(date, date + 1, date + 2, date + 3),
    #'   varname = "name"
    #' )
    #' filter$set_selected(c(date + 1, date + 2))
    set_selected = function(value) {
      super$set_selected(value)
    },

    #' @description
    #' Shiny module server.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns reactive value
    #'   signaling that remove button has been clicked
    #'
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {

          observeEvent(self$get_keep_na(), {
            
            if (self$get_keep_na()) {
                class <- "fa fa-check"
              } else {
                class <- "fa fa-xmark"
              }

            output$header_keep_na <- renderUI({
              tagList(
                tags$span("NA "),
                tags$span(class = class)
              )
            })
          })

          observeEvent(self$get_selected(), {
            if (length(self$get_selected() > 1)) {
              value <- paste0(
                "(",
                paste(self$get_selected(), collapse = "-"),
                ")"
              )
            } else {
              value <- self$get_selected()
            }

            output$header_name_value <- renderUI({
              tagList(
                tags$span(self$get_varname(deparse = TRUE))#,
                #tags$span(value)
              )
            })
          })
          private$server_inputs("inputs")
          reactive(input$remove) # back to parent to remove self
        }
      )
    },

    #' @description
    #' Shiny module UI.
    #'
    #' @param id (`character(1)`)\cr
    #'  shiny element (module instance) id;
    #'  the UI for this class contains simple message stating that it is not supported
    #'
    ui = function(id) {
      ns <- NS(id)
      
      tags$li(
        id = id,
        tags$div(
        class = "filter-card",
        tags$div(
          class = "filter-card-header",
          uiOutput(ns("header_name_value"), inline = TRUE),
          uiOutput(ns("header_keep_na"), inline = TRUE),
          tags$div(
            class = "filter-card-icons",
            tags$span(
              class = "filter-card-toggle fa fa-chevron-right"
            ),
            actionLink(
              inputId = ns("remove"),
              label = icon("circle-xmark", lib = "font-awesome"),
              class = "filter-card-remove"
            )
          )
        ),
        tags$div(
          class = "filter-card-body",
          private$ui_inputs(ns("inputs"))
        )
        )
      )
    }
  ),
  private = list(
    validate_selection = function(value) {
      if (!is(value, "Date")) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a Date",
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
          values <- as.Date(values)
          if (any(is.na(values))) stop()
        },
        error = function(error) stop("The array of set values must contain values coercible to Date.")
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
    # UI Module for `DateFilterState`.
    # This UI element contains two date selections for `min` and `max`
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
            class = "w-80 filter_datelike_input",
            dateRangeInput(
              inputId = ns("selection"),
              label = NULL,
              start = self$get_selected()[1],
              end = self$get_selected()[2],
              min = private$choices[1],
              max = private$choices[2],
              width = "100%"
            )
          ),
          actionButton(
            class = "date_reset_button",
            inputId = ns("end_date_reset"),
            label = NULL,
            icon = icon("fas fa-undo")
          )
        ),
        private$keep_na_ui(ns("keep_na"))
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
          logger::log_trace("DateFilterState$server initializing, dataname: { deparse1(private$input_dataname) }")

          # this observer is needed in the situation when private$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          private$observers$seletion_api <- observeEvent(
            ignoreNULL = TRUE, # dates needs to be selected
            ignoreInit = TRUE,
            eventExpr = self$get_selected(),
            handlerExpr = {
              if (!setequal(self$get_selected(), input$selection)) {
                updateDateRangeInput(
                  session = session,
                  inputId = "selection",
                  start = self$get_selected()[1],
                  end = self$get_selected()[2]
                )
                logger::log_trace(sprintf(
                  "DateFilterState$server@1 selection of variable %s changed, dataname: %s",
                  deparse1(self$get_varname()),
                  deparse1(private$input_dataname)
                ))
              }
            }
          )

          private$observers$selection <- observeEvent(
            ignoreNULL = TRUE, # dates needs to be selected
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = input$selection,
            handlerExpr = {
              start_date <- input$selection[1]
              end_date <- input$selection[2]

              self$set_selected(c(start_date, end_date))
              logger::log_trace(sprintf(
                "DateFilterState$server@2 selection of variable %s changed, dataname: %s",
                deparse1(self$get_varname()),
                deparse1(private$input_dataname)
              ))
            }
          )


          private$keep_na_srv("keep_na")

          private$observers$reset1 <- observeEvent(input$start_date_reset, {
            updateDateRangeInput(
              session = session,
              inputId = "selection",
              start = private$choices[1]
            )
            logger::log_trace(sprintf(
              "DateFilterState$server@3 reset start date of variable %s, dataname: %s",
              deparse1(self$get_varname()),
              deparse1(private$input_dataname)
            ))
          })

          private$observers$reset2 <- observeEvent(input$end_date_reset, {
            updateDateRangeInput(
              session = session,
              inputId = "selection",
              end = private$choices[2]
            )
            logger::log_trace(sprintf(
              "DateFilterState$server@4 reset end date of variable %s, dataname: %s",
              deparse1(self$get_varname()),
              deparse1(private$input_dataname)
            ))
          })
          logger::log_trace("DateFilterState$server initialized, dataname: { deparse1(private$input_dataname) }")
          NULL
        }
      )
    }
  )
)
