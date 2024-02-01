# LogicalFilterState ------

#' @name LogicalFilterState
#' @docType class
#'
#' @title `FilterState` object for logical data
#'
#' @description Manages choosing a logical state.
#'
#' @keywords internal
#'
LogicalFilterState <- R6::R6Class( # nolint
  "LogicalFilterState",
  inherit = FilterState,

  # public methods ----
  public = list(

    #' @description
    #' Initialize a `FilterState` object.
    #'
    #' @param x (`logical`)
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
    #' @return Object of class `LogicalFilterState`, invisibly.
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          extract_type = character(0),
                          slice) {
      isolate({
        checkmate::assert_logical(x)
        checkmate::assert_logical(slice$selected, null.ok = TRUE)
        super$initialize(x = x, x_reactive = x_reactive, slice = slice, extract_type = extract_type)

        private$set_choices(slice$choices)
        if (is.null(slice$multiple)) slice$multiple <- FALSE
        if (is.null(slice$selected) && slice$multiple) {
          slice$selected <- private$get_choices()
        } else if (length(slice$selected) != 1 && !slice$multiple) {
          slice$selected <- TRUE
        }
        private$set_selected(slice$selected)
        df <- factor(x, levels = c(TRUE, FALSE))
        tbl <- table(df)
        private$set_choices_counts(tbl)
      })
      invisible(self)
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For `LogicalFilterState` it's a `!<varname>` or `<varname>` and optionally `is.na(<varname>)`
    #' @param dataname name of data set; defaults to `private$get_dataname()`
    #' @return `call`
    #'
    get_call = function(dataname) {
      if (isFALSE(private$is_any_filtered())) {
        return(NULL)
      }
      if (missing(dataname)) dataname <- private$get_dataname()
      varname <- private$get_varname_prefixed(dataname)
      choices <- private$get_selected()
      n_choices <- length(choices)

      filter_call <-
        if (n_choices == 1 && choices) {
          varname
        } else if (n_choices == 1 && !choices) {
          call("!", varname)
        } else {
          call("%in%", varname, make_c_call(choices))
        }
      private$add_keep_na_call(filter_call, varname)
    }
  ),

  # private members ----
  private = list(
    choices_counts = integer(0),

    # private methods ----
    set_choices = function(choices) {
      private$teal_slice$choices <- c(TRUE, FALSE)
      invisible(NULL)
    },
    # @description
    # Sets choices_counts private field
    set_choices_counts = function(choices_counts) {
      private$choices_counts <- choices_counts
      invisible(NULL)
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.logical(values)
          if (anyNA(values)) stop()
          values
        },
        error = function(e) stop("Vector of set values must contain values coercible to logical.")
      )
    },
    # If multiple forbidden but selected, restores previous selection with warning.
    check_length = function(values) {
      if (!private$is_multiple() && length(values) > 1) {
        warning(
          sprintf("Selection: %s is not a vector of length one. ", toString(values, width = 360)),
          "Maintaining previous selection."
        )
        values <- isolate(private$get_selected())
      }
      values
    },

    # Answers the question of whether the current settings and values selected actually filters out any values.
    # @return logical scalar
    is_any_filtered = function() {
      if (private$is_choice_limited) {
        TRUE
      } else if (all(private$choices_counts > 0)) {
        TRUE
      } else if (
        setequal(private$get_selected(), private$get_choices()) &&
          !anyNA(private$get_selected(), private$get_choices())
      ) {
        TRUE
      } else if (!isTRUE(private$get_keep_na()) && private$na_count > 0) {
        TRUE
      } else {
        FALSE
      }
    },

    # shiny modules ----

    # @description
    # UI Module for `EmptyFilterState`.
    # This UI element contains available choices selection and
    # checkbox whether to keep or not keep the `NA` values.
    # @param id (`character(1)`) `shiny` module instance id.
    ui_inputs = function(id) {
      ns <- NS(id)
      isolate({
        countsmax <- private$choices_counts
        countsnow <- if (!is.null(private$x_reactive())) {
          unname(table(factor(private$x_reactive(), levels = private$get_choices())))
        } else {
          NULL
        }

        labels <- countBars(
          inputId = ns("labels"),
          choices = as.character(private$get_choices()),
          countsnow = countsnow,
          countsmax = countsmax
        )
        ui_input <- if (private$is_multiple()) {
          checkboxGroupInput(
            inputId = ns("selection"),
            label = NULL,
            selected = isolate(as.character(private$get_selected())),
            choiceNames = labels,
            choiceValues = factor(as.character(private$get_choices()), levels = c("TRUE", "FALSE")),
            width = "100%"
          )
        } else {
          radioButtons(
            inputId = ns("selection"),
            label = NULL,
            selected = isolate(as.character(private$get_selected())),
            choiceNames = labels,
            choiceValues = factor(as.character(private$get_choices()), levels = c("TRUE", "FALSE")),
            width = "100%"
          )
        }
        div(
          div(
            class = "choices_state",
            uiOutput(ns("trigger_visible"), inline = TRUE),
            ui_input
          ),
          private$keep_na_ui(ns("keep_na"))
        )
      })
    },

    # @description
    # Server module
    # @param id (`character(1)`) `shiny` module instance id.
    # @return `NULL`.
    server_inputs = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          # this observer is needed in the situation when teal_slice$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          non_missing_values <- reactive(Filter(Negate(is.na), private$x_reactive()))
          output$trigger_visible <- renderUI({
            logger::log_trace("LogicalFilterState$server@1 updating count labels, id: { private$get_id() }")

            countsnow <- if (!is.null(private$x_reactive())) {
              unname(table(factor(non_missing_values(), levels = private$get_choices())))
            } else {
              NULL
            }

            updateCountBars(
              inputId = "labels",
              choices = as.character(private$get_choices()),
              countsmax = private$choices_counts,
              countsnow = countsnow
            )
            NULL
          })

          private$observers$seleted_api <- observeEvent(
            ignoreNULL = !private$is_multiple(),
            ignoreInit = TRUE,
            eventExpr = private$get_selected(),
            handlerExpr = {
              if (!setequal(private$get_selected(), input$selection)) {
                logger::log_trace("LogicalFilterState$server@1 state changed, id: { private$get_id() }")
                if (private$is_multiple()) {
                  updateCheckboxGroupInput(
                    inputId = "selection",
                    selected = private$get_selected()
                  )
                } else {
                  updateRadioButtons(
                    inputId = "selection",
                    selected = private$get_selected()
                  )
                }
              }
            }
          )

          private$observers$selection <- observeEvent(
            ignoreNULL = FALSE,
            ignoreInit = TRUE,
            eventExpr = input$selection,
            handlerExpr = {
              logger::log_trace("LogicalFilterState$server@2 selection changed, id: { private$get_id() }")
              # for private$is_multiple() == TRUE input$selection will always have value
              if (is.null(input$selection) && isFALSE(private$is_multiple())) {
                selection_state <- private$get_selected()
              } else {
                selection_state <- as.logical(input$selection)
              }

              if (is.null(selection_state)) {
                selection_state <- logical(0)
              }
              private$set_selected(selection_state)
            }
          )

          private$keep_na_srv("keep_na")

          logger::log_trace("LogicalFilterState$server initialized, id: { private$get_id() }")
          NULL
        }
      )
    },
    server_inputs_fixed = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("LogicalFilterState$server initializing, id: { private$get_id() }")

          output$selection <- renderUI({
            countsnow <- unname(table(factor(private$x_reactive(), levels = private$get_choices())))
            countsmax <- private$choices_counts

            ind <- private$get_choices() %in% private$get_selected()
            countBars(
              inputId = session$ns("labels"),
              choices = private$get_selected(),
              countsnow = countsnow[ind],
              countsmax = countsmax[ind]
            )
          })

          logger::log_trace("LogicalFilterState$server initialized, id: { private$get_id() }")
          NULL
        }
      )
    },

    # @description
    # Server module to display filter summary
    #  renders text describing whether TRUE or FALSE is selected
    #  and if NA are included also
    content_summary = function(id) {
      tagList(
        tags$span(
          class = "filter-card-summary-value",
          toString(private$get_selected())
        ),
        tags$span(
          class = "filter-card-summary-controls",
          if (private$na_count > 0) {
            tags$span("NA", if (isTRUE(private$get_keep_na())) icon("check") else icon("xmark"))
          }
        )
      )
    }
  )
)
