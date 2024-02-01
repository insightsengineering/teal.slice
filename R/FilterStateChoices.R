# ChoicesFilterState ------

#' @name ChoicesFilterState
#' @docType class
#'
#' @title `FilterState` object for categorical data
#'
#' @description Manages choosing elements from a set.
#'
#' @keywords internal
#'
ChoicesFilterState <- R6::R6Class( # nolint
  "ChoicesFilterState",
  inherit = FilterState,

  # public methods ----

  public = list(

    #' @description
    #' Initialize a `FilterState` object.
    #'
    #' @param x (`character`)
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
    #' @return Object of class `ChoicesFilterState`, invisibly.
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          slice,
                          extract_type = character(0)) {
      isolate({
        checkmate::assert(
          is.character(x),
          is.factor(x),
          length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup"),
          combine = "or"
        )

        x_factor <- if (!is.factor(x)) {
          structure(
            factor(as.character(x), levels = as.character(sort(unique(x)))),
            label = attr(x, "label")
          )
        } else {
          x
        }

        super$initialize(
          x = x_factor,
          x_reactive = x_reactive,
          slice = slice,
          extract_type = extract_type
        )
        private$set_choices(slice$choices)
        if (is.null(slice$selected) && slice$multiple) {
          slice$selected <- private$get_choices()
        } else if (is.null(slice$selected)) {
          slice$selected <- private$get_choices()[1]
        } else if (length(slice$selected) > 1 && !slice$multiple) {
          warning(
            "ChoicesFilterState allows \"selected\" to be of length 1 when \"multiple\" is FALSE. ",
            "Only the first value will be used."
          )
          slice$selected <- slice$selected[1]
        }
        private$set_selected(slice$selected)
        private$data_class <- class(x)[1L]
        if (inherits(x, "POSIXt")) {
          private$tzone <- Find(function(x) x != "", attr(as.POSIXlt(x), "tzone"))
        }

        private$set_choices_counts(unname(table(x_factor)))
      })
      invisible(self)
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For this class returned call looks like
    #' `<varname> %in% c(<values selected>)` with optional `is.na(<varname>)`.
    #' @param dataname (`character(1)`) name of data set; defaults to `private$get_dataname()`
    #' @return `call` or `NULL`
    #'
    get_call = function(dataname) {
      if (isFALSE(private$is_any_filtered())) {
        return(NULL)
      }
      if (missing(dataname)) dataname <- private$get_dataname()
      varname <- private$get_varname_prefixed(dataname)
      selected <- private$get_selected()
      if (length(selected) == 0) {
        choices <- private$get_choices()
        fun_compare <- if (length(choices) == 1L) "==" else "%in%"
        filter_call <- call("!", call(fun_compare, varname, make_c_call(as.character(choices))))
      } else {
        if (setequal(na.omit(private$x), selected)) {
          filter_call <- NULL
        } else {
          fun_compare <- if (length(selected) == 1L) "==" else "%in%"

          if (private$data_class != "factor") {
            selected <- do.call(sprintf("as.%s", private$data_class), list(x = selected))
          }

          filter_call <-
            if (inherits(selected, "Date")) {
              call(fun_compare, varname, call("as.Date", make_c_call(as.character(selected))))
            } else if (inherits(selected, c("POSIXct", "POSIXlt"))) {
              class <- class(selected)[1L]
              date_fun <- as.name(
                switch(class,
                  "POSIXct" = "as.POSIXct",
                  "POSIXlt" = "as.POSIXlt"
                )
              )
              call(
                fun_compare,
                varname,
                as.call(list(date_fun, make_c_call(as.character(selected)), tz = private$tzone))
              )
            } else {
              # This handles numerics, characters, and factors.
              call(fun_compare, varname, make_c_call(selected))
            }
        }
      }
      private$add_keep_na_call(filter_call, varname)
    }
  ),

  # private members ----
  private = list(
    x = NULL,
    choices_counts = integer(0),
    data_class = character(0), # stores class of filtered variable so that it can be restored in $get_call
    tzone = character(0), # if x is a datetime, stores time zone so that it can be restored in $get_call

    # private methods ----

    # @description
    # Checks validity of the choices, adjust if neccessary and sets the flag for the case where choices
    #  are limited by default from the start.
    set_choices = function(choices) {
      if (is.null(choices)) {
        choices <- levels(private$x)
      } else {
        choices <- as.character(choices)
        choices_adjusted <- choices[choices %in% levels(private$x)]
        if (length(setdiff(choices, choices_adjusted)) > 0L) {
          warning(
            sprintf(
              "Some choices not found in data. Adjusting. Filter id: %s.",
              private$get_id()
            )
          )
          choices <- choices_adjusted
        }
        if (length(choices) == 0) {
          warning(
            sprintf(
              "None of the choices were found in data. Setting defaults. Filter id: %s.",
              private$get_id()
            )
          )
          choices <- levels(private$x)
        }
      }
      private$set_is_choice_limited(private$x, choices)
      private$teal_slice$choices <- choices
      private$x <- private$x[(private$x %in% private$get_choices()) | is.na(private$x)]
      private$x <- droplevels(private$x)
      invisible(NULL)
    },
    # @description
    # Check whether the initial choices filter out some values of x and set the flag in case.
    set_is_choice_limited = function(x, choices) {
      xl <- x[!is.na(x)]
      private$is_choice_limited <- length(setdiff(xl, choices)) > 0L
      invisible(NULL)
    },
    # @description
    # Sets choices_counts private field.
    set_choices_counts = function(choices_counts) {
      private$choices_counts <- choices_counts
      invisible(NULL)
    },
    # @description
    # Checks how many counts of each choice is present in the data.
    get_choices_counts = function() {
      if (!is.null(private$x_reactive)) {
        table(factor(private$x_reactive(), levels = private$get_choices()))
      } else {
        NULL
      }
    },
    # @description
    # Checks whether the input should be rendered as a checkboxgroup/radiobutton or a drop-down.
    is_checkboxgroup = function() {
      length(private$get_choices()) <= getOption("teal.threshold_slider_vs_checkboxgroup")
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.character(values)
          if (anyNA(values)) stop()
        },
        error = function(e) stop("The vector of set values must contain values coercible to character.")
      )
      values
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
    remove_out_of_bounds_values = function(values) {
      in_choices_mask <- values %in% private$get_choices()
      if (length(values[!in_choices_mask]) > 0) {
        warning(paste(
          "Values:", toString(values[!in_choices_mask], width = 360),
          "are not in choices of column", private$get_varname(), "in dataset", private$get_dataname(), "."
        ))
      }
      values[in_choices_mask]
    },

    # shiny modules ----

    # @description
    # UI Module for `ChoicesFilterState`.
    # This UI element contains available choices selection and
    # checkbox whether to keep or not keep the `NA` values.
    # @param id (`character(1)`) `shiny` module instance id.
    ui_inputs = function(id) {
      ns <- NS(id)

      # we need to isolate UI to not rettrigger renderUI
      isolate({
        countsmax <- private$choices_counts
        countsnow <- if (!is.null(private$x_reactive())) {
          unname(table(factor(private$x_reactive(), levels = private$get_choices())))
        }

        ui_input <- if (private$is_checkboxgroup()) {
          labels <- countBars(
            inputId = ns("labels"),
            choices = private$get_choices(),
            countsnow = countsnow,
            countsmax = countsmax
          )
          div(
            class = "choices_state",
            if (private$is_multiple()) {
              checkboxGroupInput(
                inputId = ns("selection"),
                label = NULL,
                selected = private$get_selected(),
                choiceNames = labels,
                choiceValues = private$get_choices(),
                width = "100%"
              )
            } else {
              radioButtons(
                inputId = ns("selection"),
                label = NULL,
                selected = private$get_selected(),
                choiceNames = labels,
                choiceValues = private$get_choices(),
                width = "100%"
              )
            }
          )
        } else {
          labels <- mapply(
            FUN = make_count_text,
            label = private$get_choices(),
            countnow = if (is.null(countsnow)) rep(list(NULL), length(private$get_choices())) else countsnow,
            countmax = countsmax
          )

          teal.widgets::optionalSelectInput(
            inputId = ns("selection"),
            choices = stats::setNames(private$get_choices(), labels),
            selected = private$get_selected(),
            multiple = private$is_multiple(),
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = (length(private$get_choices()) > 10),
              noneSelectedText = "Select a value"
            )
          )
        }
        div(
          uiOutput(ns("trigger_visible")),
          ui_input,
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
          logger::log_trace("ChoicesFilterState$server_inputs initializing, id: { private$get_id() }")

          # 1. renderUI is used here as an observer which triggers only if output is visible
          #  and if the reactive changes - reactive triggers only if the output is visible.
          # 2. We want to trigger change of the labels only if reactive count changes (not underlying data)
          non_missing_values <- reactive(Filter(Negate(is.na), private$x_reactive()))
          output$trigger_visible <- renderUI({
            logger::log_trace("ChoicesFilterState$server_inputs@1 updating count labels, id: { private$get_id() }")

            countsnow <- if (!is.null(private$x_reactive())) {
              unname(table(factor(non_missing_values(), levels = private$get_choices())))
            }

            # update should be based on a change of counts only
            isolate({
              if (private$is_checkboxgroup()) {
                updateCountBars(
                  inputId = "labels",
                  choices = private$get_choices(),
                  countsmax = private$choices_counts,
                  countsnow = countsnow
                )
              } else {
                labels <- mapply(
                  FUN = make_count_text,
                  label = private$get_choices(),
                  countnow = if (is.null(countsnow)) rep(list(NULL), length(private$get_choices())) else countsnow,
                  countmax = private$choices_counts
                )
                teal.widgets::updateOptionalSelectInput(
                  session = session,
                  inputId = "selection",
                  choices = stats::setNames(private$get_choices(), labels),
                  selected = private$get_selected()
                )
              }
              NULL
            })
          })

          if (private$is_checkboxgroup()) {
            private$observers$selection <- observeEvent(
              ignoreNULL = FALSE,
              ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
              eventExpr = input$selection,
              handlerExpr = {
                logger::log_trace("ChoicesFilterState$server_inputs@2 changed selection, id: { private$get_id() }")

                selection <- if (is.null(input$selection) && private$is_multiple()) {
                  character(0)
                } else {
                  input$selection
                }

                private$set_selected(selection)
              }
            )
          } else {
            private$observers$selection <- observeEvent(
              ignoreNULL = FALSE,
              ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
              eventExpr = input$selection_open, # observe click on a dropdown
              handlerExpr = {
                if (!isTRUE(input$selection_open)) { # only when the dropdown got closed
                  logger::log_trace("ChoicesFilterState$server_inputs@2 changed selection, id: { private$get_id() }")

                  selection <- if (is.null(input$selection) && private$is_multiple()) {
                    character(0)
                  } else if (isTRUE(length(input$selection) != 1) && !private$is_multiple()) {
                    # In optionalSelectInput user is able to select mutliple options. But if FilterState is not multiple
                    # we should prevent this selection to be processed further.
                    # This is why notification is thrown and dropdown is changed back to latest selected.
                    showNotification(paste(
                      "This filter exclusively supports single selection.",
                      "Any additional choices made will be disregarded."
                    ))
                    teal.widgets::updateOptionalSelectInput(
                      session, "selection",
                      selected = private$get_selected()
                    )
                    return(NULL)
                  } else {
                    input$selection
                  }
                  private$set_selected(selection)
                }
              }
            )
          }


          private$keep_na_srv("keep_na")

          # this observer is needed in the situation when teal_slice$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          private$observers$selection_api <- observeEvent(private$get_selected(), {
            # it's important to not retrigger when the input$selection is the same as reactive values
            # kept in the teal_slice$selected
            if (!setequal(input$selection, private$get_selected())) {
              logger::log_trace("ChoicesFilterState$server@1 state changed, id: { private$get_id() }")
              if (private$is_checkboxgroup()) {
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
              } else {
                teal.widgets::updateOptionalSelectInput(
                  session, "selection",
                  selected = private$get_selected()
                )
              }
            }
          })

          logger::log_trace("ChoicesFilterState$server_inputs initialized, id: { private$get_id() }")
          NULL
        }
      )
    },
    server_inputs_fixed = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("ChoicesFilterState$server_inputs_fixed initializing, id: { private$get_id() }")

          output$selection <- renderUI({
            countsnow <- if (!is.null(private$x_reactive())) {
              unname(table(factor(private$x_reactive(), levels = private$get_choices())))
            }
            countsmax <- private$choices_counts

            ind <- private$get_choices() %in% isolate(private$get_selected())
            countBars(
              inputId = session$ns("labels"),
              choices = isolate(private$get_selected()),
              countsnow = countsnow[ind],
              countsmax = countsmax[ind]
            )
          })

          logger::log_trace("ChoicesFilterState$server_inputs_fixed initialized, id: { private$get_id() }")
          NULL
        }
      )
    },

    # @description
    # UI module to display filter summary
    #  renders text describing number of selected levels
    #  and if NA are included also
    content_summary = function(id) {
      selected <- private$get_selected()
      selected_text <-
        if (length(selected) == 0L) {
          "no selection"
        } else {
          if (sum(nchar(selected)) <= 40L) {
            paste(selected, collapse = ", ")
          } else {
            paste(length(selected), "levels selected")
          }
        }
      tagList(
        tags$span(
          class = "filter-card-summary-value",
          selected_text
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
