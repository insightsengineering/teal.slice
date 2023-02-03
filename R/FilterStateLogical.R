#' @name LogicalFilterState
#' @title `FilterState` object for logical variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::LogicalFilterState$new(
#'   sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
#'   varname = "x",
#'   input_dataname = as.name("data"),
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#'
#' isolate(filter_state$set_selected(TRUE))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
LogicalFilterState <- R6::R6Class( # nolint
  "LogicalFilterState",
  inherit = FilterState,
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`logical`)\cr
    #'   values of the variable used in filter
    #' @param x_reactive (`reactive`)\cr
    #'   a `reactive` returning a filtered vector. Is used to update
    #'   counts following the change in values of the filtered dataset.
    #' @param varname (`character`, `name`)\cr
    #'   label of the variable (optional).
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
      stopifnot(is.logical(x))
      super$initialize(x, x_reactive, varname, varlabel, input_dataname, extract_type)
      df <- as.factor(x)
      if (length(levels(df)) != 2) {
        if (levels(df) %in% c(TRUE, FALSE)) {
          choices_not_included <- c(TRUE, FALSE)[!c(TRUE, FALSE) %in% levels(df)]
          levels(df) <- c(levels(df), choices_not_included)
        }
      }

      tbl <- table(df)

      choices <- as.logical(names(tbl))
      names(choices) <- tbl
      private$set_choices(as.list(choices))
      self$set_selected(unname(choices)[1])
      private$histogram_data <- data.frame(
        x = sprintf(
          "%s (%s)",
          choices,
          names(choices)
        ),
        y = as.vector(tbl)
      )

      invisible(self)
    },

    #' @description
    #' Answers the question of whether the current settings and values selected actually filters out any values.
    #' @return logical scalar
    is_any_filtered = function() {
      if (!isTRUE(self$get_keep_na()) && private$na_count > 0) {
        TRUE
      } else if (all(private$histogram_data$y > 0)) {
        TRUE
      } else if (self$get_selected() == FALSE && "FALSE (0)" %in% private$histogram_data$x) {
        TRUE
      } else if (self$get_selected() == TRUE && "TRUE (0)" %in% private$histogram_data$x) {
        TRUE
      } else {
        FALSE
      }
    },

    #' @description
    #' Returns reproducible condition call for current selection.
    #' For `LogicalFilterState` it's a `!<varname>` or `<varname>` and optionally
    #' `is.na(<varname>)`
    get_call = function() {
      filter_call <- call_condition_logical(
        varname = private$get_varname_prefixed(),
        choice = self$get_selected()
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' Sets the selected values of this `LogicalFilterState`.
    #'
    #' @param value (`logical(1)`)\cr
    #'  the value to set. Must not contain the NA value.
    #'
    #' @returns invisibly `NULL`.
    #'
    #' @note Casts the passed object to `logical` before validating the input
    #' making it possible to pass any object coercible to `logical` to this method.
    #'
    #' @examples
    #' filter <- teal.slice:::LogicalFilterState$new(c(TRUE), varname = "name")
    #' filter$set_selected(TRUE)
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
    histogram_data = data.frame(),
    validate_selection = function(value) {
      if (!(checkmate::test_logical(value, max.len = 1, any.missing = FALSE))) {
        stop(
          sprintf(
            "value of the selection for `%s` in `%s` should be a logical scalar (TRUE or FALSE)",
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
      check_in_subset(value, private$choices, pre_msg = pre_msg)
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values_logical <- as.logical(values)
          if (any(is.na(values_logical))) stop()
        },
        error = function(cond) stop("The array of set values must contain values coercible to logical.")
      )
      values_logical
    },

    get_choice_labels = function() {
      l_counts <- as.numeric(names(private$choices))
      is_na_l_counts <- is.na(l_counts)
      if (any(is_na_l_counts)) l_counts[is_na_l_counts] <- 0
      f_counts <- unname(table(factor(private$x_reactive(), levels = private$choices)))
      f_counts[is.na(f_counts)] <- 0
      labels <- lapply(seq_along(private$choices), function(i) {
        l_count <- l_counts[i]
        f_count <- f_counts[i]
        l_freq <- l_count / sum(l_counts)
        f_freq <- f_count / sum(l_counts)

        if (is.na(l_freq) || is.nan(l_freq)) l_freq <- 0
        if (is.na(f_freq) || is.nan(f_freq)) f_freq <- 0
        tagList(
          div(
            class = "choices_state_label_unfiltered",
            style = sprintf("width:%s%%", l_freq * 100)
          ),
          div(
            class = "choices_state_label",
            style = sprintf("width:%s%%", f_freq * 100)
          ),
          div(
            class = "choices_state_label_text",
              sprintf("%s (%s/%s)", private$choices[i], f_count, l_count)
          )
        )
      })
    },

    # @description
    # UI Module for `EmptyFilterState`.
    # This UI element contains available choices selection and
    # checkbox whether to keep or not keep the `NA` values.
    # @param id (`character(1)`)\cr
    #  id of shiny element
    ui_inputs = function(id) {
      ns <- NS(id)
      div(
        div(
          class = "choices_state",
          radioButtons(
            ns("selection"),
            label = NULL,
            choiceNames = isolate(private$get_choice_labels()),
            choiceValues = as.character(private$choices),
            selected = isolate(as.character(self$get_selected())),
            width = "100%"
          )
        ),
        private$keep_na_ui(ns("keep_na"))
      )
    },

    # @description
    # Server module
    #
    # @param id (`character(1)`)\cr
    #   an ID string that corresponds with the ID used to call the module's UI function.
    # @return `moduleServer` function which returns `NULL`
    server_inputs = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          # this observer is needed in the situation when private$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          private$observers$seleted_api <- observeEvent(
            ignoreNULL = TRUE, # this is radio button so something have to be selected
            ignoreInit = TRUE,
            eventExpr = self$get_selected(),
            handlerExpr = {
              if (!setequal(self$get_selected(), input$selection)) {
                updateRadioButtons(
                  session = session,
                  inputId = "selection",
                  selected =  self$get_selected()
                )
                logger::log_trace(sprintf(
                  "LogicalFilterState$server@1 selection of variable %s changed, dataname: %s",
                  deparse1(self$get_varname()),
                  deparse1(private$input_dataname)
                ))
              }
            }
          )

          private$observers$selection <- observeEvent(
            ignoreNULL = TRUE, # in radio button something has to be selected to input$selection can't be NULL
            ignoreInit = TRUE,
            eventExpr = input$selection,
            handlerExpr = {
              selection_state <- as.logical(input$selection)
              if (is.null(selection_state)) {
                selection_state <- logical(0)
              }
              self$set_selected(selection_state)
              logger::log_trace(
                sprintf(
                  "LogicalFilterState$server@2 selection of variable %s changed, dataname: %s",
                  deparse1(self$get_varname()),
                  deparse1(private$input_dataname)
                )
              )
            }
          )

          private$keep_na_srv("keep_na")


          observeEvent(private$x_reactive(), {
            updateRadioButtons(
              inputId = "selection",
              choiceNames = private$get_choice_labels(),
              choiceValues = as.character(private$choices),
              selected = self$get_selected()
            )
          })

          logger::log_trace("LogicalFilterState$server initialized, dataname: { deparse1(private$input_dataname) }")
          NULL
        }
      )
    }
  )
)
