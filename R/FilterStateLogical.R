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
                          varname,
                          varlabel = character(0),
                          input_dataname = NULL,
                          extract_type = character(0)) {
      stopifnot(is.logical(x))
      super$initialize(x, varname, varlabel, input_dataname, extract_type)
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
    #' UI Module for `EmptyFilterState`.
    #' This UI element contains available choices selection and
    #' checkbox whether to keep or not keep the `NA` values.
    #' @param id (`character(1)`)\cr
    #'  id of shiny element
    ui = function(id) {
      ns <- NS(id)
      l_counts <- as.numeric(names(private$choices))
      is_na_l_counts <- is.na(l_counts)
      if (any(is_na_l_counts)) l_counts[is_na_l_counts] <- 0
      labels <- lapply(seq_along(private$choices), function(i) {
        l_count <- l_counts[i]
        l_freq <- l_count / sum(l_counts)
        if (is.na(l_freq) || is.nan(l_freq)) l_freq <- 0
        div(
          class = "choices_state_label",
          style = sprintf("width:%s%%", l_freq * 100),
          span(
            class = "choices_state_label_text",
            sprintf(
              "%s (%s)",
              private$choices[i],
              l_count
            )
          )
        )
      })

      div(
        div(
          class = "choices_state",
          radioButtons(
            ns("selection"),
            label = NULL,
            choiceNames = labels,
            choiceValues = as.character(private$choices),
            width = "100%"
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

    #' @description
    #' Server module
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("LogicalFilterState$server initializing, dataname: { deparse1(private$input_dataname) }")

          shiny::setBookmarkExclude(c("selection", "keep_na"))

          private$observers$selection_reactive <- observeEvent(
            private$selected_reactive(),
            ignoreNULL = TRUE,
            handlerExpr = {
              updateRadioButtons(
                session = session,
                inputId = "selection",
                selected =  private$selected_reactive()
              )
              logger::log_trace(sprintf(
                "LogicalFilterState$server@1 selection of variable %s changed, dataname: %s",
                deparse1(self$get_varname()),
                deparse1(private$input_dataname)
              ))
              private$selected_reactive(NULL)
            }
          )
          private$observe_keep_na_reactive(private$keep_na_reactive())

          private$observers$selection <- observeEvent(
            ignoreNULL = FALSE,
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

          private$observe_keep_na(input)

          logger::log_trace("LogicalFilterState$server initialized, dataname: { deparse1(private$input_dataname) }")
          NULL
        }
      )
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
    }
  )
)
