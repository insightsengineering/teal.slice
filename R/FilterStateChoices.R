#' @name ChoicesFilterState
#' @title `FilterState` object for factor or character variable
#' @docType class
#' @keywords internal
#'
#'
#' @examples
#' filter_state <- teal.slice:::ChoicesFilterState$new(
#'   c(LETTERS, NA),
#'   varname = "x",
#'   dataname = "data",
#'   extract_type = character(0)
#' )
#' isolate(filter_state$get_call())
#' isolate(filter_state$set_selected("B"))
#' isolate(filter_state$set_keep_na(TRUE))
#' isolate(filter_state$get_call())
ChoicesFilterState <- R6::R6Class( # nolint
  "ChoicesFilterState",
  inherit = FilterState,

  # public methods ----

  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`character` or `factor`)\cr
    #'   values of the variable used in filter
    #' @param x_reactive (`reactive`)\cr
    #'   a `reactive` returning a filtered vector or returning `NULL`. Is used to update
    #'   counts following the change in values of the filtered dataset. If the `reactive`
    #'   is `NULL` counts based on filtered dataset are not shown.
    #' @param varname (`character`)\cr
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
                          x_reactive,
                          varname,
                          varlabel = character(0),
                          dataname = NULL,
                          extract_type = character(0)) {
      checkmate::assert(
        is.character(x),
        is.factor(x),
        length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup"),
        combine = "or"
      )

      # validation on x_reactive here
      super$initialize(x, x_reactive, varname, varlabel, dataname, extract_type)
      if (!is.factor(x)) {
        x <- factor(x, levels = as.character(sort(unique(x))))
      }

      x <- droplevels(x)
      tbl <- table(x)
      choices <- names(tbl)
      names(choices) <- tbl

      private$set_choices(as.list(choices))
      self$set_selected(unname(choices))
      private$histogram_data <- data.frame(
        x = levels(x),
        y = tabulate(x)
      )

      return(invisible(self))
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
    #' `<varname> %in%  c(<values selected>)` with
    #' optional `is.na(<varname>)`.
    #' @return (`call`)
    get_call = function() {
      filter_call <- call_condition_choice(
        varname = private$get_varname_prefixed(),
        choice = self$get_selected()
      )

      filter_call <- private$add_keep_na_call(filter_call)

      filter_call
    },

    #' @description
    #' Set state
    #' @param state (`list`)\cr
    #'  contains fields relevant for a specific class
    #' \itemize{
    #' \item{`selected`}{ defines initial selection}
    #' \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
    #' }
    set_state = function(state) {
      if (!is.null(state$selected)) {
        state$selected <- as.character(state$selected)
      }
      super$set_state(state)
      invisible(NULL)
    },

    #' @description
    #' Sets the selected values of this `ChoicesFilterState`.
    #'
    #' @param value (`character`) the array of the selected choices.
    #'   Must not contain NA values.
    #'
    #' @return invisibly `NULL`
    #'
    #' @note Casts the passed object to `character` before validating the input
    #' making it possible to pass any object coercible to `character` to this method.
    #'
    #' @examples
    #' filter <- teal.slice:::ChoicesFilterState$new(c("a", "b", "c"), varname = "name")
    #' filter$set_selected(c("c", "a"))
    set_selected = function(value) {
      super$set_selected(value)
    }
  ),

  # private members ----

  private = list(
    histogram_data = data.frame(),

    # private methods ----
    validate_selection = function(value) {
      if (!is.character(value)) {
        stop(
          sprintf(
            "Values of the selection for `%s` in `%s` should be an array of character.",
            self$get_varname(),
            self$get_dataname()
          )
        )
      }
      pre_msg <- sprintf(
        "data '%s', variable '%s': ",
        self$get_dataname(),
        self$get_varname()
      )
      check_in_subset(value, private$choices, pre_msg = pre_msg)
    },
    cast_and_validate = function(values) {
      tryCatch(
        expr = {
          values <- as.character(values)
          if (any(is.na(values))) stop()
        },
        error = function(error) stop("The array of set values must contain values coercible to character.")
      )
      values
    },
    remove_out_of_bound_values = function(values) {
      in_choices_mask <- values %in% private$choices
      if (length(values[!in_choices_mask]) > 0) {
        warning(paste(
          "Values:", strtrim(paste(values[!in_choices_mask], collapse = ", "), 360),
          "are not in choices of column", private$varname, "in dataset", private$dataname, "."
        ))
      }
      values[in_choices_mask]
    },
    is_checkboxgroup = function() {
      length(private$choices) <= getOption("teal.threshold_slider_vs_checkboxgroup")
    },
    get_choice_labels = function() {
      if (private$is_checkboxgroup()) {
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
            if (!is.null(private$x_reactive())) {
              div(
                class = "choices_state_label",
                style = sprintf("width:%s%%", f_freq * 100)
              )
            },
            div(
              class = "choices_state_label_text",
              sprintf(
                "%s (%s%s)", private$choices[i],
                if (is.null(private$x_reactive())) "" else sprintf("%s/", f_count),
                l_count
              )
            )
          )
        })
      } else {
        x <- if (is.null(private$x_reactive())) {
          ""
        } else {
          sprintf("%s/", table(factor(private$x_reactive(), levels = private$choices)))
        }
        sprintf("%s (%s%s)", private$choices, x, names(private$choices))
      }
    },

    # shiny modules ----

    # @description
    # UI Module for `ChoicesFilterState`.
    # This UI element contains available choices selection and
    # checkbox whether to keep or not keep the `NA` values.
    # @param id (`character(1)`)\cr
    #  id of shiny element
    ui_inputs = function(id) {
      ns <- NS(id)
      div(
        if (private$is_checkboxgroup()) {
          div(
            class = "choices_state",
            checkboxGroupInput(
              inputId = ns("selection"),
              label = NULL,
              selected = isolate(self$get_selected()),
              choiceNames = isolate(private$get_choice_labels()),
              choiceValues = as.character(private$choices),
              width = "100%"
            )
          )
        } else {
          teal.widgets::optionalSelectInput(
            inputId = ns("selection"),
            choices = isolate(stats::setNames(private$choices, private$get_choice_labels())),
            selected = isolate(self$get_selected()),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = (length(private$choices) > 10),
              noneSelectedText = "Select a value"
            )
          )
        },
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
          logger::log_trace("ChoicesFilterState$server initializing, dataname: { private$dataname }")

          # this observer is needed in the situation when private$selected has been
          # changed directly by the api - then it's needed to rerender UI element
          # to show relevant values
          private$observers$selection_api <- observeEvent(
            ignoreNULL = FALSE, # it's possible that nothing is selected
            ignoreInit = TRUE,
            eventExpr = self$get_selected(),
            handlerExpr = {
              if (!setequal(self$get_selected(), input$selection)) {
                if (private$is_checkboxgroup()) {
                  updateCheckboxGroupInput(
                    session = session,
                    inputId = "selection",
                    selected = self$get_selected()
                  )
                } else {
                  teal.widgets::updateOptionalSelectInput(
                    session = session,
                    inputId = "selection",
                    selected =  self$get_selected()
                  )
                }
                logger::log_trace(sprintf(
                  "ChoicesFilterState$server@1 selection of variable %s changed, dataname: %s",
                  private$varname,
                  private$dataname
                ))
              }
            }
          )

          private$observers$selection <- observeEvent(
            ignoreNULL = FALSE, # it's possible that nothing is selected
            ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
            eventExpr = input$selection,
            handlerExpr = {
              selection <- if (is.null(input$selection)) character(0) else input$selection
              self$set_selected(selection)
              logger::log_trace(sprintf(
                "ChoicesFilterState$server@2 selection of variable %s changed, dataname: %s",
                private$varname,
                private$dataname
              ))
            }
          )
          private$keep_na_srv("keep_na")

          observeEvent(private$x_reactive(), {
            if (private$is_checkboxgroup()) {
              updateCheckboxGroupInput(
                session,
                inputId = "selection",
                choiceNames = private$get_choice_labels(),
                choiceValues = as.character(private$choices),
                selected = input$selection
              )
            } else {
              teal.widgets::updateOptionalSelectInput(
                session, "selection",
                choices = stats::setNames(private$choices, private$get_choice_labels()),
                selected = input$selection
              )
            }
          })

          logger::log_trace("ChoicesFilterState$server initialized, dataname: { private$dataname }")
          NULL
        }
      )
    }
  )
)
