# FilterState ------

#' @name FilterState
#' @docType class
#'
#' @title `FilterState` abstract class
#'
#' @description Abstract class to encapsulate single filter state.
#'
#' @details
#' This class is responsible for managing a single filter item within a `FilteredData` object
#' and outputs a condition call (logical predicate) for subsetting one variable.
#' Filter states depend on the variable type:
#' (`logical`, `integer`, `numeric`, `character`, `factor`, `Date`, `POSIXct`, `POSIXlt`)
#' and `FilterState` subclasses exist that correspond to those types.
#' - `logical`: `class = LogicalFilterState`
#' - `integer`: `class = RangeFilterState`
#' - `numeric`: `class = RangeFilterState`
#' - `character`: `class = ChoicesFilterState`
#' - `factor`: `class = ChoicesFilterState`
#' - `Date`: `class = DateFilterState`
#' - `POSIXct`, `POSIXlt`: `class = DatetimeFilterState`
#' - all `NA` entries: `class: FilterState`, cannot be filtered
#' - default: `FilterState`, cannot be filtered
#'
#' Each variable's filter state is an `R6` object keeps the variable that is filtered,
#' a `teal_slice` object that describes the filter state, as well as a `shiny` module (UI and server)
#' that allows the user to alter the filter state.
#' Changes to the filter state that cause some observations to be omitted
#' trigger the `get_call` method and every `R` function call up in the reactive chain.
#'
#' @section Modifying state:
#' Modifying a `FilterState` object is possible in three scenarios:
#' - In an interactive session, by passing an appropriate `teal_slice` to the `set_state` method.
#' - In a running application, by changing appropriate inputs.
#' - In a running application, by using [filter_state_api] which directly uses
#' `set_state` method of the `FilterState` object.
#'
#' @keywords internal
#'
FilterState <- R6::R6Class( # nolint
  "FilterState",

  # public methods ----
  public = list(

    #' @description
    #' Initialize a `FilterState` object.
    #'
    #' @param x (`vector`)
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
    #' @return Object of class `FilterState`, invisibly.
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          slice,
                          extract_type = character(0)) {
      checkmate::assert_class(x_reactive, "reactive")
      checkmate::assert_class(slice, "teal_slice")
      checkmate::assert_character(extract_type, max.len = 1, any.missing = FALSE)
      if (length(extract_type) == 1) {
        checkmate::assert_choice(extract_type, choices = c("list", "matrix"))
      }

      # Set data properties.
      private$x <- x
      private$x_reactive <- x_reactive
      # Set derived data properties.
      private$na_count <- sum(is.na(x))
      private$filtered_na_count <- reactive(
        if (!is.null(private$x_reactive())) {
          sum(is.na(private$x_reactive()))
        }
      )
      # Set extract type.
      private$extract_type <- extract_type

      # Set state properties.
      if (is.null(isolate(slice$keep_na)) && anyNA(x)) slice$keep_na <- TRUE
      private$teal_slice <- slice
      # Obtain variable label.
      varlabel <- attr(x, "label", exact = TRUE)
      # Display only when different from varname.
      private$varlabel <-
        if (is.null(varlabel) || identical(varlabel, private$get_varname())) {
          character(0)
        } else {
          varlabel
        }

      private$state_history <- reactiveVal(list())

      invisible(self)
    },

    #' @description
    #' Returns a formatted string representing this `FilterState` object.
    #'
    #' @param show_all (`logical(1)`) passed to `format.teal_slice`
    #' @param trim_lines (`logical(1)`) passed to `format.teal_slice`
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(show_all = FALSE, trim_lines = TRUE) {
      sprintf(
        "%s:\n%s",
        class(self)[1],
        format(self$get_state(), show_all = show_all, trim_lines = trim_lines)
      )
    },

    #' @description
    #' Prints this `FilterState` object.
    #'
    #' @param ... additional arguments
    #'
    print = function(...) {
      cat(isolate(self$format(...)))
    },

    #' @description
    #' Sets mutable parameters of the filter state.
    #' - `fixed` state is prevented from changing state
    #' - `anchored` state is prevented from removing state
    #'
    #' @param state (`teal_slice`)
    #'
    #' @return `self` invisibly
    #'
    set_state = function(state) {
      checkmate::assert_class(state, "teal_slice")
      if (private$is_fixed()) {
        warning("attempt to set state on fixed filter aborted id: ", private$get_id())
      } else {
        logger::log_debug("{ class(self)[1] }$set_state setting state of filter id: { private$get_id() }")
        isolate({
          if (!is.null(state$selected)) {
            private$set_selected(state$selected)
          }
          if (!is.null(state$keep_na)) {
            private$set_keep_na(state$keep_na)
          }
          if (!is.null(state$keep_inf)) {
            private$set_keep_inf(state$keep_inf)
          }
          current_state <- sprintf(
            "selected: %s; keep_na: %s; keep_inf: %s",
            toString(private$get_selected()),
            private$get_keep_na(),
            private$get_keep_inf()
          )
        })
      }

      invisible(self)
    },


    #' @description
    #' Returns a complete description of the filter state.
    #'
    #' @return A `teal_slice` object.
    #'
    get_state = function() {
      private$teal_slice
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
    #'
    get_call = function() {
      stop("this is a virtual method")
    },

    #' @description
    #' `shiny` module server.
    #'
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #'
    #' @param remove_callback (`function`)
    #'   callback to handle removal of this `FilterState` object from `state_list`
    #'
    #' @return Reactive expression signaling that remove button has been clicked.
    #'
    server = function(id, remove_callback) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_debug("FilterState$server initializing module for slice: { private$get_id() } ")
          private$server_summary("summary")
          if (private$is_fixed()) {
            private$server_inputs_fixed("inputs")
          } else {
            private$server_inputs("inputs")
          }

          private$session_bindings[[session$ns("state")]] <- observeEvent(
            eventExpr = list(private$get_selected(), private$get_keep_na(), private$get_keep_inf()),
            handlerExpr = {
              current_state <- as.list(self$get_state())
              history <- private$state_history()
              history_update <- c(history, list(current_state))
              private$state_history(history_update)
            }
          )

          private$session_bindings[[session$ns("back")]] <- observeEvent(
            eventExpr = input$back,
            handlerExpr = {
              history <- rev(private$state_history())
              slice <- history[[2L]]
              history_update <- rev(history[-(1:2)])
              private$state_history(history_update)
              self$set_state(as.teal_slice(slice))
            }
          )

          private$session_bindings[[session$ns("reset")]] <- observeEvent(
            eventExpr = input$reset,
            handlerExpr = {
              slice <- private$state_history()[[1L]]
              self$set_state(as.teal_slice(slice))
            }
          )

          # Buttons for rewind/reset are disabled upon change in history to prevent double-clicking.
          # Re-enabling occurs after 100 ms, after they are potentially hidden when no history is present.
          private$session_bindings[[session$ns("state_history")]] <- observeEvent(
            eventExpr = private$state_history(),
            handlerExpr = {
              shinyjs::disable(id = "back")
              shinyjs::disable(id = "reset")
              shinyjs::delay(
                ms = 100,
                expr = {
                  shinyjs::toggleElement(id = "back", condition = length(private$state_history()) > 1L)
                  shinyjs::enable(id = "back")
                }
              )
              shinyjs::delay(
                ms = 100,
                expr = {
                  shinyjs::toggleElement(id = "reset", condition = length(private$state_history()) > 1L)
                  shinyjs::enable(id = "reset")
                }
              )
            }
          )

          private$session_bindings[[session$ns("remove")]] <- observeEvent(
            once = TRUE, # remove button can be called once, should be destroyed afterwards
            ignoreInit = TRUE, # ignoreInit: should not matter because we destroy the previous input set of the UI
            eventExpr = input$remove, # when remove button is clicked in the FilterState ui
            handlerExpr = remove_callback()
          )

          private$session_bindings[[session$ns("inputs")]] <- list(
            destroy = function() {
              logger::log_debug("Destroying FilterState inputs and observers; id: { private$get_id() }")
              lapply(session$ns(names(input)), .subset2(input, "impl")$.values$remove)
            }
          )

          private$state_history <- reactiveVal(list())

          NULL
        }
      )
    },

    #' @description
    #' `shiny` UI module.
    #'  The UI for this class contains simple message stating that it is not supported.
    #' @param id (`character(1)`)
    #'  `shiny` module instance id.
    #' @param parent_id (`character(1)`) id of the `FilterStates` card container
    ui = function(id, parent_id = "cards") {
      ns <- NS(id)

      # Filter card consists of header and body, arranged in a single column.
      # Body is hidden and is toggled by clicking on header.
      ## Header consists of title and summary, arranged in a column.
      ### Title consists of conditional icon, varname, conditional varlabel, and controls, arranged in a row.
      ### Summary consists of value and controls, arranged in a row.

      tags$div(
        id = id,
        class = "panel filter-card",
        include_js_files("count-bar-labels.js"),
        tags$div(
          class = "filter-card-header",
          `data-toggle` = "collapse",
          `data-bs-toggle` = "collapse",
          href = paste0("#", ns("body")),
          tags$div(
            class = "filter-card-title",
            if (private$is_anchored() && private$is_fixed()) {
              icon("anchor-lock", class = "filter-card-icon")
            } else if (private$is_anchored() && !private$is_fixed()) {
              icon("anchor", class = "filter-card-icon")
            } else if (!private$is_anchored() && private$is_fixed()) {
              icon("lock", class = "filter-card-icon")
            },
            tags$div(class = "filter-card-varname", tags$strong(private$get_varname())),
            tags$div(class = "filter-card-varlabel", private$get_varlabel()),
            tags$div(
              class = "filter-card-controls",
              # Suppress toggling body when clicking on this div.
              # This is for bootstrap 3 and 4. Causes page to scroll to top, prevented by setting href on buttons.
              onclick = "event.stopPropagation();event.preventDefault();",
              # This is for bootstrap 5.
              `data-bs-toggle` = "collapse",
              `data-bs-target` = NULL,
              if (isFALSE(private$is_fixed())) {
                actionLink(
                  inputId = ns("back"),
                  label = NULL,
                  icon = icon("circle-arrow-left", lib = "font-awesome"),
                  title = "Rewind state",
                  class = "filter-card-back",
                  style = "display: none"
                )
              },
              if (isFALSE(private$is_fixed())) {
                actionLink(
                  inputId = ns("reset"),
                  label = NULL,
                  icon = icon("circle-arrow-up", lib = "font-awesome"),
                  title = "Restore original state",
                  class = "filter-card-back",
                  style = "display: none"
                )
              },
              if (isFALSE(private$is_anchored())) {
                actionLink(
                  inputId = ns("remove"),
                  label = icon("circle-xmark", lib = "font-awesome"),
                  title = "Remove filter",
                  class = "filter-card-remove"
                )
              }
            )
          ),
          tags$div(class = "filter-card-summary", private$ui_summary(ns("summary")))
        ),
        tags$div(
          id = ns("body"),
          class = "collapse out",
          `data-parent` = paste0("#", parent_id),
          `data-bs-parent` = paste0("#", parent_id),
          tags$div(
            class = "filter-card-body",
            if (private$is_fixed()) {
              private$ui_inputs_fixed(ns("inputs"))
            } else {
              private$ui_inputs(ns("inputs"))
            }
          )
        )
      )
    },

    #' @description
    #' Destroy inputs and observers stored in `private$session_bindings`.
    #'
    #'
    #' @return `NULL`, invisibly.
    #'
    finalize = function() {
      .finalize_session_bindings(self, private)
      invisible(NULL)
    }
  ),

  # private members ----
  private = list(
    # set by constructor
    x = NULL, # the filtered variable
    x_reactive = NULL, # reactive containing the filtered variable, used for updating counts and histograms
    teal_slice = NULL, # stores all transferable properties of this filter state
    extract_type = character(0), # used by private$get_varname_prefixed
    na_count = integer(0),
    filtered_na_count = NULL, # reactive containing the count of NA in the filtered dataset
    varlabel = character(0), # taken from variable labels in data; displayed in filter cards
    # other
    is_choice_limited = FALSE, # flag whether number of possible choices was limited when specifying filter
    session_bindings = list(), # stores observers and inputs to destroy afterwards
    state_history = NULL, # reactiveVal holding a list storing states this FilterState has had since instantiation

    # private methods ----

    # setters for state features ----

    # @description
    # Set values that can be selected from.
    set_choices = function(choices) {
      stop("this is a virtual method")
    },

    # @description
    # Set selection.
    #
    # @param value (`vector`)
    #   value(s) that come from filter selection; values are set in the
    #   module server after a selection is made in the app interface;
    #   values are stored in `teal_slice$selected` which is reactive;
    #   value types have to be the same as `private$get_choices()`
    #
    # @return `NULL`, invisibly.
    set_selected = function(value) {
      logger::log_debug(
        sprintf(
          "%s$set_selected setting selection of id: %s",
          class(self)[1],
          private$get_id()
        )
      )
      isolate({
        value <- private$cast_and_validate(value)
        value <- private$check_length(value)
        value <- private$remove_out_of_bounds_values(value)
        private$teal_slice$selected <- value
      })

      invisible(NULL)
    },

    # @description
    # Sets `value` in `private$teal_slice$keep_na`.
    #
    # @param value (`logical(1)`)
    #   corresponding to the state of a checkbox input in the `shiny` interface.
    #
    # @return `NULL`, invisibly.
    #
    set_keep_na = function(value) {
      checkmate::assert_flag(value)
      private$teal_slice$keep_na <- value
      logger::log_debug(
        sprintf(
          "%s$set_keep_na set for filter %s to %s.",
          class(self)[1],
          private$get_id(),
          value
        )
      )
      invisible(NULL)
    },

    # @description
    # Sets `value` in `private$teal_slice$keep_inf`.
    #
    # @param value (`logical(1)`)
    #   corresponding to the state of a checkbox input in the `shiny` interface.
    #
    # @return `NULL`, invisibly.
    #
    set_keep_inf = function(value) {
      checkmate::assert_flag(value)
      private$teal_slice$keep_inf <- value
      logger::log_debug(
        sprintf(
          "%s$set_keep_inf of filter %s set to %s",
          class(self)[1],
          private$get_id(),
          value
        )
      )

      invisible(NULL)
    },

    # getters for state features ----

    # @description
    # Returns dataname.
    # @return `character(1)`
    get_dataname = function() {
      isolate(private$teal_slice$dataname)
    },

    # @description
    # Get variable name.
    # @return `character(1)`
    get_varname = function() {
      isolate(private$teal_slice$varname)
    },

    # @description
    # Get id of the teal_slice.
    # @return `character(1)`
    get_id = function() {
      isolate(private$teal_slice$id)
    },

    # @description
    # Get allowed values from `FilterState`.
    # @return
    # Vector describing the available choices. Return type depends on the `FilterState` subclass.
    get_choices = function() {
      isolate(private$teal_slice$choices)
    },

    # @description
    # Get selected values from `FilterState`.
    # @return
    # Vector describing the current selection. Return type depends on the `FilterState` subclass.
    get_selected = function() {
      private$teal_slice$selected
    },

    # @description
    # Returns current `keep_na` selection.
    # @return `logical(1)`
    get_keep_na = function() {
      private$teal_slice$keep_na
    },

    # @description
    # Returns current `keep_inf` selection.
    # @return (`logical(1)`)
    get_keep_inf = function() {
      private$teal_slice$keep_inf
    },

    # Check whether this filter is fixed (cannot be changed).
    # @return `logical(1)`
    is_fixed = function() {
      isolate(isTRUE(private$teal_slice$fixed))
    },

    # Check whether this filter is anchored (cannot be removed).
    # @return `logical(1)`
    is_anchored = function() {
      isolate(isTRUE(private$teal_slice$anchored))
    },

    # Check whether this filter is capable of selecting multiple values.
    # @return `logical(1)`
    is_multiple = function() {
      isolate(isTRUE(private$teal_slice$multiple))
    },

    # other ----

    # @description
    # Returns variable label.
    # @return `character(1)`
    get_varlabel = function() {
      private$varlabel
    },

    # @description
    # Return variable name prefixed by `dataname` to be evaluated as extracted object, for example `data$var`
    # @return Call that extracts the variable from the dataset.
    get_varname_prefixed = function(dataname) {
      varname <- private$get_varname()
      varname_backticked <- sprintf("`%s`", varname)
      ans <-
        if (isTRUE(private$extract_type == "list")) {
          sprintf("%s$%s", dataname, varname_backticked)
        } else if (isTRUE(private$extract_type == "matrix")) {
          sprintf("%s[, \"%s\"]", dataname, varname)
        } else {
          varname_backticked
        }
      str2lang(ans)
    },

    # @description
    # Adds `is.na(varname)` moiety to the existing condition call, according to `keep_na` status.
    # @param filter_call `call` raw filter call, as defined by selection
    # @param varname `character(1)` name of a variable
    # @return `call`
    add_keep_na_call = function(filter_call, varname) {
      # No need to deal with NAs.
      if (private$na_count == 0L) {
        return(filter_call)
      }

      if (is.null(filter_call) && isFALSE(private$get_keep_na())) {
        call("!", call("is.na", varname))
      } else if (!is.null(filter_call) && isTRUE(private$get_keep_na())) {
        call("|", call("is.na", varname), filter_call)
      } else if (!is.null(filter_call) && isFALSE(private$get_keep_na())) {
        call("&", call("!", call("is.na", varname)), filter_call)
      }
    },

    # Converts values to the type fitting this `FilterState` and validates the conversion.
    # Raises error if casting does not execute successfully.
    #
    # @param values vector of values
    #
    # @return vector converted to appropriate class
    cast_and_validate = function(values) {
      values
    },

    # Checks length of selection.
    check_length = function(values) {
      values
    },

    # Filters out erroneous values from vector.
    #
    # @param values vector of values
    #
    # @return vector in which values that cannot be set in this FilterState have been dropped
    remove_out_of_bounds_values = function(values) {
      values
    },

    # Checks if the selection is valid in terms of class and length.
    # It should not return anything but raise an error if selection
    # has a wrong class or is outside of possible choices
    validate_selection = function(value) {
      invisible(NULL)
    },

    # @description
    # Checks whether the current settings actually cause any values to be omitted.
    # @return logical scalar
    is_any_filtered = function() {
      if (private$is_choice_limited) {
        TRUE
      } else if (!setequal(private$get_selected(), private$get_choices())) {
        TRUE
      } else if (!isTRUE(private$get_keep_na()) && private$na_count > 0) {
        TRUE
      } else {
        FALSE
      }
    },

    # shiny modules -----

    # @description
    # Server module to display filter summary
    # @param id (`character(1)`) `shiny` module instance id.
    ui_summary = function(id) {
      ns <- NS(id)
      uiOutput(ns("summary"), class = "filter-card-summary")
    },

    # @description
    # UI module to display filter summary
    # @param id (`character(1)`) `shiny` module instance id.
    # @return Nothing. Renders the UI.
    server_summary = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          output$summary <- renderUI(private$content_summary())
        }
      )
    },

    # module with inputs
    ui_inputs = function(id) {
      stop("abstract class")
    },
    # module with inputs
    server_inputs = function(id) {
      stop("abstract class")
    },

    # @description
    # Module displaying inputs in a fixed filter state.
    # There are no input widgets, only selection visualizations.
    # @param id (`character(1)`) `shiny` module instance id.
    ui_inputs_fixed = function(id) {
      ns <- NS(id)
      tags$div(
        class = "choices_state",
        uiOutput(ns("selection"))
      )
    },

    # @description
    # Module creating the display of a fixed filter state.
    # @param id (`character(1)`) `shiny` module instance id.
    server_inputs_fixed = function(id) {
      stop("abstract class")
    },

    # @description
    # Module UI function displaying input to keep or remove NA in the `FilterState` call.
    # Renders a checkbox input only when variable with which `FilterState` has been created contains NAs.
    # @param id (`character(1)`) `shiny` module instance id.
    keep_na_ui = function(id) {
      ns <- NS(id)
      if (private$na_count > 0) {
        isolate({
          countmax <- private$na_count
          countnow <- private$filtered_na_count()
          ui_input <- checkboxInput(
            inputId = ns("value"),
            label = tags$span(
              id = ns("count_label"),
              make_count_text(
                label = "Keep NA",
                countmax = countmax,
                countnow = countnow
              )
            ),
            value = private$get_keep_na()
          )
          tags$div(
            uiOutput(ns("trigger_visible"), inline = TRUE),
            ui_input
          )
        })
      } else {
        NULL
      }
    },

    # @description
    # Module server function to handle NA values in the `FilterState`.
    # Sets `private$slice$keep_na` according to the selection
    # and updates the relevant UI element if `private$slice$keep_na` has been changed by the api.
    # @param id (`character(1)`) `shiny` module instance id.
    # @return `NULL`, invisibly.
    keep_na_srv = function(id) {
      moduleServer(id, function(input, output, session) {
        # 1. renderUI is used here as an observer which triggers only if output is visible
        #  and if the reactive changes - reactive triggers only if the output is visible.
        # 2. We want to trigger change of the labels only if reactive count changes (not underlying data)
        output$trigger_visible <- renderUI({
          updateCountText(
            inputId = "count_label",
            label = "Keep NA",
            countmax = private$na_count,
            countnow = private$filtered_na_count()
          )
          NULL
        })

        # this observer is needed in the situation when private$keep_inf has been
        # changed directly by the api - then it's needed to rerender UI element
        # to show relevant values
        private$session_bindings[[session$ns("keep_na_api")]] <- observeEvent(
          ignoreNULL = FALSE, # nothing selected is possible for NA
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = private$get_keep_na(),
          handlerExpr = {
            if (!setequal(private$get_keep_na(), input$value)) {
              logger::log_debug("FilterState$keep_na_srv@1 changed reactive value, id: { private$get_id() }")
              updateCheckboxInput(
                inputId = "value",
                label = sprintf("Keep NA (%s/%s)", private$filtered_na_count(), private$na_count),
                value = private$get_keep_na()
              )
            }
          }
        )
        private$session_bindings[[session$ns("keep_na")]] <- observeEvent(
          ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = input$value,
          handlerExpr = {
            logger::log_debug("FilterState$keep_na_srv@2 changed input, id: { private$get_id() }")
            keep_na <- if (is.null(input$value)) {
              FALSE
            } else {
              input$value
            }
            private$set_keep_na(keep_na)
          }
        )
        invisible(NULL)
      })
    }
  )
)
