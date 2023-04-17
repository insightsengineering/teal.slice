#' @name FilterState
#' @docType class
#'
#'
#' @title `FilterState` Abstract Class
#'
#' @description Abstract class to encapsulate single filter state
#'
#' @details
#' This class is responsible for managing single filter item within
#' `FilteredData` class. Filter states depend on the variable type:
#' (`logical`, `integer`, `numeric`, `factor`, `character`, `Date`, `POSIXct`, `POSIXlt`)
#' and returns `FilterState` object with class corresponding to input variable.
#' Class controls single filter entry in `module_single_filter_item` and returns
#' code relevant to selected values.
#' - `factor`, `character`: `class = ChoicesFilterState`
#' - `numeric`: `class = RangeFilterState`
#' - `logical`: `class = LogicalFilterState`
#' - `Date`: `class = DateFilterState`
#' - `POSIXct`, `POSIXlt`: `class = DatetimeFilterState`
#' - all `NA` entries: `class: FilterState`, cannot be filtered
#' - default: `FilterState`, cannot be filtered
#' \cr
#' Each variable's filter state is an `R6` object which contains `choices`,
#' `selected`, `varname`, `dataname`, `labels`, `na_count`, `keep_na` and other
#' variable type specific fields (`keep_inf`, `inf_count`, `timezone`).
#' Object contains also shiny module (`ui` and `server`) which manages
#' state of the filter through reactive values `selected`, `keep_na`, `keep_inf`
#' which trigger `get_call()` and every R function call up in reactive chain.
#' \cr
#' \cr
#' @section Modifying state:
#' Modifying a `FilterState` object is possible in three scenarios:
#' * In the interactive session by passing an appropriate `teal_slice`
#'   to the `set_state` method, or using
#'   `set_selected`, `set_keep_na` or `set_keep_inf` methods.
#' * In a running application by changing appropriate inputs.
#' * In a running application by using [filter_state_api] which directly uses
#' `set_state` method of the `InteractiveFilterState` object.
#'
#' @keywords internal
FilterState <- R6::R6Class( # nolint
  "FilterState",

  # public methods ----
  public = list(

    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`vector`)\cr
    #'   values of the variable used in filter
    #' @param x_reactive (`reactive`)\cr
    #'   returning vector of the same type as `x`. Is used to update
    #'   counts following the change in values of the filtered dataset.
    #'   If it is set to `reactive(NULL)` then counts based on filtered
    #'   dataset are not shown.
    #' @param dataname (`character(1)`)\cr
    #'   optional name of dataset where `x` is taken from. Must be specified
    #'   if `extract_type` argument is not empty.
    #' @param varname (`character(1)`)\cr
    #'   name of the variable.
    #' @param keep_na (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep missing values
    #' @param keep_inf (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep infinite values
    #' @param fixed (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated fixed
    #' @param disabled (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated disabled
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
    #' }
    #' @param ... additional arguments to be saved as a list in `private$extras` field
    #'
    #' @return self invisibly
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          dataname,
                          varname,
                          keep_na = NULL,
                          keep_inf = NULL,
                          fixed = FALSE,
                          disabled = FALSE,
                          extract_type = character(0),
                          ...) {
      checkmate::assert_class(x_reactive, "reactive")
      checkmate::assert_string(dataname)
      checkmate::assert_string(varname)
      checkmate::assert_flag(keep_na, null.ok = TRUE)
      checkmate::assert_flag(keep_inf, null.ok = TRUE)
      checkmate::assert_flag(fixed)
      checkmate::assert_flag(disabled)
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
      # Set state properties.
      private$dataname <- dataname
      private$varname <- varname
      private$selected <- reactiveVal()
      private$keep_na <- reactiveVal(keep_na)
      private$keep_inf <- reactiveVal(keep_inf)
      private$fixed <- fixed
      private$disabled <- reactiveVal(disabled)
      private$extras <- list(...)
      # Set extract type.
      private$extract_type <- extract_type
      # Obtain variable label.
      varlabel <- attr(x, "label")
      # Display only when different from varname.
      private$varlabel <-
        if (is.null(varlabel) || identical(varlabel, private$varname)) {
          character(0)
        } else {
          varlabel
        }

      logger::log_trace("Instantiated FilterState object")

      invisible(self)
    },

    #' @description
    #' Virtual method for printing FilterState.
    #'
    format = function() {
      stop("this is a virtual method")
    },

    #' @description
    #' Prints this `FilterState` object.
    #'
    #' @param ... additional arguments to this method
    #'
    print = function(...) {
      cat(shiny::isolate(self$format()), "\n")
    },

    #' @description
    #' Sets filtering state.
    #'
    #' @param state a `teal_slice` object
    #'
    #' @return `self` invisibly
    #'
    set_state = function(state) {
      checkmate::assert_class(state, "teal_slice")
      if (private$fixed) {
        logger::log_warn("attempt to set state on fixed filter aborted: { private$dataname } { private$varname }")
      } else {
        # Allow for enabling a filter state before altering state.
        if (isTRUE(state$disabled)) private$disable()
        if (isFALSE(state$disabled)) private$enable()

        if (private$is_disabled()) {
          mutables <- state[c("selected", "keep_na", "keep_inf")]
          if (any(!vapply(mutables, is.null, logical(1L)))) {
            logger::log_warn("attempt to set state on disabled filter aborted: { private$dataname } { private$varname }")
          }
        } else {
          logger::log_trace("{ class(self)[1] }$set_state setting state of variable: { private$varname }")
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
            toString(shiny::isolate(private$get_selected())),
            shiny::isolate(private$get_keep_na()),
            shiny::isolate(private$get_keep_inf())
          )

          logger::log_trace("state of variable: { private$varname } set to: { current_state }")
        }
      }

      invisible(self)
    },


    #' @description
    #' Returns filtering state.
    #'
    #' @return A `teal_slice` object.
    #'
    get_state = function() {
      args <- list(
        dataname = private$get_dataname(),
        varname = private$get_varname(),
        choices = private$choices,
        selected = private$get_selected(),
        keep_na = private$get_keep_na(),
        keep_inf = private$get_keep_inf(),
        fixed = private$fixed,
        disabled = private$is_disabled()
      )
      args <- append(args, private$extras)
      args <- Filter(Negate(is.null), args)
      do.call(filter_var, args)
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
          private$server_summary("summary")
          if (private$fixed) {
            private$server_inputs_fixed("inputs")
          } else {
            private$server_inputs("inputs")
          }
          observeEvent(input$enable,
            {
              if (isTRUE(input$enable)) {
                private$enable()
              } else {
                private$disable()
              }
            },
            ignoreInit = TRUE
          )
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
    #' @param parent_id (`character(1)`) id of the FilterStates card container
    ui = function(id, parent_id = "cards") {
      ns <- NS(id)

      tags$div(
        id = id,
        class = "panel filter-card",
        include_js_files("count-bar-labels.js"),
        tags$div(
          class = "filter-card-header",
          tags$div(
            class = "filter-card-title",
            `data-toggle` = "collapse",
            `data-bs-toggle` = "collapse",
            href = paste0("#", ns("body")),
            if (private$fixed) {
              actionButton("dummy", label = NULL, icon = icon("lock"), class = "dummy-button")
            } else {
              NULL
            },
            tags$span(tags$strong(private$get_varname())),
            if (length(private$get_varlabel())) {
              tags$span(private$get_varlabel(), class = "filter-card-varlabel")
            } else {
              NULL
            }
          ),
          tags$div(
            class = "filter-card-controls",
            shinyWidgets::prettySwitch(
              ns("enable"),
              label = "",
              status = "success",
              fill = TRUE,
              value = !private$is_disabled(),
              width = 30
            ),
            actionLink(
              inputId = ns("remove"),
              label = icon("circle-xmark", lib = "font-awesome"),
              class = "filter-card-remove"
            )
          ),
          tags$div(
            class = "filter-card-summary",
            `data-toggle` = "collapse",
            `data-bs-toggle` = "collapse",
            href = paste0("#", ns("body")),
            private$ui_summary(ns("summary"))
          )
        ),
        tags$div(
          id = ns("body"),
          class = "collapse out",
          `data-parent` = paste0("#", parent_id),
          `data-bs-parent` = paste0("#", parent_id),
          tags$div(
            class = "filter-card-body",
            if (private$fixed) {
              private$ui_inputs_fixed(ns("inputs"))
            } else {
              private$ui_inputs(ns("inputs"))
            }
          )
        )
      )
    },

    #' @description
    #' Destroy observers stored in `private$observers`.
    #'
    #' @return NULL invisibly
    #'
    destroy_observers = function() {
      lapply(private$observers, function(x) x$destroy())
      return(invisible(NULL))
    }
  ),

  # private members ----
  private = list(
    # set by constructor
    x = NULL, # the filtered variable
    x_reactive = NULL, # reactive containing the filtered variable, used for updating counts and histograms
    extract_type = character(0), # used by private$get_varname_prefixed
    na_count = integer(0),
    filtered_na_count = NULL, # reactive containing the count of NA in the filtered dataset
    varlabel = character(0),
    # set by set_state
    dataname = character(0),
    varname = character(0),
    choices = NULL, # because each class has different choices type
    selected = NULL, # reactiveVal holding vector of choices (depends on class)
    keep_na = NULL, # reactiveVal holding a logical(1)
    keep_inf = NULL, # reactiveVal holding a logical(1)
    fixed = logical(0), # logical flag whether this filter state is fixed/locked
    extras = list(), # additional information passed in teal_slice (product of filter_var)
    disabled = NULL, # reactiveVal holding a logical(1)
    # other
    is_choice_limited = FALSE, # flag whether number of possible choices was limited when specifying filter
    na_rm = FALSE, # logical(1)
    observers = NULL, # stores observers
    cache = NULL, # cache state when filter disabled so we can later restore

    # @description
    # Set values that can be selected from.
    set_choices = function(choices) {
      stop("this is a virtual method")
    },

    # @description
    # Set selection.
    #
    # @param value (`vector`)\cr
    #   value(s) that come from filter selection; values are set in the
    #   module server after a selection is made in the app interface;
    #   values are stored in `private$selected` which is reactive;
    #   value types have to be the same as `private$choices`
    #
    # @return NULL invisibly
    set_selected = function(value) {
      logger::log_trace(
        sprintf(
          "%s$set_selected setting selection of variable %s, dataname: %s.",
          class(self)[1],
          private$varname,
          private$dataname
        )
      )
      if (is.null(value)) value <- private$choices

      value <- private$cast_and_validate(value)
      value <- private$remove_out_of_bound_values(value)
      private$validate_selection(value)
      private$selected(value)
      logger::log_trace(
        sprintf(
          "%s$set_selected selection of variable %s set, dataname: %s",
          class(self)[1],
          private$varname,
          private$dataname
        )
      )

      invisible(NULL)
    },

    # @description
    # Set whether to keep NAs.
    #
    # @param value `logical(1)`\cr
    #   value(s) which come from the filter selection. Value is set in `server`
    #   modules after selecting check-box-input in the shiny interface. Values are set to
    #   `private$keep_na` which is reactive.
    #
    # @return NULL invisibly
    #
    set_keep_na = function(value) {
      checkmate::assert_flag(value)
      private$keep_na(value)
      logger::log_trace(
        sprintf(
          "%s$set_keep_na set for variable %s to %s.",
          class(self)[1],
          private$varname,
          value
        )
      )

      invisible(NULL)
    },

    # @description
    # Set whether to keep Infs
    #
    # @param value (`logical(1)`)\cr
    #  Value(s) which come from the filter selection. Value is set in `server`
    #  modules after selecting check-box-input in the shiny interface. Values are set to
    #  `private$keep_inf` which is reactive.
    #
    set_keep_inf = function(value) {
      checkmate::assert_flag(value)
      private$keep_inf(value)
      logger::log_trace(
        sprintf(
          "%s$set_keep_inf of variable %s set to %s, dataname: %s.",
          class(self)[1],
          private$varname,
          value,
          private$dataname
        )
      )

      invisible(NULL)
    },

    # @description
    # Some methods need an additional `!is.na(varame)` condition to drop
    # missing values. When `private$na_rm = TRUE`, `self$get_call` returns
    # condition extended by `!is.na`.
    #
    # @param value `logical(1)`\cr
    #   when `TRUE`, `FilterState$get_call` appends an expression
    #   removing `NA` values to the filter expression returned by `get_call`
    #
    # @return NULL invisibly
    #
    set_na_rm = function(value) {
      checkmate::assert_flag(value)
      private$na_rm <- value
      invisible(NULL)
    },

    # @description
    # Returns dataname.
    # @return `character(1)`
    get_dataname = function() {
      private$dataname
    },

    # @description
    # Get variable name.
    # @return `character(1)`
    get_varname = function() {
      private$varname
    },

    # @description
    # Get selected values from `FilterState`.
    # @return class of the returned object depends of class of the `FilterState`
    get_selected = function() {
      private$selected()
    },

    # @description
    # Returns current `keep_na` selection.
    # @return `logical(1)`
    get_keep_na = function() {
      private$keep_na()
    },

    # @description
    # Returns current `keep_inf` selection.
    # @return (`logical(1)`)
    get_keep_inf = function() {
      private$keep_inf()
    },

    # @description
    # Returns variable label.
    # @return `character(1)`
    get_varlabel = function() {
      private$varlabel
    },

    # @description
    # Return variable name prefixed by dataname to be evaluated as extracted object,
    # for example `data$var`
    # @return a character string representation of a subset call
    #         that extracts the variable from the dataset
    get_varname_prefixed = function(dataname) {
      ans <-
        if (isTRUE(private$extract_type == "list")) {
          sprintf("%s$%s", dataname, private$varname)
        } else if (isTRUE(private$extract_type == "matrix")) {
          sprintf("%s[, \"%s\"]", dataname, private$varname)
        } else {
          private$varname
        }
      str2lang(ans)
    },

    # @description
    # Adds `is.na(varname)` before existing condition calls if `keep_na` is selected.
    # Otherwise, if missing values are found in the variable `!is.na` will be added
    # only if `private$na_rm = TRUE`
    # @param filter_call `call` raw filter call, as defined by selection
    # @param dataname `character(1)` name of data set to prepend to variables
    # @return a `call`
    add_keep_na_call = function(filter_call, dataname) {
      if (isTRUE(private$get_keep_na())) {
        call("|", call("is.na", private$get_varname_prefixed(dataname)), filter_call)
      } else if (isTRUE(private$na_rm) && private$na_count > 0L) {
        call(
          "&",
          call("!", call("is.na", private$get_varname_prefixed(dataname))),
          filter_call
        )
      } else {
        filter_call
      }
    },

    # Checks if the selection is valid in terms of class and length.
    # It should not return anything but throw an error if selection
    # has a wrong class or is outside of possible choices
    validate_selection = function(value) {
      invisible(NULL)
    },

    # Filters out erroneous values from an array.
    #
    # @param values the array of values
    #
    # @return the array of values without elements, which are outside of
    # the accepted set for this FilterState
    remove_out_of_bound_values = function(values) {
      values
    },

    # Casts an array of values to the type fitting this `FilterState`
    # and validates the elements of the casted array
    # satisfy the requirements of this `FilterState`.
    #
    # @param values the array of values
    #
    # @return the casted array
    #
    # @note throws an error if the casting did not execute successfully.
    cast_and_validate = function(values) {
      values
    },

    # Disables `FilterState`
    # `state` is moved to cache and set to `NULL`
    # @return `NULL` invisibly
    disable = function() {
      private$cache <- self$get_state()
      private$selected(NULL)
      private$keep_na(NULL)
      private$keep_inf(NULL)
      private$disabled(TRUE)
      invisible(NULL)
    },

    # Enables `FilterState`
    # Cached `state` is reset again and cache is cleared.
    # @return `NULL` invisibly
    enable = function() {
      private$disabled(FALSE)
      if (!is.null(private$cache)) {
        state <- private$cache
        state$disabled <- NULL
        private$fixed <- FALSE
        self$set_state(state)
        private$fixed <- TRUE
        private$cache <- NULL
      }
      invisible(NULL)
    },

    # Check whether this filter is disabled
    # @return `logical(1)`
    is_disabled = function() {
      if (shiny::isRunning()) {
        private$disabled()
      } else {
        shiny::isolate(private$disabled())
      }
    },

    # shiny modules -----

    # @description
    # Server module to display filter summary
    # @param id `shiny` id parameter
    ui_summary = function(id) {
      ns <- NS(id)
      uiOutput(ns("summary"), class = "filter-card-summary")
    },

    # @description
    # UI module to display filter summary
    # @param shiny `id` parameter passed to `moduleServer`
    #  renders text describing current state
    server_summary = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          output$summary <- renderUI({
            if (private$is_disabled()) {
              tags$span("Disabled")
            } else {
              private$content_summary()
            }
          })
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
    # module displaying inputs in a fixed filter state
    # there are no input widgets, only selection visualizations
    # @param id
    #   character string specifying this `shiny` module instance
    ui_inputs_fixed = function(id) {
      ns <- NS(id)
      div(
        class = "choices_state",
        uiOutput(ns("selection"))
      )
    },

    # @description
    # module creating the display of a fixed filter state
    # @param id
    #   character string specifying this `shiny` module instance
    server_inputs_fixed = function(id) {
      stop("abstract class")
    },

    # @description
    # module displaying input to keep or remove NA in the FilterState call
    # @param id `shiny` id parameter
    #  renders checkbox input only when variable from which FilterState has
    #  been created has some NA values.
    keep_na_ui = function(id) {
      ns <- NS(id)
      if (private$na_count > 0) {
        countmax <- private$na_count
        countnow <- isolate(private$filtered_na_count())
        div(
          uiOutput(ns("trigger_visible"), inline = TRUE),
          checkboxInput(
            inputId = ns("value"),
            label = tags$span(
              id = ns("count_label"),
              make_count_text(
                label = "Keep NA",
                countmax = countmax,
                countnow = countnow
              )
            ),
            value = shiny::isolate(private$get_keep_na())
          )
        )
      } else {
        NULL
      }
    },

    # @description
    # module to handle NA values in the FilterState
    # @param shiny `id` parameter passed to moduleServer
    #  module sets `private$keep_na` according to the selection.
    #  Module also updates a UI element if the `private$keep_na` has been
    #  changed through the api
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
        private$observers$keep_na_api <- observeEvent(
          eventExpr = private$get_keep_na(),
          ignoreNULL = FALSE, # nothing selected is possible for NA
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          handlerExpr = {
            if (!setequal(private$get_keep_na(), input$value)) {
              updateCheckboxInput(
                inputId = "value",
                label = sprintf("Keep NA (%s/%s)", private$filtered_na_count(), private$na_count),
                value = private$get_keep_na()
              )
            }
          }
        )
        private$observers$keep_na <- observeEvent(
          ignoreNULL = FALSE, # ignoreNULL: we don't want to ignore NULL when nothing is selected in the `selectInput`
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          eventExpr = input$value,
          handlerExpr = {
            keep_na <- if (is.null(input$value)) {
              FALSE
            } else {
              input$value
            }
            private$set_keep_na(keep_na)
            logger::log_trace(
              sprintf(
                "%s$server keep_na of variable %s set to: %s, dataname: %s",
                class(self)[1],
                private$varname,
                input$value,
                private$dataname
              )
            )
          }
        )
        invisible(NULL)
      })
    }
  )
)
