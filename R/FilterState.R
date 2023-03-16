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
#' which trigger `get_call()` and every R function call up in reactive
#' chain.
#' \cr
#' \cr
#' @section Modifying state:
#' Modifying a `FilterState` object is possible in three scenarios:
#' * In the interactive session by directly specifying values of `selected`,
#'   `keep_na` or `keep_inf` using `set_state` method (to update all at once),
#'   or using `set_selected`, `set_keep_na` or `set_keep_inf`
#' * In a running application by changing appropriate inputs
#' * In a running application by using [filter_state_api] which directly uses `set_state` method
#'  of the `FilterState` object.
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
    #' @param choices (`vector`, unique(na.omit(x)))\cr
    #'   vector specifying allowed selection values
    #' @param selected (`atomic`, `NULL`)\cr
    #'   vector specifying selection
    #' @param keep_na (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep missing values
    #' @param keep_inf (`logical(1)`, `NULL`)\cr
    #'   flag specifying whether to keep infinite values
    #' @param fixed (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated fixed
    #' @param extras (`named list` or `NULL`) of `character` vectors\cr
    #'   storing additional information on this filter state
    #' @param varlabel (`character(0)`, `character(1)`)\cr
    #'   label of the variable (optional)
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #' whether condition calls should be prefixed by dataname. Possible values:
    #' \itemize{
    #' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
    #' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
    #' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
    #' }
    #'
    #' @return self invisibly
    #'
    initialize = function(x,
                          x_reactive = reactive(NULL),
                          dataname,
                          varname,
                          choices = unique(na.omit(x)),
                          selected = NULL,
                          keep_na = NULL,
                          keep_inf = NULL,
                          fixed = FALSE,
                          extras = NULL,
                          dataname_prefixed = character(0),
                          varlabel = character(0),
                          extract_type = character(0)) {
      checkmate::assert_class(x_reactive, "reactive")
      checkmate::assert_string(dataname)
      checkmate::assert_string(varname)
      checkmate::assert_character(varlabel, max.len = 1, any.missing = FALSE)
      checkmate::assert_character(extract_type, max.len = 1, any.missing = FALSE)
      if (length(extract_type) == 1) {
        checkmate::assert_choice(extract_type, choices = c("list", "matrix"))
      }
      if (length(extract_type) == 1 && is.null(dataname)) {
        stop("if extract_type is specified, dataname must also be specified")
      }
      private$dataname <- dataname
      private$varname <- varname
      private$selected <- reactiveVal(NULL)
      private$keep_na <- reactiveVal(keep_na)
      private$extras <- extras
      private$fixed <- fixed
      private$dataname_prefixed <- dataname_prefixed
      private$varlabel <-
        if (identical(varlabel, as.character(varname))) {
          # to not display duplicated label
          character(0)
        } else {
          varlabel
        }
      private$extract_type <- extract_type
      private$na_count <- sum(is.na(x))
      private$x_reactive <- x_reactive
      private$filtered_na_count <- reactive(
        if (!is.null(private$x_reactive())) {
          sum(is.na(private$x_reactive()))
        }
      )
      private$disabled <- reactiveVal(FALSE)
      private$set_choices(choices)
      self$set_selected(selected)
      logger::log_trace(
        sprintf(
          "Instantiated %s with variable %s, dataname: %s",
          class(self)[1],
          private$varname,
          private$dataname
        )
      )
      invisible(self)
    },

    #' @description
    #' Destroy observers stored in `private$observers`.
    #'
    #' @return NULL invisibly
    #'
    destroy_observers = function() {
      lapply(private$observers, function(x) x$destroy())
      return(invisible(NULL))
    },

    #' @description
    #' Returns a formatted string representing this `FilterState`.
    #'
    #' @param indent (`numeric(1)`)
    #'   number of spaces before after each new line character of the formatted string;
    #'   defaults to 0
    #' @param wrap_width (`numeric(1)`)
    #'   number of characters to wrap lines at in the printed output;
    #'   allowed range is 30 to 120; defaults to 76
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(indent = 0L, wrap_width = 76L) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0L)
      checkmate::assert_number(wrap_width, finite = TRUE, lower = 30L, upper = 120L)

      # List all selected values separated by commas.
      values <- paste(format(self$get_selected(), nsmall = 3L, justify = "none"), collapse = ", ")
      paste(c(
        strwrap(
          sprintf("Filtering on: %s", private$varname),
          width = wrap_width,
          indent = indent
        ),
        # Add wrapping and progressive indent to values enumeration as it is likely to be long.
        strwrap(
          sprintf("Selected values: %s", values),
          width = wrap_width,
          indent = indent + 2L,
          exdent = indent + 4L
        ),
        strwrap(
          sprintf("Include missing values: %s", self$get_keep_na()),
          width = wrap_width,
          indent = indent + 2L
        )
      ), collapse = "\n")
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
    #'
    get_call = function() {
      NULL
    },

    #' @description
    #' Returns dataname or "NULL" if dataname is NULL.
    #'
    #' @return `character(1)`
    #'
    get_dataname = function() {
      if (!is.null(private$dataname)) {
        private$dataname
      } else {
        character(1)
      }
    },

    #' @description
    #' Returns current `keep_na` selection.
    #'
    #' @return `logical(1)`
    #'
    get_keep_na = function() {
      private$keep_na()
    },

    #' @description
    #' Returns variable label.
    #'
    #' @return `character(1)`
    #'
    get_varlabel = function() {
      private$varlabel
    },

    #' @description
    #' Get variable name.
    #'
    #' @return `character(1)`
    #'
    get_varname = function() {
      private$varname
    },

    #' @description
    #' Get selected values from `FilterState`.
    #'
    #' @return class of the returned object depends of class of the `FilterState`
    #'
    get_selected = function() {
      private$selected()
    },

    #' @description
    #' Returns filtering state.
    #'
    #' @return A `teal_slice` object.
    #'
    get_state = function() {
      args <- list(
        dataname = private$dataname,
        varname = private$varname,
        choices = private$choices,
        selected = private$selected(),
        keep_na = if (!is.null(private$keep_na)) private$keep_na() else NULL,
        keep_inf = if (!is.null(private$keep_inf)) private$keep_inf() else NULL,
        fixed = private$fixed
      )
      args <- append(args, private$extras)
      args <- Filter(Negate(is.null), args)
      do.call(filter_var, args)
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
    #' Set whether to keep NAs.
    #'
    #' @param value `logical(1)`\cr
    #'   value(s) which come from the filter selection. Value is set in `server`
    #'   modules after selecting check-box-input in the shiny interface. Values are set to
    #'   `private$keep_na` which is reactive.
    #'
    #' @return NULL invisibly
    #'
    set_keep_na = function(value) {
      checkmate::assert_flag(value)
      if (shiny::isolate(private$is_disabled())) {
        warning("This filter state is disabled. Can not change keep NA.")
      } else {
        private$keep_na(value)
        logger::log_trace(
          sprintf(
            "%s$set_keep_na set for variable %s to %s.",
            class(self)[1],
            private$varname,
            value
          )
        )
      }
      invisible(NULL)
    },

    #' @description
    #' Some methods need an additional `!is.na(varame)` condition to drop
    #' missing values. When `private$na_rm = TRUE`, `self$get_call` returns
    #' condition extended by `!is.na`.
    #'
    #' @param value `logical(1)`\cr
    #'   when `TRUE`, `FilterState$get_call` appends an expression
    #'   removing `NA` values to the filter expression returned by `get_call`
    #'
    #' @return NULL invisibly
    #'
    set_na_rm = function(value) {
      checkmate::assert_flag(value)
      private$na_rm <- value
      invisible(NULL)
    },

    #' @description
    #' Set selection.
    #'
    #' @param value (`vector`)\cr
    #'   value(s) that come from filter selection; values are set in the
    #'   module server after a selection is made in the app interface;
    #'   values are stored in `private$selected` which is reactive;
    #'   value types have to be the same as `private$choices`
    #'
    #' @return NULL invisibly
    #'
    set_selected = function(value) {
      logger::log_trace(
        sprintf(
          "%s$set_selected setting selection of variable %s, dataname: %s.",
          class(self)[1],
          private$varname,
          private$dataname
        )
      )

      if (shiny::isolate(private$is_disabled())) {
        warning("This filter state is disabled. Can not change selected.")
      } else {
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
      }

      invisible(NULL)
    },

    #' @description
    #' Sets filtering state.
    #'
    #' @param state a (`teal_slice`) object; see `Filter state specification`
    #'
    #' @return NULL invisibly
    #'
    set_state = function(state) {
      if (inherits(state, "teal_slice")) {
        if (!is.null(state$selected)) {
          self$set_selected(state$selected)
        }
        if (!is.null(state$keep_na)) {
          self$set_keep_na(state$keep_na)
        }
        if (!is.null(state$keep_inf)) {
          self$set_keep_inf(state$keep_inf)
        }
      } else {
        logger::log_trace(sprintf(
          "%s$set_state, dataname: %s setting state of variable %s to: selected=%s, keep_na=%s",
          class(self)[1],
          private$dataname,
          private$varname,
          paste(state$selected, collapse = " "),
          state$keep_na
        ))
        stopifnot(is.list(state) && all(names(state) %in% c("selected", "keep_na")))

        if (!is.null(state$keep_na)) {
          self$set_keep_na(state$keep_na)
        }
        if (!is.null(state$selected)) {
          self$set_selected(state$selected)
        }
        logger::log_trace(
          sprintf(
            "%s$set_state, dataname: %s done setting state for variable %s",
            class(self)[1],
            private$dataname,
            private$varname
          )
        )
      }
      invisible(NULL)
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
          private$server_inputs("inputs")
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

      theme <- getOption("teal.bs_theme")

      if (is.null(theme)) {
        private$ui_bs3(id, parent_id)
      } else {
        private$ui_bs45(id, parent_id)
      }
    }
  ),

  # private members ----
  private = list(
    dataname = character(0),
    varname = character(0),
    choices = NULL, # because each class has different choices type
    is_choice_limited = FALSE, # flag whether number of possible choices was limited when specifying filter
    selected = NULL, # because it holds reactiveVal and each class has different choices type
    varlabel = character(0),
    keep_na = NULL, # reactiveVal holding a logical(1),
    fixed = logical(0), # logical flag whether this filter state is fixed/locked
    extras = list(), # additional information passed in teal_slice (product of filter_var)
    na_rm = FALSE, # logical(1)
    na_count = integer(0),
    filtered_na_count = NULL, # reactive containing the count of NA in the filtered dataset
    observers = NULL, # stores observers
    x_reactive = NULL, # reactive containing the filtered variable, used for updating counts and histograms
    disabled = NULL, # reactiveVal holding a logical(1)
    cache = NULL, # cache state when filter disabled so we can later restore
    extract_type = character(0),
    dataname_prefixed = character(0), # depends on encapsulating FilterStates object class

    # private methods ----

    # @description
    # Return variable name prefixed by dataname to be evaluated as extracted object,
    # for example `data$var`
    # @return a character string representation of a subset call
    #         that extracts the variable from the dataset
    get_varname_prefixed = function() {
      ans <-
        if (isTRUE(private$extract_type == "list")) {
          sprintf("%s$%s", private$dataname_prefixed, private$varname)
        } else if (isTRUE(private$extract_type == "matrix")) {
          sprintf("%s[, \"%s\"]", private$dataname_prefixed, private$varname)
        } else {
          private$varname
        }
      str2lang(ans)
    },

    # @description
    # Adds `is.na(varname)` before existing condition calls if `keep_na` is selected.
    # Otherwise, if missings are found in the variable `!is.na` will be added
    # only if `private$na_rm = TRUE`
    # @return a `call`
    add_keep_na_call = function(filter_call) {
      if (isTRUE(self$get_keep_na())) {
        call("|", call("is.na", private$get_varname_prefixed()), filter_call)
      } else if (isTRUE(private$na_rm) && private$na_count > 0L) {
        call(
          "&",
          call("!", call("is.na", private$get_varname_prefixed())),
          filter_call
        )
      } else {
        filter_call
      }
    },

    # @description
    # Set choices is supposed to be executed once in the constructor
    # to define set/range which selection is made from.
    # parameter choices (`vector`)\cr
    #  class of the vector depends on the `FilterState` class.
    # @return `NULL`
    set_choices = function(choices) {
      private$choices <- choices
      invisible(NULL)
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
      private$disabled(TRUE)
      invisible(NULL)
    },

    # Enables `FilterState`
    # Cached `state` is reset again and cache is cleared.
    # @return `NULL` invisibly
    enable = function() {
      if (!is.null(private$cache)) {
        private$disabled(FALSE)
        self$set_state(private$cache)
        private$cache <- NULL
      }
      invisible(NULL)
    },

    # Check whether filter is disabled
    # @return `logical(1)`
    is_disabled = function() {
      private$disabled()
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
    # @param shiny `id` parametr passed to moduleServer
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

    #' module with inputs
    ui_inputs = function(id) {
      stop("abstract class")
    },
    # module with inputs
    server_inputs = function(id) {
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
            value = shiny::isolate(self$get_keep_na())
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
          eventExpr = self$get_keep_na(),
          ignoreNULL = FALSE, # nothing selected is possible for NA
          ignoreInit = TRUE, # ignoreInit: should not matter because we set the UI with the desired initial state
          handlerExpr = {
            if (!setequal(self$get_keep_na(), input$value)) {
              updateCheckboxInput(
                inputId = "value",
                label = sprintf("Keep NA (%s/%s)", private$filtered_na_count(), private$na_count),
                value = self$get_keep_na()
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
            self$set_keep_na(keep_na)
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
    },
    # @description
    # Filter card UI for Bootstrap 3.
    #
    # @param id (`character(1)`) Id for the containing HTML element.
    # @param parent_id (`character(1)`) id of the FilterStates card container
    ui_bs3 = function(id, parent_id) {
      ns <- NS(id)

      enable <- !private$is_disabled()

      tags$div(
        id = id,
        class = "panel panel-default",
        include_js_files("count-bar-labels.js"),
        tags$div(
          class = "panel-heading",
          tags$div(
            class = "panel-title",
            tags$a(
              class = "accordion-toggle",
              `data-toggle` = "collapse",
              `data-parent` = paste0("#", parent_id),
              href = paste0("#", ns("body")),
              tags$span(tags$strong(self$get_varname())),
              if (length(self$get_varlabel())) {
                tags$span(self$get_varlabel(), class = "filter-card-varlabel")
              } else {
                NULL
              }
            ),
            shinyWidgets::prettySwitch(
              ns("enable"),
              label = "",
              status = "success",
              fill = TRUE,
              value = enable,
              width = 30
            ),
            actionLink(
              inputId = ns("remove"),
              label = icon("circle-xmark", lib = "font-awesome"),
              class = "filter-card-remove"
            )
          ),
          private$ui_summary(ns("summary"))
        ),
        tags$div(
          id = ns("body"),
          class = "panel-collapse collapse out",
          tags$div(
            class = "panel-body",
            private$ui_inputs(ns("inputs"))
          )
        )
      )
    },
    # @description
    # Filter card ui for Bootstrap 4 and 5.
    #
    # @param id (`character(1)`) Id for the containing HTML element.
    # @param parent_id (`character(1)`) id of the FilterStates card container
    ui_bs45 = function(id, parent_id) {
      ns <- NS(id)
      enable <- !private$is_disabled()

      tags$div(
        id = id,
        class = "card",
        include_js_files("count-bar-labels.js"),
        tags$div(
          class = "card-header",
          tags$div(
            class = "card-title",
            tags$a(
              class = "accordion-toggle",
              `data-toggle` = "collapse",
              `data-bs-toggle` = "collapse",
              href = paste0("#", ns("body")),
              tags$span(tags$strong(self$get_varname())),
              if (length(self$get_varlabel())) {
                tags$span(self$get_varlabel(), class = "filter-card-varlabel")
              } else {
                NULL
              }
            ),
            shinyWidgets::prettySwitch(
              ns("enable"),
              label = "",
              status = "success",
              fill = TRUE,
              value = enable,
              width = 30
            ),
            actionLink(
              inputId = ns("remove"),
              label = icon("circle-xmark", lib = "font-awesome"),
              class = "filter-card-remove"
            )
          ),
          private$ui_summary(ns("summary"))
        ),
        tags$div(
          id = ns("body"),
          class = "collapse out",
          `data-parent` = paste0("#", parent_id),
          `data-bs-parent` = paste0("#", parent_id),
          tags$div(
            class = "card-body",
            private$ui_inputs(ns("inputs"))
          )
        )
      )
    }
  )
)
