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
    #' @param slice (`teal_slice`)\cr
    #'   object created by [teal_slice()]
    #' @param extract_type (`character(0)`, `character(1)`)\cr
    #'   specifying whether condition calls should be prefixed by `dataname`. Possible values:
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
      if (is.null(shiny::isolate(slice$keep_na)) && anyNA(x)) slice$keep_na <- TRUE
      private$teal_slice <- slice
      # Obtain variable label.
      varlabel <- attr(x, "label")
      # Display only when different from varname.
      private$varlabel <-
        if (is.null(varlabel) || identical(varlabel, private$get_varname())) {
          character(0)
        } else {
          varlabel
        }

      private$state_history <- reactiveVal(list(as.list(slice)))

      logger::log_trace("Instantiated FilterState object id: { private$get_id() }")

      invisible(self)
    },

    #' @description
    #' Returns a formatted string representing this `FilterState` object.
    #'
    #' @param show_all `logical(1)` passed to `format.teal_slice`
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(show_all = FALSE) {
      sprintf(
        "%s:\n%s",
        class(self)[1],
        format(self$get_state(), show_all = show_all)
      )
    },

    #' @description
    #' Prints this `FilterState` object.
    #'
    #' @param ... additional arguments
    #'
    print = function(...) {
      cat(shiny::isolate(self$format(...)))
    },

    #' @description
    #' Sets filtering state.
    #' - `fixed` state is prevented from changing state
    #' - `locked` state is prevented from removing state
    #'
    #' @param state a `teal_slice` object
    #'
    #' @return `self` invisibly
    #'
    set_state = function(state) {
      checkmate::assert_class(state, "teal_slice")
      if (private$is_fixed()) {
        logger::log_warn("attempt to set state on fixed filter aborted id: { private$get_id() }")
      } else {
        logger::log_trace("{ class(self)[1] }$set_state setting state of filter id: { private$get_id() }")
        shiny::isolate({
          if (!is.null(state$selected)) {
            private$set_selected(state$selected)
          }
          if (!is.null(state$keep_na)) {
            private$set_keep_na(state$keep_na)
          }
          if (!is.null(state$keep_inf)) {
            private$set_keep_inf(state$keep_inf)
          }
        })
      }

      invisible(self)
    },


    #' @description
    #' Returns filtering state.
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
          logger::log_trace("FilterState$server initializing module for slice: { private$get_id() } ")

          private$server_summary("summary")
          if (private$is_fixed()) {
            private$server_inputs_fixed("inputs")
          } else {
            private$server_inputs("inputs")
          }

          private$observers$state <- observeEvent(
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            eventExpr = list(private$get_selected(), private$get_keep_na(), private$get_keep_inf()),
            handlerExpr = {
              current_state <- as.list(self$get_state())
              history <- private$state_history()
              history_update <- c(history, list(current_state))
              private$state_history(history_update)
            }
          )

          private$observers$back <- observeEvent(
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            eventExpr = input$back,
            handlerExpr = {
              history <- rev(private$state_history())
              slice <- history[[2L]]
              history_update <- rev(history[-(1:2)])
              private$state_history(history_update)
              self$set_state(as.teal_slice(slice))
            }
          )

          private$observers$reset <- observeEvent(
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            eventExpr = input$reset,
            handlerExpr = {
              slice <- private$state_history()[[1L]]
              self$set_state(as.teal_slice(slice))
            }
          )

          private$observers$state_history <- observeEvent(
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            eventExpr = private$state_history(),
            handlerExpr = {
              shinyjs::delay(
                ms = 100,
                expr = shinyjs::toggleElement(id = "back", condition = length(private$state_history()) > 1L)
              )
              shinyjs::delay(
                ms = 100,
                expr = shinyjs::toggleElement(id = "reset", condition = length(private$state_history()) > 1L)
              )
            }
          )

          private$destroy_shiny <- function() {
            logger::log_trace("Destroying FilterState inputs and observers; id: { private$get_id() }")
            # remove values from the input list
            lapply(session$ns(names(input)), .subset2(input, "impl")$.values$remove)

            # remove observers
            lapply(private$observers, function(x) x$destroy())
          }

          reactive(input$remove)
        }
      )
    },

    #' @description
    #' Shiny module UI.
    #'
    #' @param id (`character(1)`)\cr
    #'  shiny element (module instance) id;
    #'  the UI for this class contains simple message stating that it is not supported
    #' @param parent_id (`character(1)`) id of the `FilterStates` card container
    ui = function(id, parent_id = "cards") {
      ns <- NS(id)

      tags$div(
        id = id,
        class = "panel filter-card",
        include_js_files("count-bar-labels.js"),
        tags$div(
          class = "filter-card-header",
          tags$div(
            # header properties
            class = "filter-card-title",
            `data-toggle` = "collapse",
            `data-bs-toggle` = "collapse",
            href = paste0("#", ns("body")),
            # header elements
            if (private$is_locked()) icon("lock") else NULL,
            if (private$is_fixed()) icon("burst") else NULL,
            tags$span(tags$strong(private$get_varname())),
            tags$span(private$get_varlabel(), class = "filter-card-varlabel")
          ),
          div(
            class = "filter-card-controls",
            if (isFALSE(private$is_fixed())) {
              actionLink(
                inputId = ns("back"),
                label = NULL,
                icon = icon("circle-arrow-left", lib = "font-awesome"),
                class = "filter-card-back",
                style = "display: none"
              )
            },
            if (isFALSE(private$is_fixed())) {
              actionLink(
                inputId = ns("reset"),
                label = NULL,
                icon = icon("circle-arrow-up", lib = "font-awesome"),
                class = "filter-card-back",
                style = "display: none"
              )
            },
            if (isFALSE(private$is_locked())) {
              actionLink(
                inputId = ns("remove"),
                label = icon("circle-xmark", lib = "font-awesome"),
                class = "filter-card-remove"
              )
            }
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
    #' Destroy observers stored in `private$observers`.
    #'
    #' @return NULL invisibly
    #'
    destroy_observers = function() {
      if (!is.null(private$destroy_shiny)) {
        private$destroy_shiny()
      }
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
    destroy_shiny = NULL, # function is set in server
    # other
    is_choice_limited = FALSE, # flag whether number of possible choices was limited when specifying filter
    na_rm = FALSE,
    observers = list(), # stores observers
    state_history = NULL, # reactiveVal storing states this FilterState has had since instantiation

    # private methods ----

    ## setters for state features ----

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
    #   values are stored in `teal_slice$selected` which is reactive;
    #   value types have to be the same as `private$get_choices()`
    #
    # @return NULL invisibly
    set_selected = function(value) {
      logger::log_trace(
        sprintf(
          "%s$set_selected setting selection of id: %s",
          class(self)[1],
          private$get_id()
        )
      )
      shiny::isolate({
        value <- private$cast_and_validate(value)
        value <- private$remove_out_of_bound_values(value)
        value <- private$check_multiple(value)
        private$validate_selection(value)
        private$teal_slice$selected <- value
      })
      logger::log_trace(
        sprintf(
          "%s$set_selected selection of id: %s",
          class(self)[1],
          private$get_id()
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
    #   `private$teal_slice$keep_na`
    #
    # @return NULL invisibly
    #
    set_keep_na = function(value) {
      checkmate::assert_flag(value)
      private$teal_slice$keep_na <- value
      logger::log_trace(
        sprintf(
          "%s$set_keep_na set for filter %s to %s.",
          class(self)[1],
          private$get_id(),
          value
        )
      )
      private$set_na_rm(!value)
      invisible(NULL)
    },

    # @description
    # Set whether to keep Infs
    #
    # @param value (`logical(1)`)\cr
    #  Value(s) which come from the filter selection. Value is set in `server`
    #  modules after selecting check-box-input in the shiny interface. Values are set to
    #  `private$teal_slice$keep_inf`
    #
    set_keep_inf = function(value) {
      checkmate::assert_flag(value)
      private$teal_slice$keep_inf <- value
      logger::log_trace(
        sprintf(
          "%s$set_keep_inf of filter %s set to %s",
          class(self)[1],
          private$get_id(),
          value
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

    ## getters for state features ----

    # @description
    # Returns dataname.
    # @return `character(1)`
    get_dataname = function() {
      shiny::isolate(private$teal_slice$dataname)
    },

    # @description
    # Get variable name.
    # @return `character(1)`
    get_varname = function() {
      shiny::isolate(private$teal_slice$varname)
    },

    # @description
    # Get id of the teal_slice.
    # @return `character(1)`
    get_id = function() {
      shiny::isolate(private$teal_slice$id)
    },

    # @description
    # Get allowed values from `FilterState`.
    # @return class of the returned object depends of class of the `FilterState`
    get_choices = function() {
      shiny::isolate(private$teal_slice$choices)
    },

    # @description
    # Get selected values from `FilterState`.
    # @return class of the returned object depends of class of the `FilterState`
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
      shiny::isolate(isTRUE(private$teal_slice$fixed))
    },

    # Check whether this filter is locked (cannot be removed).
    # @return `logical(1)`
    is_locked = function() {
      shiny::isolate(isTRUE(private$teal_slice$locked))
    },

    # Check whether this filter is capable of selecting multiple values.
    # @return `logical(1)`
    is_multiple = function() {
      shiny::isolate(isTRUE(private$teal_slice$multiple))
    },

    ## other ----

    # @description
    # Returns variable label.
    # @return `character(1)`
    get_varlabel = function() {
      private$varlabel
    },

    # @description
    # Return variable name prefixed by `dataname` to be evaluated as extracted object,
    # for example `data$var`
    # @return a character string representation of a subset call
    #         that extracts the variable from the dataset
    get_varname_prefixed = function(dataname) {
      ans <-
        if (isTRUE(private$extract_type == "list")) {
          sprintf("%s$%s", dataname, private$get_varname())
        } else if (isTRUE(private$extract_type == "matrix")) {
          sprintf("%s[, \"%s\"]", dataname, private$get_varname())
        } else {
          private$get_varname()
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

    # Converts values to the type fitting this `FilterState` and validates
    # whether the elements of the resulting vector satisfy the requirements of this `FilterState`.
    # Raises error if casting does not execute successfully.
    #
    # @param values vector of values
    #
    # @return vector converted to appropriate class
    cast_and_validate = function(values) {
      values
    },

    # Filters out erroneous values from vector.
    #
    # @param values vector of values
    #
    # @return vector in which values that cannot be set in this FilterState have been dropped
    remove_out_of_bound_values = function(values) {
      values
    },

    # Checks whether multiple choices are allowed.
    # If not value is of length 2 or more, drops all but first item with a warning.
    check_multiple = function(value) {
      value
    },

    # Checks if the selection is valid in terms of class and length.
    # It should not return anything but raise an error if selection
    # has a wrong class or is outside of possible choices
    validate_selection = function(value) {
      invisible(NULL)
    },

    # @description
    # Answers the question of whether the current settings and values selected actually filters out any values.
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

    ## shiny modules -----

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
        shiny::isolate({
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
          div(
            uiOutput(ns("trigger_visible"), inline = TRUE),
            ui_input
          )
        })
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
              logger::log_trace("FilterState$keep_na_srv@1 changed reactive value, id: { private$get_id() }")
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
            logger::log_trace("FilterState$keep_na_srv@2 changed input, id: { private$get_id() }")
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
