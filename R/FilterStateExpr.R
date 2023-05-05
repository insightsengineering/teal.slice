
#' @name FilterStateExpr
#' @docType class
#'
#'
#' @title `FilterStateExpr`  Class
#'
#' @description
#' elo
#'
#' @details
#' This class is responsible
#' @keywords internal
FilterStateExpr <- R6::R6Class( # nolint
  classname = "FilterStateExpr",

  # public methods ----
      #' @description
    #' Initialize a `FilterStateExpr` object
    #' @param id (`character(1)`)\cr
    #'   identifier of the filter
    #' @param title (`reactive`)\cr
    #'   title of the filter
    #' @param dataname (`character(1)`)\cr
    #'   name of the dataset where `expr` could be executed on.
    #' @param expr (`language`)\cr
    #'   logical expression written in executable way. By "executable" means
    #'   that `subset` call should be able to evaluate this without failure. For
    #'   example `MultiAssayExperiment::subsetByColData` requires varnames prefixed
    #'   by dataname (e.g. `data$var1 == "x" & data$var2 > 0`). For `data.frame` call
    #'   can be written without prefixing `var1 == "x" & var2 > 0`.
    #' @param disabled (`logical(1)`)\cr
    #'   flag specifying whether the `FilterState` is initiated disabled
    #' @param ... additional arguments to be saved as a list in `private$extras` field
  public = list(
    initialize = function(id, title, dataname, expr, disabled, ...) {
      private$id <- id
      private$title <- title
      private$dataname <- dataname
      private$expr <- expr
      private$disabled <- reactiveVal(disabled)
      private$extras <- list(...)
      invisible(self)

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
    format = function(indent = 2L, wrap_width = 76L) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0L)
      checkmate::assert_number(wrap_width, finite = TRUE, lower = 30L, upper = 120L)

      # List all selected values separated by commas.
      paste(
        strwrap(
          sprintf("Filter expr: %s", deparse1(private$expr)),
          width = wrap_width,
          indent = indent
        ),
        collapse = "\n")
    },

    #' @description
    #' Returns filtering state.
    #'
    #' @return A `teal_slice` object.
    #'
    get_state = function() {
      states <- append(
        list(
          id = private$id,
          title = private$title,
          dataname = private$dataname,
          expr = private$expr,
          disable = private$disabled()
        ),
        private$extras
      )
      do.call(filter_expr, states)
    },

    #' @description
    #' Sets filtering state.
    #'
    #' @param state a `teal_slice` object
    #'
    #' @return `self` invisibly
    #'
    set_state = function(state) {
      checkmate::assert_class(state, "teal_slice_expr")
      if (isTRUE(state$disabled) && isFALSE(private$is_disabled())) private$disable()
      if (isFALSE(state$disabled) && isTRUE(private$is_disabled())) private$enable()

    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
    #' @return `language`
    get_call = function() {
      private$expr
    },

    #' @description
    #' Answers the question of whether the current settings and values selected actually filters out any values.
    #' `FALSE` only when disabled.
    #' @return logical scalar
    is_any_filtered = function() {
      if (isTRUE(private$is_disabled())) {
        FALSE
      } else {
        TRUE
      }
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
          summary <- private$server_summary("summary")
          private$observers$enable <- observeEvent(input$enable,
            {
              if (isTRUE(input$enable)) {
                private$enable()
              } else {
                private$disable()
              }
            },
            ignoreInit = TRUE
          )
          out <- reactive(input$remove) # back to parent to remove self
          out
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
            tags$span(tags$strong(private$id)),
            if (length(private$title)) {
              tags$span(private$title, class = "filter-card-varlabel")
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
            private$ui_summary("summary")
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
  private = list(
    dataname = character(0),
    disabled = NULL,
    expr = NULL,
    extras = list(),
    id = character(0),
    observers = list(),
    title = character(0),

    # Disables `FilterState`
    # `state` is moved to cache and set to `NULL`
    # @return `NULL` invisibly
    disable = function() {
      private$disabled(TRUE)
      invisible(NULL)
    },

    # Enables `FilterState`
    # Cached `state` is reset again and cache is cleared.
    # @return `NULL` invisibly
    enable = function() {
      private$disabled(FALSE)
      invisible(NULL)
    },

    # Check whether this filter is disabled
    # @return `logical(1)`
    is_disabled = function() {
      if (shiny::isRunning()) {
        isTRUE(private$disabled())
      } else {
        shiny::isolate(isTRUE(private$disabled()))
      }
    },
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
    content_summary = function() {
      deparse1(private$expr)
    }
  )
)


is_predicate <-  function(x) {
  is.call(x) && list(x[[1]]) %in% lapply(c(">=", "<=", "==", "<", ">", "%in%", "!="), as.symbol)
}

get_varnames <- function(x) {
  if (is_predicate(x)) {
    if (is.name(x[[2]])) {
      x[[2]]
    } else if (is.name(x[[3]])) {
      x[[3]]
    }
  } else {
    lapply(as.list(x), get_varnames)
  }
}
