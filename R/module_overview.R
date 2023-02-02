#' Creates the UI for the module showing counts for each dataset
#' contrasting the filtered to the full unfiltered dataset
#'
#' Per dataset, it displays
#' the number of rows/observations in each dataset,
#' the number of unique subjects.
#'
#' @param id module id
ui_overview_FilteredData <- function(id) {
  ns <- NS(id)
  div(
    id = ns("container"), # not used, can be used to customize CSS behavior
    class = "well",
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-9",
        tags$label("Active Filter Summary", class = "text-primary mb-4")
      ),
      tags$div(
        class = "col-sm-3",
        actionLink(
          ns("minimise_filter_overview"),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        )
      )
    ),
    tags$br(),
    div(
      id = ns("filters_overview_contents"),
      div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table"))
      )
    )
  )
}

#' Server function to display the number of records in the filtered and unfiltered
#' data
#'
#' @param id (`character(1)`)\cr
#'   an ID string that corresponds with the ID used to call the module's UI function.
#' @param active_datanames (`function`, `reactive`)\cr
#'   returning datanames that should be shown on the filter panel,
#'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
#'   if the function returns `NULL` (as opposed to `character(0)`), the filter
#'   panel will be hidden.
#' @return `moduleServer` function which returns `NULL`
srv_overview_FilteredData <- function(id, x, active_datanames = function() "all") {
  stopifnot(is.function(active_datanames))
  checkmate::assert_class(x, "FilteredData")
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace("FilteredData$srv_overview_FilteredData initializing")

      shiny::observeEvent(input$minimise_filter_overview, {
        shinyjs::toggle("filters_overview_contents")
        toggle_icon(session$ns("minimise_filter_overview"), c("fa-angle-right", "fa-angle-down"))
        toggle_title(session$ns("minimise_filter_overview"), c("Restore panel", "Minimise Panel"))
      })

      output$table <- renderUI({
        logger::log_trace("FilteredData$srv_overview_FilteredData@1 updating counts")
        datanames <- active_datanames()
        if (length(datanames) == 0) {
          return(NULL)
        }

        datasets_df <- x$get_filter_overview(datanames = datanames)

        body_html <- lapply(
          seq_len(nrow(datasets_df)),
          function(row_idx) {
            tags$tr(
              tags$td(rownames(datasets_df)[row_idx]),
              tags$td(datasets_df[row_idx, 1]),
              tags$td(datasets_df[row_idx, 2])
            )
          }
        )

        header_html <- tags$tr(
          tags$td(""),
          tags$td(colnames(datasets_df)[1]),
          tags$td(colnames(datasets_df)[2])
        )

        table_html <- tags$table(
          class = "table custom-table",
          tags$thead(header_html),
          tags$tbody(body_html)
        )
        logger::log_trace("FilteredData$srv_overview_FilteredData@1 updated counts")
        table_html
      })

      shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
      logger::log_trace("FilteredData$srv_overview_FilteredData initialized")
      NULL
    }
  )
}