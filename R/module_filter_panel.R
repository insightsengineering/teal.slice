#' Module for the right filter panel in the teal app
#' with a filter overview panel and a filter variable panel.
#'
#' This panel contains info about the number of observations left in
#' the (active) datasets and allows to filter the datasets.
#'
#' @param id (`character(1)`)\cr
#'   module id
ui_filter_panel = function(id, x) {
  ns <- NS(id)
  div(
    id = ns(NULL), # used for hiding / showing
    include_css_files(pattern = "filter-panel"),
    div(
      id = ns("switch-button"),
      class = "flex justify-content-right",
      div(
        title = "Enable/Disable filtering",
        shinyWidgets::prettySwitch(
          ns("filter_panel_active"),
          label = "",
          status = "success",
          fill = TRUE,
          value = TRUE,
          inline = FALSE,
          width = 30
        )
      )
    ),
    ui_overview_FilteredData(ns("filters_info")),
    ui_active_FilteredData(ns("active_filters"), datanames = x$datanames()),
    ui_add_FilteredData(ns("add_filters"), x) # todo: replace x with datanames
  )
}

#' Server function for filter panel
#'
#' @param id (`character(1)`)\cr
#'   an ID string that corresponds with the ID used to call the module's UI function.
#' @param active_datanames `function / reactive` returning datanames that
#'   should be shown on the filter panel,
#'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
#'   if the function returns `NULL` (as opposed to `character(0)`), the filter
#'   panel will be hidden
#' @return `moduleServer` function which returns `NULL`
srv_filter_panel <- function(id, active_datanames = function() "all", x) {
  stopifnot(is.function(active_datanames))
  checkmate::assert_class(x, "FilteredData")
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace("FilteredData$srv_filter_panel initializing")
      datanames <- eventReactive(req(active_datanames()), {
        x$handle_active_datanames(active_datanames())
      })

      srv_overview_FilteredData(id = "filters_info", active_datanames = datanames, x = x)
      srv_active_FilteredData("active_filters", active_datanames = datanames, x = x)
      srv_add.FilteredData("add_filters", active_datanames = datanames, x = x)

      observeEvent(
        eventExpr = input$filter_panel_active,
        ignoreNULL = TRUE,
        handlerExpr = {
          if (isTRUE(input$filter_panel_active)) {
            x$filter_panel_enable()
            logger::log_trace("Enable the Filtered Panel with the filter_panel_enable method")
          } else {
            x$filter_panel_disable()
            logger::log_trace("Disable the Filtered Panel with the filter_panel_enable method")
          }
        }
      )

      logger::log_trace("FilteredData$srv_filter_panel initialized")
      NULL
    }
  )
}
