#' Filter Panel module
#'
#' @description
#' This module provides a filter panel that can be used to filter data
#' @name module_filter_panel
#' @param id (`character(1)`)\cr
#'   an ID string that corresponds with the ID used to call the module's UI function.
#' @param filtered_data (`FilteredData`)\cr
#'  object to store filter state and filtered datasets.
#' @param active_datanames `function / reactive` returning `datanames` that
#'   should be shown on the filter panel,
#'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
#'   if the function returns `NULL` (as opposed to `character(0)`), the filter
#'   panel will be hidden
#' @return `shiny.tag` and `moduleServer`
#' @export
ui_filter_panel <- function(id) {
  ns <- NS(id)
  div(
    id = ns(NULL), # used for hiding / showing
    include_css_files(pattern = "filter-panel"),
    uiOutput(ns("filter_panel"))
  )
}

#' @rdname module_filter_panel
#' @export
srv_filter_panel <- function(id, filtered_data, active_datanames = filtered_data$datanames) {
  checkmate::assert_function(active_datanames)
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace("FilteredData$srv_filter_panel initializing")

      active_datanames_resolved <- reactive({
        # todo: this can be relaxed to throw warnings instead of errors
        # filter-panel can be built if some datanames are not available.
        # inexisting datanames should be ignored in the loops for active, add and overview
        # checkmate::assert_subset(active_datanames(), filtered_data$datanames())
        active_datanames()
      })

      output$filter_panel <- renderUI({
        tagList(
          ui_overview_data(session$ns("overview"), filtered_data),
          ui_active_data(session$ns("active"), filtered_data),
          if (isTRUE(attr(filtered_data$get_filter_state(), "allow_add"))) {
            ui_add_data(session$ns("add"), filtered_data)
          }
        )
      })


      srv_overview_data("overview", filtered_data, active_datanames = active_datanames_resolved)
      srv_active_data("active", filtered_data, active_datanames = active_datanames_resolved)
      srv_add_data("add", filtered_data, active_datanames = active_datanames_resolved)
      NULL
    }
  )
}
