#' Filter panel module for active dataset's filters
#'
#' @description
#' Server module responsible for displaying active filters for a single dataset.
#' @name module_active_dataset
#' @inheritParams module_filter_panel
#' @param filtered_data (`FilteredData`)\cr.
#' @param dataname (`character(1)`)\cr
#'  name of the dataset to be displayed
NULL

#' @rdname module_active_dataset
#' @keywords internal
ui_active_dataset <- function(id, data, dataname) {
  ns <- NS(id)
  span(
    id = id,
    include_css_files("filter-panel"),
    div(
      id = ns("whole_ui"), # to hide it entirely
      fluidRow(
        column(
          width = 8,
          div(
            tags$span(dataname, class = "filter_panel_dataname"),
            tags$span(attr(data, "label"), class = "filter_panel_datalabel") # comment: this is new feature
          )
        ),
        column(
          width = 4,
          tagList(
            actionLink(
              ns("remove_filters"),
              label = "",
              icon = icon("circle-xmark", lib = "font-awesome"),
              class = "remove pull-right"
            ),
            actionLink(
              ns("collapse"),
              label = "",
              icon = icon("angle-down", lib = "font-awesome"),
              class = "remove pull-right"
            )
          )
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("filter_count_ui"),
          tagList(
            textOutput(ns("filter_count")),
            br()
          )
        )
      ),
      div(
        # id needed to insert and remove UI to filter single variable as needed
        # it is currently also used by the above module to entirely hide this panel
        id = ns("filters"),
        class = "parent-hideable-list-group",
        ui_active(ns("filters"), data = data)
      )
    )
  )
}

#' @rdname module_active_dataset
#' @keywords internal
srv_active_dataset <- function(id, filtered_data) {
  # id is a dataname
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_active_dataset initializing, dataname: { id }")
    get_dataset_states <- reactive({
      Filter(
        function(state) state$get_state()$dataname %in% id,
        filtered_data$state_list_get()
      )
    })

    output$filter_count <- renderText(
      sprintf(
        "%d filter%s applied",
        length(get_dataset_states()), if (length(get_dataset_states()) != 1) "s" else ""
      )
    )

    shiny::observeEvent(get_dataset_states(), {
      shinyjs::hide("filter_count_ui")
      shinyjs::show("filters")
      shinyjs::toggle("remove_filters", condition = length(get_dataset_states()) != 0)
      shinyjs::toggle("collapse", condition = length(get_dataset_states()) != 0)
    })

    shiny::observeEvent(input$collapse, {
      shinyjs::toggle("filter_count_ui")
      shinyjs::toggle("filters")
      toggle_icon(session$ns("collapse"), c("fa-angle-right", "fa-angle-down"))
    })

    observeEvent(input$remove_filters, {
      logger::log_trace("FilteredDataset$srv_active@1 removing all non-anchored filters, dataname: { id }")
      slices_to_remove <- lapply(get_dataset_states(), function(state) state$get_state())
      slices <- do.call(teal_slices, slices_to_remove)
      filtered_data$remove_filter_state(slices)
    })

    data <- isolate(filtered_data$get_data(id, filtered = FALSE))
    srv_active("filters", data, get_dataset_states, filtered_data$remove_filter_state)
  })
}
