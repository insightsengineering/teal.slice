#' Filter panel module for active datasets' filters
#'
#' @description
#' Server module responsible for displaying active filters for all datasets.
#' @name module_active_data
#' @inheritParams module_filter_panel
#' @param filtered_data (`FilteredData`)\cr.
#' @param dataname (`character(1)`)\cr
#'  name of the dataset to be displayed
#' @return `shiny.tag` or `moduleServer`
NULL

#' @rdname module_active_data
#' @keywords internal
ui_active_data <- function(id, filtered_data) {
  ns <- NS(id)
  div(
    id = id, # not used, can be used to customize CSS behavior
    class = "well",
    tags$div(
      class = "filter-panel-active-header",
      tags$span("Active Filter Variables", class = "text-primary mb-4"),
      ui_available_filters(id = ns("available_filters"), filtered_data = filtered_data),
      actionLink(
        inputId = ns("minimise_filter_active"),
        label = NULL,
        icon = icon("angle-down", lib = "font-awesome"),
        title = "Minimise panel",
        class = "remove_all pull-right"
      ),
      actionLink(
        inputId = ns("remove_all_filters"),
        label = "",
        icon("circle-xmark", lib = "font-awesome"),
        title = "Remove active filters",
        class = "remove_all pull-right"
      )
    ),
    div(
      id = ns("filter_active_vars_contents"),
      tagList(
        lapply(
          filtered_data$datanames(), # this is reactive - any change in datanames will rerender filter-panel
          function(dataname) {
            data <- filtered_data$get_data(dataname, filtered = TRUE)
            ui_active_dataset(ns(dataname), data = data, dataname = dataname)
          }
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("filters_active_count"),
        textOutput(ns("teal_filters_count"))
      )
    )
  )
}

#' @rdname module_active_data
#' @keywords internal
srv_active_data <- function(id, filtered_data, active_datanames = filtered_data$datanames) {
  checkmate::assert_function(active_datanames)
  shiny::moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_active_data initializing")
    srv_available_filters("available_filters", filtered_data)

    observeEvent(input$minimise_filter_active, {
      logger::log_trace("FilteredData$srv_active@1 minimalize/maximize ui")
      shinyjs::toggle("filter_active_vars_contents")
      shinyjs::toggle("filters_active_count")
      toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"))
      toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"))
    })

    observeEvent(filtered_data$get_filter_state(), {
      logger::log_trace("FilteredData$srv_active@2 toggle ui on filters count")
      shinyjs::toggle("remove_all_filters", condition = length(filtered_data$get_filter_state()) != 0)
      shinyjs::show("filter_active_vars_contents")
      shinyjs::hide("filters_active_count")
      toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"), TRUE)
      toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"), TRUE)
    })

    observeEvent(active_datanames(), {
      logger::log_trace("FilteredData$srv_active@3 toggle datanames according to active_datanames()")
      lapply(filtered_data$datanames(), function(dataname) {
        if (dataname %in% active_datanames()) {
          shinyjs::show(dataname)
        } else {
          shinyjs::hide(dataname)
        }
      })
    })

    output$teal_filters_count <- shiny::renderText({
      logger::log_trace("FilteredData$srv_active@4 rerender filter count text")
      n_filters_active <- length(filtered_data$get_filter_state())
      shiny::req(n_filters_active > 0L)
      sprintf(
        "%s filter%s applied across datasets",
        n_filters_active,
        ifelse(n_filters_active == 1, "", "s")
      )
    })

    observeEvent(input$remove_all_filters, {
      logger::log_trace("FilteredData$srv_filter_panel@5 removing all non-anchored filters")
      filtered_data$clear_filter_states()
    })

    # should be reactive to change of datanames()
    current_datanames <- reactive(filtered_data$datanames())
    previous_datanames <- reactiveVal(NULL) # FilterState list
    added_datanames <- reactiveVal(NULL) # FilterState list

    lapply(
      filtered_data$datanames(),
      function(dataname) {
        srv_active_dataset(id = dataname, filtered_data = filtered_data)
      }
    )

    NULL
  })
}
