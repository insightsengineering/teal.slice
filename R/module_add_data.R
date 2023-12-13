#' @description
#' Server module responsible for displaying drop-downs with variables to add a filter.
#' @name module_add_data
#' @param id (`character(1)`)\cr
#'   an ID string that corresponds with the ID used to call the module's UI function.
#' @param active_datanames (`reactive`)\cr
#'   defining subset of `self$datanames()` to be displayed.
#' @return `shiny.tag` or `moduleServer` returning `NULL`
NULL

#' @rdname module_add_data
#' @keywords internal
ui_add_data <- function(id, filtered_data) {
  ns <- NS(id)
  div(
    id = id, # not used, can be used to customize CSS behavior
    class = "well",
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-9",
        tags$label("Add Filter Variables", class = "text-primary mb-4")
      ),
      tags$div(
        class = "col-sm-3",
        actionLink(
          ns("minimise_filter_add_vars"),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        )
      )
    ),
    div(
      id = ns("filter_add_vars_contents"),
      tagList(
        lapply(
          filtered_data$datanames(),
          function(dataname) {
            data <- filtered_data$get_data(dataname, filtered = FALSE)
            ui_add(ns(dataname), data, dataname = dataname)
          }
        )
      )
    )
  )
}

#' @rdname module_add_data
#' @keywords internal
srv_add_data <- function(id, filtered_data, active_datanames = reactive(filtered_data$datanames())) {
  checkmate::assert_class(active_datanames, "reactive")
  moduleServer(id, function(input, output, session) {
    logger::log_trace("FilteredData$srv_add initializing")
    shiny::observeEvent(input$minimise_filter_add_vars, {
      shinyjs::toggle("filter_add_vars_contents")
      toggle_icon(session$ns("minimise_filter_add_vars"), c("fa-angle-right", "fa-angle-down"))
      toggle_title(session$ns("minimise_filter_add_vars"), c("Restore panel", "Minimise Panel"))
    })

    observeEvent(active_datanames(), {
      lapply(filtered_data$datanames(), function(dataname) {
        if (dataname %in% active_datanames()) {
          shinyjs::show(dataname)
        } else {
          shinyjs::hide(dataname)
        }
      })
    })

    # should not use for-loop as variables are otherwise only bound by reference
    # and last dataname would be used
    lapply(
      filtered_data$datanames(),
      function(dataname) {
        data <- filtered_data$get_data(dataname, filtered = FALSE)
        srv_add(session$ns(dataname), data, filtered_data = filtered_data)
      }
    )
    NULL
  })
}
