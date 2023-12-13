ui_add <- function(id, data, dataname) {
  UseMethod("ui_add", data)
}

ui_add.default <- function(id, data, dataname) {
  span(sprintf("'%s' not supported", class(data)[1]))
}

ui_add.data.frame <- function(id, data, dataname) {
  ns <- NS(id)
  tagList(
    tags$label("Add", tags$code(dataname), "filter"),
    if (ncol(data) == 0) {
      div("no sample variables available")
    } else if (nrow(data) == 0) {
      div("no samples available")
    } else {
      uiOutput(ns("add_filter"))
    }
  )
}

ui_add.MultiAssayExperiment <- function(id, data, dataname) {
  # todo: add experiment selector
}

srv_add <- function(id, data, filtered_data) {
  UseMethod("srv_add", data)
}

srv_add.default <- function(id, data, filtered_data) {
  logger::log_trace("srv_add.default initializing")
  NULL
}

srv_add.data.frame <- function(id, data, filtered_data) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add.data.frame initializing")

    available_choices <- reactive({
      browser()
      dataset_current_states <- Filter(
        function(slice) slice$dataname == id,
        filtered_data$get_filter_state()
      )
      selected_variables <- lapply(dataset_current_states, function(state) slice$variable)
      choices <- setdiff(colnames(data), selected_variables)
    })

    output$add_filter <- renderUI({
      browser()
      logger::log_trace("srv_add.data.frame@1 renderUI rerendering { id } column selector")
      if (length(available_choices()) == 0) {
        span("No available columns to add.")
      } else {
        div(
          teal.widgets::optionalSelectInput(
            session$ns("var_to_add"),
            choices = available_choices(),
            selected = NULL,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select variable to filter"
            )
          )
        )
      }
    })

    observeEvent(input$var_to_add, {
      browser()
      logger::log_trace("srv_add.data.frame@2 observeEvent adding filter for dataname: { id } - { input$var_to_add }")
      filtered_data$set_filter_state(
        teal_slices(
          teal_slice(dataname = id, varname = input$var_to_add)
        )
      )
    })
  })
}

srv_add.MultiAssayExperiment <- function(id, data, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # todo:
  })
}
