# @description
# Activate available filters.
# Module is composed from plus button and dropdown menu. Menu is shown when
# the button is clicked. Menu contains available/active filters list
# passed via `set_available_teal_slice`.
ui_available_filters <- function(id, filtered_data) {
  ns <- NS(id)
  active_slices_id <- shiny::isolate(vapply(filtered_data$get_filter_state(), `[[`, character(1), "id"))
  div(
    id = ns("available_menu"),
    shinyWidgets::dropMenu(
      actionLink(
        ns("show"),
        label = NULL,
        icon = icon("plus", lib = "font-awesome"),
        title = "Available filters",
        class = "remove pull-right"
      ),
      div(
        class = "menu-content",
        shinycssloaders::withSpinner(
          uiOutput(ns("checkbox")),
          type = 4,
          size = 0.25
        )
      )
    )
  )
}

# @description
# Activate available filters. When a filter is selected or removed,
# `set_filter_state` or `remove_filter_state` is executed for
# the appropriate filter state id.
srv_available_filters <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    slices_available <- filtered_data$get_available_teal_slices()
    slices_interactive <- reactive(
      Filter(function(slice) isFALSE(slice$fixed), slices_available())
    )
    slices_fixed <- reactive(
      Filter(function(slice) isTRUE(slice$fixed), slices_available())
    )
    available_slices_id <- reactive(vapply(slices_available(), `[[`, character(1), "id"))
    active_slices_id <- reactive(vapply(filtered_data$get_filter_state(), `[[`, character(1), "id"))
    duplicated_slice_references <- reactive({
      # slice refers to a particular column
      slice_reference <- vapply(slices_available(), get_default_slice_id, character(1))
      is_duplicated_reference <- duplicated(slice_reference) | duplicated(slice_reference, fromLast = TRUE)
      is_active <- available_slices_id() %in% active_slices_id()
      is_not_expr <- !vapply(slices_available(), inherits, logical(1), "teal_slice_expr")
      slice_reference[is_duplicated_reference & is_active & is_not_expr]
    })

    checkbox_group_element <- function(name, value, label, checked, disabled = FALSE) {
      tags$div(
        class = "checkbox available-filters",
        tags$label(
          tags$input(
            type = "checkbox",
            name = name,
            value = value,
            checked = checked,
            disabled = if (disabled) "disabled"
          ),
          tags$span(label, disabled = if (disabled) disabled)
        )
      )
    }

    output$checkbox <- renderUI({
      checkbox <- checkboxGroupInput(
        session$ns("available_slices_id"),
        label = NULL,
        choices = NULL,
        selected = NULL
      )
      active_slices_ids <- active_slices_id()
      duplicated_slice_refs <- duplicated_slice_references()

      checkbox_group_slice <- function(slice) {
        # we need to isolate changes in the fields of the slice (teal_slice)
        shiny::isolate({
          checkbox_group_element(
            name = session$ns("available_slices_id"),
            value = slice$id,
            label = slice$id,
            checked = if (slice$id %in% active_slices_ids) "checked",
            disabled = slice$anchored ||
              get_default_slice_id(slice) %in% duplicated_slice_refs &&
                !slice$id %in% active_slices_ids
          )
        })
      }

      interactive_choice_mock <- lapply(slices_interactive(), checkbox_group_slice)
      non_interactive_choice_mock <- lapply(slices_fixed(), checkbox_group_slice)

      htmltools::tagInsertChildren(
        checkbox,
        br(),
        if (length(non_interactive_choice_mock)) tags$strong("Fixed filters"),
        non_interactive_choice_mock,
        if (length(interactive_choice_mock)) tags$strong("Interactive filters"),
        interactive_choice_mock,
        .cssSelector = "div.shiny-options-group",
        after = 0
      )
    })

    observeEvent(input$available_slices_id, ignoreNULL = FALSE, ignoreInit = TRUE, {
      new_slices_id <- setdiff(input$available_slices_id, active_slices_id())
      removed_slices_id <- setdiff(active_slices_id(), input$available_slices_id)
      if (length(new_slices_id)) {
        new_teal_slices <- Filter(
          function(slice) slice$id %in% new_slices_id,
          filtered_data$get_available_teal_slices()
        )
        filtered_data$set_filter_state(new_teal_slices)
      }

      if (length(removed_slices_id)) {
        removed_teal_slices <- Filter(
          function(slice) slice$id %in% removed_slices_id,
          filtered_data$get_filter_state()
        )
        filtered_data$remove_filter_state(removed_teal_slices)
      }
    })

    observeEvent(filtered_data$get_available_teal_slices(), ignoreNULL = FALSE, {
      if (length(filtered_data$get_available_teal_slices())) {
        shinyjs::show("available_menu")
      } else {
        shinyjs::hide("available_menu")
      }
    })
  })
}
