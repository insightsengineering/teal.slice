#' Add new filter state module
#'
#' Add new filter state module
#' @rdname module_add
#' @param id (`character(1)`)
#' @param x (`FilteredData`, `FilteredDataset`, `FilterStates`)
#' @keywords internal
NULL

#' @rdname module_add
srv_add <- function(id, x, active_datanames) {
  UseMethod("srv_add", x)
}

#' @rdname module_add
ui_add_FilteredData <- function(id, x) {
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
          x$datanames(),
          function(dataname) {
            fdataset <- x$get_filtered_dataset(dataname)
            span(id = ns(dataname), ui_add_FilteredDataset(ns(dataname), x = fdataset))
          }
        )
      )
    )
  )
}

#' @rdname module_add
srv_add.FilteredData <- function(id, x, active_datanames) {
  stopifnot(is.function(active_datanames))
  moduleServer(id = id, function(input, output, session) {
    shiny::observeEvent(input$minimise_filter_add_vars, {
      shinyjs::toggle("filter_add_vars_contents")
      toggle_icon(session$ns("minimise_filter_add_vars"), c("fa-angle-right", "fa-angle-down"))
      toggle_title(session$ns("minimise_filter_add_vars"), c("Restore panel", "Minimise Panel"))
    })

    # use isolate because we assume that the number of datasets does not change
    # over the course of the teal app
    # alternatively, one can proceed as in modules_filter_items to dynamically insert, remove UIs
    isol_datanames <- isolate(x$datanames()) # they are already ordered
    # should not use for-loop as variables are otherwise only bound by reference
    # and last dataname would be used

    lapply(isol_datanames, function(dataname) {
      fdataset <- x$get_filtered_dataset(dataname)
      srv_add(id = dataname, x = fdataset)
    })

    # hide and show datanames cards
    observeEvent(active_datanames(), {
      lapply(isol_datanames, function(dataname) {
        if (dataname %in% active_datanames()) {
          shinyjs::show(dataname)
        } else {
          shinyjs::hide(dataname)
        }
      })
    })
  })
}

#' @rdname module_add
ui_add_FilteredDataset <- function(id, x) {
  ns <- NS(id)
  tagList(
    tags$label("Add", tags$code(x$get_dataname()), "filter"),
    br(),
    HTML("&#9658;"),
    uiOutput(ns("states_ui"))
  )
}

#' @rdname module_add
srv_add.FilteredDataset <- function(id, x) {
  dataname <- deparse1(x$get_dataname())
  fs <- x$get_filter_states()
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace(
        "FilteredDataset$srv_add_filter_state initializing, dataname: { dataname }"
      )

      # rendered once on init
      output$states_ui <- renderUI({
        tagList(
          lapply(names(fs), function(fs_name) {
            ui_add_FilterStates(id = session$ns(fs_name))
          })
        )
      })

      lapply(names(fs), function(fs_name) {
        srv_add(id = fs_name, x = fs[[fs_name]])
      })

      logger::log_trace(
        "FilteredDataset$srv_add_filter_state initialized, dataname: { dataname }"
      )
      NULL
    }
  )
}

#' @rdname module_add
ui_add_FilterStates <- function(id) {
  checkmate::assert_string(id)
  ns <- NS(id)
  uiOutput(ns("add_state"))
}

#' @rdname module_add
srv_add.DFFilterStates <- function(id, x) {
  data <- x$get_data()
  vars_include <- x$get_filterable_varnames()
  dataname <- deparse1(x$get_dataname())
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace("DFFilterStates$srv_add_filter_state initializing, dataname: { dataname }")

      # available choices to display
      avail_column_choices <- reactive({
        active_filter_vars <- vapply(
          X = x$state_list_get(state_list_index = 1L),
          FUN.VALUE = character(1),
          FUN = function(x) x$get_varname(deparse = TRUE)
        )

        choices <- setdiff(vars_include, active_filter_vars)

        data_choices_labeled(
          data = data,
          choices = choices,
          varlabels = x$get_varlabels(choices),
          keys = x$get_keys()
        )
      })

      output$add_state <- renderUI({
        logger::log_trace(
          "DFFilterStates$srv_add_filter_state@1 updating available column choices, dataname: { dataname }"
        )
        choices <- avail_column_choices()
        out <- if (length(choices) > 0) {
          teal.widgets::optionalSelectInput(
            session$ns("var_to_add"),
            choices = choices,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select variable to filter"
            )
          )
        } else {
          span("No columns available")
        }
      })

      observeEvent(
        eventExpr = input$var_to_add,
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        handlerExpr = {
          logger::log_trace(
            sprintf(
              "DFFilterStates$srv_add_filter_state@2 adding FilterState of variable %s, dataname: %s",
              input$var_to_add,
              dataname
            )
          )
          varname <- input$var_to_add
          x$set_filter_state(state = setNames(list(list()), varname))
          logger::log_trace(
            sprintf(
              "DFFilterStates$srv_add_filter_state@2 added FilterState of variable %s, dataname: %s",
              varname,
              dataname
            )
          )
        }
      )

      logger::log_trace("DFFilterStates$srv_add_filter_state initialized, dataname: { dataname }")
      NULL
    }
  )
}

#' @rdname module_add
srv_add.MAEFilterStates = function(id, x) {
  col_data <- SummarizedExperiment::colData(x$get_data())
  dataname <- deparse1(x$get_dataname())
  supported_varnames <- get_supported_filter_varnames(data = col_data)

  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace(
        "MAEFilterState$srv_add_filter_state initializing, dataname: { dataname }"
      )

      # available choices to display
      avail_column_choices <- reactive({
        active_filter_vars <- vapply(
          X = x$state_list_get("y"),
          FUN.VALUE = character(1),
          FUN = function(x) x$get_varname(deparse = TRUE)
        )
        choices <- setdiff(supported_varnames, active_filter_vars)
        data_choices_labeled(
          data = col_data,
          choices = choices,
          varlabels = x$get_varlabels(choices)
        )
      })

      output$add_state <- renderUI({
        logger::log_trace(
          "DFFilterStates$srv_add_filter_state@1 updating available column choices, dataname: { dataname }"
        )
        choices <- avail_column_choices()
        out <- if (length(choices) > 0) {
          teal.widgets::optionalSelectInput(
            session$ns("var_to_add"),
            choices = choices,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select variable to filter"
            )
          )
        } else {
          span("No columns available.")
        }
      })

      observeEvent(
        eventExpr = input$var_to_add,
        handlerExpr = {
          logger::log_trace(
            sprintf(
              "MAEFilterStates$srv_add_filter_state@2 adding FilterState of variable %s, dataname: %s",
              input$var_to_add,
              dataname
            )
          )
          varname <- input$var_to_add
          x$set_filter_state(setNames(list(list()), varname))
          logger::log_trace(
            sprintf(
              "MAEFilterStates$srv_add_filter_state@2 added FilterState of variable %s, dataname: %s",
              deparse1(varname),
              dataname
            )
          )
        }
      )

      logger::log_trace(
        "MAEFilterState$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
      )
      NULL
    }
  )
}

srv_add.SEFilterStates = function(id, x) {
  data <- x$get_data()
  dataname <- x$get_dataname()
  row_data <- SummarizedExperiment::rowData(data)
  col_data <- SummarizedExperiment::colData(data)
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace(
        "SEFilterState$srv_add_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
      )

      # available choices to display
      avail_row_choices <- reactive({
        active_filter_row_vars <- vapply(
          X = x$state_list_get(state_list_index = "subset"),
          FUN.VALUE = character(1),
          FUN = function(x) x$get_varname(deparse = TRUE)
        )


        choices <- setdiff(
          get_supported_filter_varnames(data = row_data),
          active_filter_row_vars
        )

        data_choices_labeled(
          data = row_data,
          choices = choices,
          varlabels = character(0),
          keys = NULL
        )
      })
      avail_col_choices <- reactive({
        active_filter_col_vars <- vapply(
          X = x$state_list_get(state_list_index = "select"),
          FUN.VALUE = character(1),
          FUN = function(x) x$get_varname(deparse = TRUE)
        )

        choices <- setdiff(
          get_supported_filter_varnames(data = col_data),
          active_filter_col_vars
        )

        data_choices_labeled(
          data = col_data,
          choices = choices,
          varlabels = character(0),
          keys = NULL
        )
      })

      output$add_state <- renderUI({
        logger::log_trace(
          "DFFilterStates$srv_add_filter_state@1 updating available column choices, dataname: { dataname }"
        )
        row_choices <- avail_row_choices()
        col_choices <- avail_col_choices()

        ui_row <- if (length(row_choices)) {
          teal.widgets::optionalSelectInput(
            session$ns("row_to_add"),
            choices = row_choices,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select gene variable "
            )
          )
        }
        ui_col <- if (length(col_choices)) {
          teal.widgets::optionalSelectInput(
            session$ns("col_to_add"),
            choices = col_choices,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select sample variable"
            )
          )
        }

        div(ui_row, ui_col)
      })

      observeEvent(
        eventExpr = input$col_to_add,
        handlerExpr = {
          varname <- input$col_to_add
          logger::log_trace(
            sprintf(
              "SEFilterStates$srv_add_filter_state@3 adding FilterState of column %s to col data, dataname: %s",
              varname,
              dataname
            )
          )

          x$set_filter_state(list(select = setNames(list(list()), varname)))
          logger::log_trace(
            sprintf(
              "SEFilterStates$srv_add_filter_state@3 added FilterState of column %s to col data, dataname: %s",
              deparse1(varname),
              dataname
            )
          )
        }
      )


      observeEvent(
        eventExpr = input$row_to_add,
        handlerExpr = {
          varname <- input$row_to_add
          logger::log_trace(
            sprintf(
              "SEFilterStates$srv_add_filter_state@4 adding FilterState of variable %s to row data, dataname: %s",
              varname,
              dataname
            )
          )

          x$set_filter_state(list(subset = setNames(list(list()), varname)))
          logger::log_trace(
            sprintf(
              "SEFilterStates$srv_add_filter_state@4 added FilterState of variable %s to row data, dataname: %s",
              varname,
              dataname
            )
          )
        }
      )

      logger::log_trace(
        "SEFilterState$srv_add_filter_state initialized, dataname: { dataname }"
      )
      NULL
    }
  )
}

#' @details
#' Composes id for the FilteredDataset shiny element (add filter state)
#' @param dataname (`character(1)`)  name of the dataset which ui is composed for.
#' @return `character(1)` - `<dataname>_filter`
get_ui_add_filter_id <- function(dataname) {
  sprintf("add_%s_filter", dataname)
}