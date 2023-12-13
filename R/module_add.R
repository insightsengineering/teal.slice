#' Add a filter state module
#'
#' @description
#' Server module responsible for displaying drop-downs with variables to add a filter.
#' @name module_add
#' @param id module id
#' @param data data to be filtered
#' @param filtered_data `FilteredData` object
#' @param dataname name of the dataset or its subitem.
#' @param ... (optional) additional arguments passed to `teal_slice`. Usefull when reusing
#'  `srv_add` for object nested inside of other object. For example `SummarizedExperiment` could be
#'  a standalone dataset or a part of `MultiAssayExperiment`. In both cases added `teal_slice` should
#' be configured in a different way.
NULL

#' @rdname module_add
#' @export
ui_add <- function(id, data, dataname) {
  UseMethod("ui_add", data)
}

#' @rdname module_add
#' @export
ui_add.default <- function(id, data, dataname) {
  tagList(
    tags$label("Add", tags$code(dataname), "filter"),
    div(sprintf("'%s' not supported", class(data)[1]))
  )
}

#' @rdname module_add
#' @export
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

#' @rdname module_add
#' @export
ui_add.MultiAssayExperiment <- function(id, data, dataname) {
  ns <- NS(id)
  tagList(
    ui_add.data.frame(ns(dataname), SummarizedExperiment::colData(data), dataname = "subjects"),
    lapply(
      names(data),
      function(experiment) {
        tagList(
          HTML("&#9658;"),
          ui_add(ns(experiment), data[[experiment]], dataname = experiment)
        )
      }
    )
  )
}

#' @rdname module_add
#' @export
ui_add.SummarizedExperiment <- function(id, data, dataname) {
  ns <- NS(id)
  row_input <- if (ncol(SummarizedExperiment::rowData(data)) == 0) {
    div("no sample variables available")
  } else if (nrow(SummarizedExperiment::rowData(data)) == 0) {
    div("no samples available")
  } else {
    uiOutput(ns("row_to_add_ui"))
  }

  col_input <- if (ncol(SummarizedExperiment::colData(data)) == 0) {
    div("no sample variables available")
  } else if (nrow(SummarizedExperiment::colData(data)) == 0) {
    div("no samples available")
  } else {
    # teal.widgets::optionalSelectInput(
    #   ns("col_to_add"),
    #   choices = NULL,
    #   options = shinyWidgets::pickerOptions(
    #     liveSearch = TRUE,
    #     noneSelectedText = "Select sample variable"
    #   )
    # )
    uiOutput(ns("col_to_add_ui"))
  }

  tagList(
    tags$label("Add", tags$code(dataname), "filter"),
    div(row_input, col_input)
  )
}


#' @rdname module_add
#' @export
srv_add <- function(id, data, filtered_data, dataname, ...) {
  UseMethod("srv_add", data)
}

#' @rdname module_add
#' @export
srv_add.default <- function(id, data, filtered_data, dataname, ...) {
  logger::log_trace("srv_add.default initializing")
  NULL
}

#' @rdname module_add
#' @export
srv_add.data.frame <- function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add.data.frame initializing")

    available_choices <- reactive({
      dataset_current_states <- Filter(
        function(slice) slice$dataname == list(...)$dataname,
        filtered_data$get_filter_state()
      )
      selected_variables <- sapply(dataset_current_states, function(slice) slice$variable)
      choices <- setdiff(colnames(data), selected_variables)
    })

    output$add_filter <- renderUI({
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
      logger::log_trace("srv_add.data.frame@2 observeEvent adding filter for dataname: { id } - { input$var_to_add }")
      filtered_data$set_filter_state(
        teal_slices(
          teal_slice(dataname, varname = input$var_to_add, ...)
        )
      )
    })
  })
}

#' @rdname module_add
#' @export
srv_add.MultiAssayExperiment <- function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    srv_add.data.frame(id, SummarizedExperiment::colData(data), filtered_data)
    lapply(
      names(data),
      function(experiment) {
        srv_add(
          id = experiment,
          data = data[[experiment]],
          filtered_data = filtered_data,
          dataname = dataname,
          experiment = experiment,
          ...
        )
      }
    )
  })
}

#' @rdname module_add
#' @export
srv_add.SummarizedExperiment <- function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    row_data <- SummarizedExperiment::rowData(data)
    col_data <- SummarizedExperiment::colData(data)

    avail_row_data_choices <- reactive({
      slices_for_subset <- Filter(function(x) x$arg == "subset", filtered_data$get_filter_state())
      active_filter_row_vars <- unique(unlist(lapply(slices_for_subset, "[[", "varname")))

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

    avail_col_data_choices <- reactive({
      slices_for_select <- Filter(function(x) x$arg == "select", filtered_data$get_filter_state())
      active_filter_col_vars <- unique(unlist(lapply(slices_for_select, "[[", "varname")))

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


    output$row_to_add_ui <- renderUI({
      logger::log_trace("srv_add.SummarizedExperiment@1 renderUI rerendering { id } row selector")
      if (length(avail_row_data_choices()) == 0) {
        span("No available row variables to add.")
      } else {
        div(
          teal.widgets::optionalSelectInput(
            session$ns("row_to_add"),
            choices = avail_row_data_choices(),
            selected = NULL,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select row variable to filter"
            )
          )
        )
      }
    })

    output$col_to_add_ui <- renderUI({
      logger::log_trace("srv_add.SummarizedExperiment@2 renderUI rerendering { id } column selector")
      if (length(avail_col_data_choices()) == 0) {
        span("No available column variables to add.")
      } else {
        div(
          teal.widgets::optionalSelectInput(
            session$ns("col_to_add"),
            choices = avail_col_data_choices(),
            selected = NULL,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select column variable to filter"
            )
          )
        )
      }
    })

    observeEvent(
      eventExpr = input$col_to_add,
      handlerExpr = {
        logger::log_trace(
          "SEFilterStates$srv_add@3 adding FilterState of column { input$col_to_add } to col data, dataname: { id }"
        )
        varname <- input$col_to_add
        filtered_data$set_filter_state(
          teal_slices(
            teal_slice(dataname = dataname, varname = varname, arg = "select", ...)
          )
        )
      }
    )


    observeEvent(
      eventExpr = input$row_to_add,
      handlerExpr = {
        logger::log_trace(
          "srv_add.SummarizedExp@4 adding FilterState of variable { input$row_to_add } to row data, dataname: { id }"
        )
        varname <- input$row_to_add
        # todo: dataname and experiment is needed
        filtered_data$set_filter_state(teal_slices(
          teal_slice(dataname = dataname, varname = varname, arg = "subset", ...)
        ))

        logger::log_trace(
          "SEFilterStates$srv_add@4 added FilterState of variable { varname } to row data, dataname: {id}"
        )
      }
    )
  })
}
