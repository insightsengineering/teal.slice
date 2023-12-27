#' Add a filter state module
#'
#' @description
#' Server module responsible for displaying drop-downs with variables to add a filter.
#'
#' @param ... (optional) additional arguments passed to `teal_slice`. Useful when reusing
#'  `srv_add` for object nested inside of other object. For example `SummarizedExperiment` could be
#'  a standalone dataset or a part of `MultiAssayExperiment`. In both cases added `teal_slice` should
#' be configured in a different way.
#' @inheritParams filter_panel_methods
#' @inheritSection filter_panel_methods Supported data types
#'
#' @name module_add
#' @rdname module_add
#'
#' @aliases ui_add
#' @aliases ui_add-ANY-method
#' @aliases ui_add-data.frame-method
#' @aliases ui_add-DataFrame-method
#' @aliases ui_add-array-method
#' @aliases ui_add-Matrix-method
#' @aliases ui_add-SummarizedExperiment-method
#' @aliases ui_add-MultiAssayExperiment-method
#'
#' @aliases srv_add
#' @aliases srv_add-ANY-method
#' @aliases srv_add-data.frame-method
#' @aliases srv_add-DataFrame-method
#' @aliases srv_add-array-method
#' @aliases srv_add-Matrix-method
#' @aliases srv_add-SummarizedExperiment-method
#' @aliases srv_add-MultiAssayExperiment-method
#'
#' @export
#'
# ui_add generic ----
setGeneric("ui_add", function(id, data, dataname) {
  ns <- NS(id)
  tagList(
    tags$label("Add", tags$code(dataname), "filter"),
    div("Unsupported data type")
  )
  # check output somehow
  # if (!inherits(ui, c("shiny.tag", "shiny.tag.list", "character"))) {
  #   # ui class is checked in case someone register ui_add.<custom_class>
  #   stop("ui_add must return a shiny.tag or shiny.tag.list or a character")
  # }
})

## data.frame method ----
setMethod("ui_add", c(data = "data.frame"), function(id, data, dataname) {
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
})

## DataFrame method ----
setMethod("ui_add", c(data = "DataFrame"), function(id, data, dataname) {
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
})

## array method ----
setMethod("ui_add", c(data = "array"), function(id, data, dataname) {
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
})

## Matrix method ----
setMethod("ui_add", c(data = "Matrix"), function(id, data, dataname) {
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
})

## SummarizedExperiment method ----
setMethod("ui_add", c(data = "SummarizedExperiment"), function(id, data, dataname) {
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
    uiOutput(ns("col_to_add_ui"))
  }

  tagList(
    tags$label("Add", tags$code(dataname), "filter"),
    div(row_input, col_input)
  )
})

## MultiAssayExperiment method ----
setMethod("ui_add", c(data = "MultiAssayExperiment"), function(id, data, dataname) {
  ns <- NS(id)
  tagList(
    ui_add_array(ns(dataname), SummarizedExperiment::colData(data), dataname = "subjects"),
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
})



#' @export
#'
# srv_add generic ----
setGeneric("srv_add", function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add_unsupported initializing")
    NULL
  })
})

## data.frame method ----
setMethod("srv_add", c(data = "data.frame"), function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add.data.frame initializing")

    available_choices <- reactive({
      # 1. Get colnames which are allowed by the teal_slice (include/exclude)
      choices <- get_include_varnames(
        data,
        include_varnames = attr(filtered_data$get_filter_state(), "include_varnames")[[dataname]],
        exclude_varnames = attr(filtered_data$get_filter_state(), "exclude_varnames")[[dataname]]
      )

      # 2. exclude variables which are already active
      dataset_current_states <- Filter(function(x) x$dataname == dataname, filtered_data$get_filter_state())
      active_filter_vars <- lapply(dataset_current_states, function(x) x$varname)
      choices <- setdiff(choices, active_filter_vars)

      # 3. exclude variables which are duplicates of the parent colnames
      parent_dataname <- teal.data::parents(filtered_data$get_join_keys())[[dataname]]
      if (length(parent_dataname)) {
        parent_varnames <- get_supported_filter_varnames(filtered_data$get_data(parent_dataname, filtered = FALSE))
        choices <- setdiff(choices, parent_varnames)
      }

      # 4. add labels and icons to the choices
      data_choices_labeled(data = data, choices = choices, keys = filtered_data$get_join_keys()[dataname, dataname])
    })

    output$add_filter <- bindCache(
      renderUI({
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
      }),
      available_choices()
    )

    observeEvent(input$var_to_add, {
      logger::log_trace("srv_add.data.frame@2 observeEvent adding filter for dataname: { id } - { input$var_to_add }")
      filtered_data$set_filter_state(
        teal_slices(
          teal_slice(dataname, varname = input$var_to_add, ...)
        )
      )
    })
  })
})

## DataFrame method ----
setMethod("srv_add", c(data = "DataFrame"), function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add.data.frame initializing")

    available_choices <- reactive({
      # 1. Get colnames which are allowed by the teal_slice (include/exclude)
      choices <- get_include_varnames(
        data,
        include_varnames = attr(filtered_data$get_filter_state(), "include_varnames")[[dataname]],
        exclude_varnames = attr(filtered_data$get_filter_state(), "exclude_varnames")[[dataname]]
      )

      # 2. exclude variables which are already active
      dataset_current_states <- Filter(function(x) x$dataname == dataname, filtered_data$get_filter_state())
      active_filter_vars <- lapply(dataset_current_states, function(x) x$varname)
      choices <- setdiff(choices, active_filter_vars)

      # 3. exclude variables which are duplicates of the parent colnames
      parent_dataname <- teal.data::parents(filtered_data$get_join_keys())[[dataname]]
      if (length(parent_dataname)) {
        parent_varnames <- get_supported_filter_varnames(filtered_data$get_data(parent_dataname, filtered = FALSE))
        choices <- setdiff(choices, parent_varnames)
      }

      # 4. add labels and icons to the choices
      data_choices_labeled(data = data, choices = choices, keys = filtered_data$get_join_keys()[dataname, dataname])
    })

    output$add_filter <- bindCache(
      renderUI({
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
      }),
      available_choices()
    )

    observeEvent(input$var_to_add, {
      logger::log_trace("srv_add.data.frame@2 observeEvent adding filter for dataname: { id } - { input$var_to_add }")
      filtered_data$set_filter_state(
        teal_slices(
          teal_slice(dataname, varname = input$var_to_add, ...)
        )
      )
    })
  })
})

## array method ----
setMethod("srv_add", c(data = "array"), function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add.data.frame initializing")

    available_choices <- reactive({
      # 1. Get colnames which are allowed by the teal_slice (include/exclude)
      choices <- get_include_varnames(
        data,
        include_varnames = attr(filtered_data$get_filter_state(), "include_varnames")[[dataname]],
        exclude_varnames = attr(filtered_data$get_filter_state(), "exclude_varnames")[[dataname]]
      )

      # 2. exclude variables which are already active
      dataset_current_states <- Filter(function(x) x$dataname == dataname, filtered_data$get_filter_state())
      active_filter_vars <- lapply(dataset_current_states, function(x) x$varname)
      choices <- setdiff(choices, active_filter_vars)

      # 3. exclude variables which are duplicates of the parent colnames
      parent_dataname <- teal.data::parents(filtered_data$get_join_keys())[[dataname]]
      if (length(parent_dataname)) {
        parent_varnames <- get_supported_filter_varnames(filtered_data$get_data(parent_dataname, filtered = FALSE))
        choices <- setdiff(choices, parent_varnames)
      }

      # 4. add labels and icons to the choices
      data_choices_labeled(data = data, choices = choices, keys = filtered_data$get_join_keys()[dataname, dataname])
    })

    output$add_filter <- bindCache(
      renderUI({
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
      }),
      available_choices()
    )

    observeEvent(input$var_to_add, {
      logger::log_trace("srv_add.data.frame@2 observeEvent adding filter for dataname: { id } - { input$var_to_add }")
      filtered_data$set_filter_state(
        teal_slices(
          teal_slice(dataname, varname = input$var_to_add, ...)
        )
      )
    })
  })
})

## Matrix method ----
setMethod("srv_add", c(data = "Matrix"), function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_add.data.frame initializing")

    available_choices <- reactive({
      # 1. Get colnames which are allowed by the teal_slice (include/exclude)
      choices <- get_include_varnames(
        data,
        include_varnames = attr(filtered_data$get_filter_state(), "include_varnames")[[dataname]],
        exclude_varnames = attr(filtered_data$get_filter_state(), "exclude_varnames")[[dataname]]
      )

      # 2. exclude variables which are already active
      dataset_current_states <- Filter(function(x) x$dataname == dataname, filtered_data$get_filter_state())
      active_filter_vars <- lapply(dataset_current_states, function(x) x$varname)
      choices <- setdiff(choices, active_filter_vars)

      # 3. exclude variables which are duplicates of the parent colnames
      parent_dataname <- teal.data::parents(filtered_data$get_join_keys())[[dataname]]
      if (length(parent_dataname)) {
        parent_varnames <- get_supported_filter_varnames(filtered_data$get_data(parent_dataname, filtered = FALSE))
        choices <- setdiff(choices, parent_varnames)
      }

      # 4. add labels and icons to the choices
      data_choices_labeled(data = data, choices = choices, keys = filtered_data$get_join_keys()[dataname, dataname])
    })

    output$add_filter <- bindCache(
      renderUI({
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
      }),
      available_choices()
    )

    observeEvent(input$var_to_add, {
      logger::log_trace("srv_add.data.frame@2 observeEvent adding filter for dataname: { id } - { input$var_to_add }")
      filtered_data$set_filter_state(
        teal_slices(
          teal_slice(dataname, varname = input$var_to_add, ...)
        )
      )
    })
  })
})

## SummarizedExperiment method ----
setMethod("srv_add", c(data = "SummarizedExperiment"), function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    row_data <- SummarizedExperiment::rowData(data)
    col_data <- SummarizedExperiment::colData(data)

    avail_row_data_choices <- reactive({
      # 1. Get colnames which are allowed by the teal_slice (include/exclude)
      choices <- get_include_varnames(
        row_data,
        include_varnames = attr(filtered_data$get_filter_state(), "include_varnames")[[dataname]],
        exclude_varnames = attr(filtered_data$get_filter_state(), "exclude_varnames")[[dataname]]
      )

      # 2. exclude variables which are already active
      dataset_current_states <- Filter(function(x) x$arg == "subset", filtered_data$get_filter_state())
      active_filter_vars <- lapply(dataset_current_states, function(x) x$varname)
      choices <- setdiff(choices, active_filter_vars)

      # 3. add labels and icons to the choices
      data_choices_labeled(data = row_data, choices = choices)
    })

    avail_col_data_choices <- reactive({
      # 1. Get colnames which are allowed by the teal_slice (include/exclude)
      choices <- get_include_varnames(
        col_data,
        include_varnames = attr(filtered_data$get_filter_state(), "include_varnames")[[dataname]],
        exclude_varnames = attr(filtered_data$get_filter_state(), "exclude_varnames")[[dataname]]
      )

      # 2. exclude variables which are already active
      dataset_current_states <- Filter(function(x) x$arg == "select", filtered_data$get_filter_state())
      active_filter_vars <- lapply(dataset_current_states, function(x) x$varname)
      choices <- setdiff(choices, active_filter_vars)

      # 3. add labels and icons to the choices
      data_choices_labeled(data = col_data, choices = choices)
    })


    # Need to bind cache on avail_row/col_data_choices as they listen to the whole get_filter_state()
    output$row_to_add_ui <- bindCache(
      renderUI({
        logger::log_trace("srv_add.SummarizedExp@1 renderUI rerendering { id } row selector")
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
      }),
      avail_row_data_choices()
    )

    output$col_to_add_ui <- bindCache(
      renderUI({
        logger::log_trace("srv_add.SummarizedExp@2 renderUI rerendering { id } column selector")
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
      }),
      avail_col_data_choices()
    )

    observeEvent(
      eventExpr = input$col_to_add,
      handlerExpr = {
        logger::log_trace(
          "srv_add.SummarizedExp@3 adding FilterState of column { input$col_to_add } to col data, dataname: { id }"
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
        filtered_data$set_filter_state(teal_slices(
          teal_slice(dataname = dataname, varname = varname, arg = "subset", ...)
        ))

        logger::log_trace(
          "srv_add.SummarizedExp@4 added FilterState of variable { varname } to row data, dataname: {id}"
        )
      }
    )
  })
})

## MultiAssayExperiment method ----
setMethod("srv_add", c(data = "MultiAssayExperiment"), function(id, data, filtered_data, dataname, ...) {
  moduleServer(id, function(input, output, session) {
    srv_add_array(id, SummarizedExperiment::colData(data), filtered_data, dataname, ...)
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
})
