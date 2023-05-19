# MAEFilteredDataset ------
#' @keywords internal
#' @title `MAEFilteredDataset` R6 class
MAEFilteredDataset <- R6::R6Class( # nolint
  classname = "MAEFilteredDataset",
  inherit = FilteredDataset,

  # public methods ----
  public = list(

    #' @description
    #' Gets a filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #'
    #' @param sid (`character`)\cr
    #'  when specified then method returns code containing filter conditions of
    #'  `FilterState` objects which `"sid"` attribute is different than this `sid` argument.
    #'
    #' @return filter `call` or `list` of filter calls
    get_call = function(sid = "") {
      states <- private$state_list_get()
      subject_states <- Filter(
        function(x) inherits(x$get_state(), "teal_slice_mae_subjects"),
        states
      )
      subject_calls <- Filter(
        Negate(is.null),
        lapply(
          subject_states,
          function(state) {
            state$get_call(dataname = private$dataname)
          }
        )
      )

      subjects_call <- if (length(subject_calls)) {
        substitute(
          dataname <- MultiAssayExperiment::subsetByColData(dataname, expr),
          list(
            dataname = str2lang(private$dataname),
            expr = calls_combine_by(subject_calls, operator = "&")
          )
        )
      }

      # experiment calls
      filter_call <- subjects_call
      if (length(filter_call) == 0) {
        return(NULL)
      }
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`named list`)\cr
    #'  names of the list should correspond to the names of the initialized `FilterStates`
    #'  kept in `private$filter_states`. For this object they are `"subjects"` and
    #'  names of the experiments. Values of initial state should be relevant
    #'  to the referred column.
    #'
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #' dataset <- teal.slice:::MAEFilteredDataset$new(miniACC, "MAE")
    #' fs <- filter_settings(
    #'   filter_var(
    #'     dataname = "MAE", varname = "years_to_birth", selected = c(30, 50), keep_na = TRUE
    #'   ),
    #'   filter_var(
    #'     dataname = "MAE", varname = "vital_status", selected = "1", keep_na = FALSE
    #'   ),
    #'   filter_var(
    #'     dataname = "MAE", varname = "gender", selected = "female", keep_na = TRUE
    #'   ),
    #'   filter_var(
    #'     dataname = "MAE", varname = "ARRAY_TYPE", selected = "", keep_na = TRUE
    #'   )
    #' )
    #' dataset$set_filter_state(state = fs)
    #' shiny::isolate(dataset$get_filter_state())
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) {
        checkmate::assert_true(x$dataname == private$dataname, .var.name = "dataname matches private$dataname")
      })

      if (length(state) > 0) {
        private$set_filter_state_impl(
          state = state,
          data = private$data,
          data_reactive = private$data_filtered_fun
        )
      }


      logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")

      invisible(NULL)
    },

    #' @description
    #' Shiny server module to add filter variable.
    #'
    #' This module controls available choices to select as a filter variable.
    #' Once selected, a variable is removed from available choices.
    #' Removing a filter variable adds it back to available choices.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #'
    #' @return `moduleServer` function which returns `NULL`
    srv_add = function(id) {
       moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilterStates$srv_add initializing, dataname: { private$dataname }")

          output$inputs <- renderUI({
            data <- self$get_dataset()
            dataname <- self$get_dataname()
            experiment_names <- names(data)

            # implement labels to the choices

            browser()
            div(
              br(),
              HTML("&#9658;"),
              tags$label("Add subjects filter"),
              div(
                ui_add(colnames(MultiAssayExperiment::colData(data)))
              ),
              tagList(
                lapply(
                  experiment_names,
                  function(experiment_name) {
                    tagList(
                      HTML("&#9658;"),
                      tags$label("Add", tags$code(experiment_name), "filter"),
                      div(
                        ui_add(session$ns(experiment_name), data[[experiment_name]])
                      )
                    )
                  }
                )
              )
            )
          })
        }
      )
    }
  )
)

ui_add <- function(id, data) {
  UseMethod("ui_add", data)
}

ui_add.SummarizedExperiment <- function(id, data) {
  ns <- NS(id)
  row_input <- if (ncol(SummarizedExperiment::rowData(data)) == 0) {
    div("no sample variables available")
  } else if (nrow(SummarizedExperiment::rowData(data)) == 0) {
    div("no samples available")
  } else {
    teal.widgets::optionalSelectInput(
      ns("row_to_add"),
      choices = colnames(SummarizedExperiment::rowData(data)),
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "Select gene variable"
      )
    )
  }

  col_input <- if (ncol(SummarizedExperiment::colData(data)) == 0) {
    div("no sample variables available")
  } else if (nrow(SummarizedExperiment::colData(data)) == 0) {
    div("no samples available")
  } else {
    teal.widgets::optionalSelectInput(
      ns("col_to_add"),
      choices = colnames(SummarizedExperiment::colData(data)),
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "Select sample variable"
      )
    )
  }

  div(row_input, col_input)
}

ui_add.default <- function(id, data) {
  ns <- NS(id)
  teal.widgets::optionalSelectInput(
    ns("col_to_add"),
    choices = colnames(data),
    options = shinyWidgets::pickerOptions(
      liveSearch = TRUE,
      noneSelectedText = "Select sample variable"
    )
  )
}

ui_add.RaggedExperiment <- function(id, data) {
  NULL
}