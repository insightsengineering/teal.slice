# MAEFilteredDataset ------
#' @keywords internal
#' @title `MAEFilteredDataset` R6 class
MAEFilteredDataset <- R6::R6Class( # nolint
  classname = "MAEFilteredDataset",
  inherit = FilteredDataset,

  # public methods ----
  public = list(
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
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    #'
    ui_add = function(id) {
      ns <- NS(id)
      data <- self$get_dataset()
      experiment_names <- names(data)

      div(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        br(),
        HTML("&#9658;"),
        tags$label("Add subjects filter"),
        private$get_filter_states()[["subjects"]]$ui_add(id = ns("subjects")),
        tagList(
          lapply(
            experiment_names,
            function(experiment_name) {
              tagList(
                HTML("&#9658;"),
                tags$label("Add", tags$code(experiment_name), "filter"),
                private$get_filter_states()[[experiment_name]]$ui_add(id = ns(experiment_name))
              )
            }
          )
        )
      )
    },

    #' @description
    #' Get filter overview rows of a dataset
    #' @return (`matrix`) matrix of observations and subjects
    get_filter_overview = function() {
      data <- self$get_dataset()
      data_filtered <- self$get_dataset(TRUE)
      experiment_names <- names(data)

      mae_info <- data.frame(
        dataname = private$dataname,
        subjects = nrow(SummarizedExperiment::colData(data)),
        subjects_filtered = nrow(SummarizedExperiment::colData(data_filtered()))
      )

      experiment_obs_info <- do.call("rbind", lapply(
        experiment_names,
        function(experiment_name) {
          data.frame(
            dataname = sprintf("- %s", experiment_name),
            obs = ncol(data[[experiment_name]]),
            obs_filtered = ncol(data_filtered()[[experiment_name]])
          )
        }
      ))

      get_experiment_keys <- function(mae, experiment) {
        sample_subset <- subset(MultiAssayExperiment::sampleMap(mae), colname %in% colnames(experiment))
        length(unique(sample_subset$primary))
      }

      experiment_subjects_info <- do.call("rbind", lapply(
        experiment_names,
        function(experiment_name) {
          data.frame(
            subjects = get_experiment_keys(data, data[[experiment_name]]),
            subjects_filtered = get_experiment_keys(data_filtered(), data_filtered()[[experiment_name]])
          )
        }
      ))

      experiment_info <- cbind(experiment_obs_info, experiment_subjects_info)
      dplyr::bind_rows(mae_info, experiment_info)
    }
  )
)
