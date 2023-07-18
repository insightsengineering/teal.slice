# MAEFilteredDataset ------
#' @keywords internal
#' @title `MAEFilteredDataset` R6 class
MAEFilteredDataset <- R6::R6Class( # nolint
  classname = "MAEFilteredDataset",
  inherit = FilteredDataset,

  # public methods ----
  public = list(
    #' @description
    #' Initialize `MAEFilteredDataset` object
    #'
    #' @param dataset (`MulitiAssayExperiment`)\cr
    #'  a single `MultiAssayExperiment` for which to define a subset
    #' @param dataname (`character`)\cr
    #'  a given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   vector with primary keys
    #' @param label (`character`)\cr
    #'   label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   field containing metadata about the dataset;
    #'   each element of the list must be atomic and length one
    #'
    initialize = function(dataset, dataname, keys = character(0), label = character(0), metadata = NULL) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_class(dataset, "MultiAssayExperiment")
      super$initialize(dataset, dataname, keys, label, metadata)
      experiment_names <- names(dataset)

      # subsetting by subjects means subsetting by colData(MAE)
      private$add_filter_states(
        filter_states = init_filter_states(
          data = dataset,
          data_reactive = private$data_filtered_fun,
          dataname = dataname,
          datalabel = "subjects",
          keys = self$get_keys()
        ),
        id = "subjects"
      )
      # elements of the list (experiments) are unknown
      # dispatch needed because we can't hardcode methods otherwise:
      #  if (matrix) else if (SummarizedExperiment) else if ...
      lapply(
        experiment_names,
        function(experiment_name) {
          data_reactive <- function(sid = "") private$data_filtered_fun(sid)[[experiment_name]]
          private$add_filter_states(
            filter_states = init_filter_states(
              data = dataset[[experiment_name]],
              data_reactive = data_reactive,
              dataname = dataname,
              datalabel = experiment_name
            ),
            id = experiment_name
          )
        }
      )
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
    #' fs <- teal_slices(
    #'   teal_slice(
    #'     dataname = "MAE", varname = "years_to_birth", selected = c(30, 50), keep_na = TRUE
    #'   ),
    #'   teal_slice(
    #'     dataname = "MAE", varname = "vital_status", selected = "1", keep_na = FALSE
    #'   ),
    #'   teal_slice(
    #'     dataname = "MAE", varname = "gender", selected = "female", keep_na = TRUE
    #'   ),
    #'   teal_slice(
    #'     dataname = "MAE", varname = "ARRAY_TYPE", selected = "", keep_na = TRUE
    #'   )
    #' )
    #' dataset$set_filter_state(state = fs)
    #' shiny::isolate(dataset$get_filter_state())
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      shiny::isolate({
        logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
        checkmate::assert_class(state, "teal_slices")
        lapply(state, function(x) {
          checkmate::assert_true(x$dataname == private$dataname, .var.name = "dataname matches private$dataname")
        })

        # set state on subjects
        subject_state <- Filter(function(x) is.null(x$experiment), state)
        private$get_filter_states()[["subjects"]]$set_filter_state(subject_state)

        # set state on experiments
        # determine target experiments (defined in teal_slices)
        experiments <- unique(unlist(lapply(state, function(x) x[["experiment"]])))
        available_experiments <- setdiff(names(private$get_filter_states()), "subjects")
        excluded_filters <- setdiff(experiments, available_experiments)
        if (length(excluded_filters)) {
          stop(sprintf(
            "%s doesn't contain elements specified in 'experiment': %s\n'experiment' should be a subset of: %s",
            private$dataname,
            toString(excluded_filters),
            toString(available_experiments)
          ))
        }

        # set states on state_lists with corresponding experiments
        lapply(available_experiments, function(experiment) {
          slices <- Filter(function(x) identical(x$experiment, experiment), state)
          private$get_filter_states()[[experiment]]$set_filter_state(slices)
        })

        logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")

        invisible(NULL)
      })
    },

    #' @description
    #' Remove one or more `FilterState` of a `MAEFilteredDataset`
    #'
    #' @param state (`teal_slices`)\cr
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      shiny::isolate({
        checkmate::assert_class(state, "teal_slices")

        logger::log_trace("{ class(self)[1] }$remove_filter_state removing filter(s), dataname: { private$dataname }")
        # remove state on subjects
        subject_state <- Filter(function(x) is.null(x$experiment), state)
        private$get_filter_states()[["subjects"]]$remove_filter_state(subject_state)

        # remove state on experiments
        # determine target experiments (defined in teal_slices)
        experiments <- unique(unlist(lapply(state, function(x) x[["experiment"]])))
        available_experiments <- setdiff(names(private$get_filter_states()), "subjects")
        excluded_filters <- setdiff(experiments, available_experiments)
        if (length(excluded_filters)) {
          stop(sprintf(
            "%s doesn't contain elements specified in 'experiment': %s\n'experiment' should be a subset of: %s",
            private$dataname,
            toString(excluded_filters),
            toString(available_experiments)
          ))
        }
        # remove states on state_lists with corresponding experiments
        lapply(experiments, function(experiment) {
          slices <- Filter(function(x) identical(x$experiment, experiment), state)
          private$get_filter_states()[[experiment]]$remove_filter_state(slices)
        })


        logger::log_trace("{ class(self)[1] }$remove_filter_state removed filter(s), dataname: { private$dataname }")

        invisible(NULL)
      })
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
            obs = nrow(data[[experiment_name]]),
            obs_filtered = nrow(data_filtered()[[experiment_name]])
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
