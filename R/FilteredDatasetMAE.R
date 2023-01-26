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
          data_filtered = self$get_dataset(filtered = TRUE),
          input_dataname = as.name(dataname),
          output_dataname = as.name(dataname),
          varlabels = self$get_varlabels(),
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
          input_dataname <- call_extract_list(
            dataname,
            experiment_name,
            dollar = FALSE
          )
          fd <- self$get_dataset(filtered = TRUE)
          private$add_filter_states(
            filter_states = init_filter_states(
              data = dataset[[experiment_name]],
              data_filtered = reactive(fd()[[experiment_name]]),
              input_dataname = input_dataname,
              output_dataname = input_dataname,
              datalabel = experiment_name
            ),
            id = experiment_name
          )
        }
      )
    },

    #' @description
    #' Get filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains multiple `FilterStates`:
    #' \itemize{
    #'   \item{`colData(dataset)`}{for this object single `MAEFilterStates`
    #'   which returns `subsetByColData` call}
    #'   \item{experiments}{for each experiment single `SEFilterStates` and
    #'   `FilterStates_matrix`, both returns `subset` call}
    #' }
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      filter_call <- Filter(
        f = Negate(is.null),
        x = lapply(
          self$get_filter_states(),
          function(x) x$get_call()
        )
      )
      if (length(filter_call) == 0) {
        return(NULL)
      }
      filter_call
    },

    #' @description
    #' Gets labels of variables in the data
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    get_varlabels = function(variables = NULL) {
      checkmate::assert_character(variables, null.ok = TRUE, any.missing = FALSE)

      labels <- vapply(
        X = SummarizedExperiment::colData(self$get_dataset()),
        FUN.VALUE = character(1),
        FUN = function(x) {
          label <- attr(x, "label")
          if (length(label) != 1) {
            NA_character_
          } else {
            label
          }
        }
      )

      if (is.null(labels)) {
        return(NULL)
      }
      if (!is.null(variables)) labels <- labels[names(labels) %in% variables]
      labels
    },

    #' @description
    #' Get filter overview rows of a dataset
    #' @return (`matrix`) matrix of observations and subjects
    get_filter_overview_info = function() {
      dataset <- self$get_dataset()
      dataset_filtered <- self$get_dataset(TRUE)
      names_exps <- paste0("- ", names(dataset))
      mae_and_exps <- c(self$get_dataname(), names_exps)

      df <- cbind(
        private$get_filter_overview_nobs(dataset, dataset_filtered),
        self$get_filter_overview_nsubjs()
      )

      rownames(df) <- mae_and_exps
      colnames(df) <- c("Obs", "Subjects")

      df
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
    #' fs <- list(
    #'   subjects = list(
    #'     years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
    #'     vital_status = list(selected = "1", keep_na = FALSE),
    #'     gender = list(selected = "female", keep_na = TRUE)
    #'   ),
    #'   RPPAArray = list(
    #'     subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
    #'   )
    #' )
    #' shiny::isolate(dataset$set_filter_state(state = fs))
    #' shiny::isolate(dataset$get_filter_state())
    #' @return `NULL`
    #'
    set_filter_state = function(state) {
      checkmate::assert_list(state)
      checkmate::assert_subset(names(state), c(names(self$get_filter_states())))

      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$set_filter_state setting up filters of variable %s, dataname: %s",
          paste(names(state), collapse = ", "),
          self$get_dataname()
        )
      )
      lapply(names(state), function(fs_name) {
        fs <- self$get_filter_states()[[fs_name]]
        fs$set_filter_state(state = state[[fs_name]])
      })

      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$set_filter_state done setting filters of variable %s, dataname: %s",
          paste(names(state), collapse = ", "),
          self$get_dataname()
        )
      )
      NULL
    },

    #' @description Remove one or more `FilterState` of a `MAEFilteredDataset`
    #'
    #' @param state_id (`list`)\cr
    #'  Named list of variables to remove their `FilterState`.
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(state_id) {
      checkmate::assert_list(state_id, names = "unique")
      checkmate::assert_subset(names(state_id), c(names(self$get_filter_states())))

      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$remove_filter_state removing filters of variable %s, dataname: %s",
          state_id,
          self$get_dataname()
        )
      )

      for (fs_name in names(state_id)) {
        fdata_filter_state <- self$get_filter_states()[[fs_name]]
        fdata_filter_state$remove_filter_state(
          `if`(fs_name == "subjects", state_id[[fs_name]][[1]], state_id[[fs_name]])
        )
      }
      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$remove_filter_state done removing filters of variable %s, dataname: %s",
          state_id,
          self$get_dataname()
        )
      )
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
    ui_add_filter_state = function(id) {
      ns <- NS(id)
      data <- self$get_dataset()
      experiment_names <- names(data)

      div(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        br(),
        HTML("&#9658;"),
        tags$label("Add subjects filter"),
        self$get_filter_states("subjects")$ui_add_filter_state(id = ns("subjects")),
        tagList(
          lapply(
            experiment_names,
            function(experiment_name) {
              tagList(
                HTML("&#9658;"),
                tags$label("Add", tags$code(experiment_name), "filter"),
                self$get_filter_states(experiment_name)$ui_add_filter_state(id = ns(experiment_name))
              )
            }
          )
        )
      )
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add_filter_state` calls multiple modules
    #' of the same name from `FilterStates` as `MAEFilteredDataset`
    #' contains one `FilterStates` object for `colData` and one for each
    #' experiment.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #'
    #' @return `moduleServer` function which returns `NULL`
    #'
    srv_add_filter_state = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(paste(
            "MAEFilteredDataset$srv_add_filter_state initializing,",
            "dataname: { deparse1(self$get_dataname()) }"
          ))
          self$get_filter_states("subjects")$srv_add_filter_state(id = "subjects")
          experiment_names <- names(self$get_dataset())
          lapply(
            experiment_names,
            function(experiment_name) {
              self$get_filter_states(experiment_name)$srv_add_filter_state(experiment_name)
            }
          )
          logger::log_trace(paste(
            "MAEFilteredDataset$srv_add_filter_state initialized,",
            "dataname: { deparse1(self$get_dataname()) }"
          ))
          NULL
        }
      )
    },

    #' @description
    #' Gets filter overview subjects number
    #' @return `list` with the number of subjects of filtered/non-filtered datasets.
    get_filter_overview_nsubjs = function() {
      data_f <- self$get_dataset(TRUE)
      data_nf <- self$get_dataset()
      experiment_names <- names(data_nf)

      data_f_subjects_info <- nrow(SummarizedExperiment::colData(data_f()))
      data_nf_subjects_info <- nrow(SummarizedExperiment::colData(data_nf))
      mae_total_subjects_info <- paste0(data_f_subjects_info, "/", data_nf_subjects_info)

      get_experiment_rows <- function(mae, experiment) {
        sample_subset <- subset(MultiAssayExperiment::sampleMap(mae), colname %in% colnames(experiment))
        length(unique(sample_subset$primary))
      }

      subjects_info <- lapply(
        experiment_names,
        function(experiment_name) {
          subjects_f_rows <- get_experiment_rows(data_f(), data_f()[[experiment_name]])
          subjects_nf_rows <- get_experiment_rows(data_nf, data_nf[[experiment_name]])

          subjects_info <- paste0(subjects_f_rows, "/", subjects_nf_rows)
          subjects_info
        }
      )

      append(
        list(mae_total_subjects_info),
        subjects_info
      )
    }
  ),

  # private members ----
  private = list(

    # Gets filter overview observations number and returns a
    # list of the number of observations of filtered/non-filtered datasets
    get_filter_overview_nobs = function(dataset, dataset_filtered) {
      experiment_names <- names(dataset)
      mae_total_data_info <- ""

      data_info <- lapply(
        experiment_names,
        function(experiment_name) {
          data_f_rows <- ncol(dataset_filtered()[[experiment_name]])
          data_nf_rows <- ncol(dataset[[experiment_name]])

          data_info <- paste0(data_f_rows, "/", data_nf_rows)
          data_info
        }
      )

      append(
        list(mae_total_data_info),
        data_info
      )
    }
  )
)
