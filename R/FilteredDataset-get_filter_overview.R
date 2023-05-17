get_filter_overview <- function(data, data_filtered, dataname, keys) {
  UseMethod("get_filter_overview")
}

#' @export
#' @examples
#' get_filter_overview(iris, iris[1:50, ], "iris")
#' get_filter_overview(iris, iris[1:50, ], "Species")
get_filter_overview.default <- function(data, data_filtered, dataname, keys = character(0)) {
  if (length(keys) == 0) {
    data.frame(
      dataname = dataname,
      obs = nrow(data),
      obs_filtered = nrow(data_filtered)
    )
  } else {
    data.frame(
      dataname = dataname,
      obs = nrow(data),
      obs_filtered = nrow(data_filtered),
      subjects = nrow(unique(data[keys])),
      subjects_filtered = nrow(unique(data_filtered[keys]))
    )
  }
}

#' @export
#' @examples
#' utils::data(miniACC, package = "MultiAssayExperiment")
#' get_filter_overview(miniACC, miniACC, "miniACC")
get_filter_overview.MultiAssayExperiment <- function(data, data_filtered, dataname, keys) {
  experiment_names <- names(data)
  mae_info <- data.frame(
    dataname = dataname,
    subjects = nrow(SummarizedExperiment::colData(data)),
    subjects_filtered = nrow(SummarizedExperiment::colData(data_filtered))
  )

  experiment_obs_info <- do.call("rbind", lapply(
    experiment_names,
    function(experiment_name) {
      data.frame(
        dataname = sprintf("- %s", experiment_name),
        obs = ncol(data[[experiment_name]]),
        obs_filtered = ncol(data_filtered[[experiment_name]])
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
        subjects_filtered = get_experiment_keys(data_filtered, data_filtered[[experiment_name]])
      )
    }
  ))

  experiment_info <- cbind(experiment_obs_info, experiment_subjects_info)
  dplyr::bind_rows(mae_info, experiment_info)
}