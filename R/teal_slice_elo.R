teal_slice_class <- function(ans) {
  if (all(c("datalabel", "arg") %in% names(ans))) {
    "teal_slice_mae_se"
  } else if (identical("subjects", ans$datalabel)) {
    "teal_slice_mae_subjects"
  } else if ("datalabel" %in% names(ans)) {
    "teal_slice_mae_matrix"
  } else {
    "teal_slice_df"
  }
}

#################################################################
#################################################################
#################################################################

pull_vector <- function(data, x) {
  UseMethod("pull_vector", x)
}

pull_vector.teal_slice_df <- function(data, x) {
  data[[x$varname]]
}

pull_vector.teal_slice_mae_subjects <- function(data, x) {
  SummarizedExperiment::colData(data)[[x$varname]]
}

pull_vector.teal_slice_mae_se <- function(data, x) {
  if (x$arg == "subset") {
    SummarizedExperiment::rowData(data[[x$datalabel]])[[x$varname]]
  } else if (x$arg == "select") {
    SummarizedExperiment::colData(data[[x$datalabel]])[[x$varname]]
  } else {
    stop("arg")
  }
}
