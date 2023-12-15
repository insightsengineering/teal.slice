#' Get slice variable
#'
#' @description
#' Returns a vector of values for given variable.
#' `slice` object defines "location" of the variable in the dataset.
#' For a `data.frame` `slice$varname` is a sufficient information to locate variable in the dataset.
#' For objects of more nested structure (e.g. `SummarizedExperiment`, `MultiAssayExperiment`) `slice$varname`
#' is not enough. This is why `MultiAssayExperiment` has `slice$experiment` property which defines which
#' experiment should be used to extract the variable. If such experiment is of class `SummarizedExperiment` then
#' `slice$arg` property is used to indicate if variable should be extracted from the `rowData` or from the `colData`.
#'
get_slice_variable <- function(data, slice) {
  UseMethod("get_slice_variable")
}

#' @export
get_slice_variable.default <- function(data, slice) {
  # data.frame and matrix
  if (inherits(data, c("data.frame", "DFrame", "array", "Matrix"))) {
    get_slice_variable_array(data, slice)
  } else if (inherits(data, "SummarizedExperiment")) {
    get_slice_variable_SummarizedExperiment(data, slice)
  } else if (inherits(data, "MultiAssayExperiment")) {
    get_slice_variable_MultiAssayExperiment(data, slice)
  } else {
    stop("get_slice_variable not implemented for class ", class(data), call. = FALSE)
  }
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_slice_variable_array <- function(data, slice) {
  data[, slice$varname, drop = TRUE]
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_slice_variable_MultiAssayExperiment <- function(data, slice) {
  if (is.null(slice$experiment)) {
    # from colData
    SummarizedExperiment::colData(data)[[slice$varname]]
  } else if (slice$experiment %in% names(data)) {
    experiment <- data[[slice$experiment]]
    get_slice_variable(experiment, slice)
  } else {
    stop("Experiment ", experiment, " not found in ", slice$dataname, call. = FALSE)
  }
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_slice_variable_SummarizedExperiment <- function(data, slice) {
  if (identical(slice$arg, "subset")) {
    # from rowData
    SummarizedExperiment::rowData(data)[[slice$varname]]
  } else if (identical(slice$arg, "select")) {
    SummarizedExperiment::colData(data)[[slice$varname]]
  } else {
    stop("teal_slice for SummarizedExperiment must contain 'arg' property ('subset' or 'select')", call. = FALSE)
  }
}
