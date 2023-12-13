#' @export
get_slice_variable <- function(data, slice) {
  UseMethod("get_slice_variable")
}

#' @export
get_slice_variable.default <- function(data, slice) {
  # data.frame and matrix
  if (inherits(data, c("data.frame", "DFrame", "matrix", "Matrix"))) {
    data[, slice$varname, drop = TRUE]
  } else {
    stop("data class is not supported", call. = FALSE)
  }
}

#' @export
get_slice_variable.list <- function(data, slice) {
  data[[slice$varname]]
}

#' @export
get_slice_variable.MultiAssayExperiment <- function(data, slice) {
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

#' @export
get_slice_variable.SummarizedExperiment <- function(data, slice) {
  if (identical(slice$arg, "subset")) {
    # from rowData
    SummarizedExperiment::rowData(data)[[slice$varname]]
  } else if (identical(slice$arg, "select")) {
    SummarizedExperiment::colData(data)[[slice$varname]]
  } else {
    stop("teal_slice for SummarizedExperiment must contain 'arg' property ('subset' or 'select')", call. = FALSE)
  }
}
