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
  # todo: assert correct slice for MAE
  if (is.null(slice$experiment)) {
    # from colData
    SummarizedExperiment::colData(data)[[slice$varname]]
  } else {
    # todo: assert experiment exists in names(data)
    # todo: fix to handle different types of experiments (like RaggedExperiment, SummarizedExperiment, Matrix)
    experiment <- data[[slice$experiment]]
    get_slice_variable(experiment, slice)
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
