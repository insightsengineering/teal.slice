get_slice_variable <- function(data, slice) {
  UseMethod("get_slice_variable")
}

get_slice_variable.default <- function(data, slice) {
  # data.frame and matrix
  data[, slice$varname, drop = TRUE]
}

get_slice_variable.list <- function(data, slice) {
  data[[slice$varname]]
}

get_slice_variable.MultiAssayExperiment <- function(data, slice) {
  if (length(slice$experiment)) {
    # from experiment
    if (identical(slice$arg, "subset")) {
      # from rowData
      SummarizedExperiment::rowData(data[[slice$experiment]])[[slice$varname]]
    } else if (identical(slice$arg, "select")) {
      SummarizedExperiment::colData(data[[slice$experiment]])[[slice$varname]]
    }
  } else {
    # from colData
    SummarizedExperiment::colData(data)[[slice$varname]]
  }
}
