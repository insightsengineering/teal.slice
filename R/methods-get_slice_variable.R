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
#' @inheritParams filter_panel_methods
#' @param slice (`teal_slice`)/cr
#'  contains fields which can determine a "location" of the vector to filter on.
#'
#' @include teal_slice.R
#'
#' @name get_slice_variable
#' @rdname get_slice_variable
#' @aliases get_slice_variable-ANY,teal_slice-method
#' @aliases get_slice_variable-data.frame,teal_slice-method
#' @aliases get_slice_variable-DataFrame,teal_slice-method
#' @aliases get_slice_variable-matrix,teal_slice-method
#' @aliases get_slice_variable-Matrix,teal_slice-method
#' @aliases get_slice_variable-SummarizedExperiment,teal_slice-method
#' @aliases get_slice_variable-MultiAssayExperiment,teal_slice-method
#'
#' @export
#'
# get_slice_variable generic ----
setGeneric("get_slice_variable", function(data, slice) {
  NULL
})

## default method ----
setMethod("get_slice_variable", c("ANY", "teal_slice"), function(data, slice) {
  stop("get_slice_variable not implemented for class ", toString(class(data)), call. = FALSE)
})

## data.frame method ----
setMethod("get_slice_variable", c("data.frame", "teal_slice"), function(data, slice) {
  data[, slice$varname, drop = TRUE]
})

## DataFrame method ----
setMethod("get_slice_variable", c("DataFrame", "teal_slice"), function(data, slice) {
  data[, slice$varname, drop = TRUE]
})

## matrix method ----
setMethod("get_slice_variable", c("matrix", "teal_slice"), function(data, slice) {
  data[, slice$varname, drop = TRUE]
})

## Matrix method ----
setMethod("get_slice_variable", c("Matrix", "teal_slice"), function(data, slice) {
  data[, slice$varname, drop = TRUE]
})

## SummarizedExperiment method ----
setMethod("get_slice_variable", c("SummarizedExperiment", "teal_slice"), function(data, slice) {
  if (identical(slice$arg, "subset")) {
    # from rowData
    SummarizedExperiment::rowData(data)[[slice$varname]]
  } else if (identical(slice$arg, "select")) {
    SummarizedExperiment::colData(data)[[slice$varname]]
  } else {
    stop("teal_slice for SummarizedExperiment must contain 'arg' property ('subset' or 'select')", call. = FALSE)
  }
})

## MultiAssayExperiment method ----
setMethod("get_slice_variable", c("MultiAssayExperiment", "teal_slice"), function(data, slice) {
  if (is.null(slice$experiment)) {
    # from colData
    SummarizedExperiment::colData(data)[[slice$varname]]
  } else if (slice$experiment %in% names(data)) {
    experiment <- data[[slice$experiment]]
    get_slice_variable(data = experiment, slice = slice)
  } else {
    stop("Experiment ", experiment, " not found in ", slice$dataname, call. = FALSE)
  }
})
