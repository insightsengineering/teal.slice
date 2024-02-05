#' Initialize `FilterStates` object
#'
#' @param data (`data.frame` or `MultiAssayExperiment` or `SummarizedExperiment` or `matrix`)
#'   object to subset.
#' @param data_reactive (`function(sid)`)
#'   should return an object of the same type as `data` or `NULL`.
#'   This function is needed for the `FilterState` `shiny` module to update counts if filtered data changes.
#'   If function returns `NULL` then filtered counts are not shown.
#'   Function has to have `sid` argument being a character which is related to `sid` argument in the `get_call` method.
#' @param dataname (`character(1)`)
#'   name of the data used in the subset expression,
#'   passed to the function argument attached to this `FilterStates`.
#' @param datalabel (`character(1)`)
#'   optional text label.
#' @param ... (optional)
#'   additional arguments for specific classes: keys.
#'
#' @return Object of class `FilterStates`.
#'
#' @keywords internal
#' @export
#'
init_filter_states <- function(data,
                               data_reactive = reactive(NULL),
                               dataname,
                               datalabel = NULL,
                               ...) {
  UseMethod("init_filter_states")
}

#' @keywords internal
#' @export
init_filter_states.data.frame <- function(data, # nolint
                                          data_reactive = function(sid = "") NULL,
                                          dataname,
                                          datalabel = NULL,
                                          keys = character(0),
                                          ...) {
  DFFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel,
    keys = keys
  )
}

#' @keywords internal
#' @export
init_filter_states.matrix <- function(data, # nolint
                                      data_reactive = function(sid = "") NULL,
                                      dataname,
                                      datalabel = NULL,
                                      ...) {
  MatrixFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel
  )
}

#' @keywords internal
#' @export
init_filter_states.MultiAssayExperiment <- function(data, # nolint
                                                    data_reactive = function(sid = "") NULL,
                                                    dataname,
                                                    datalabel = "subjects",
                                                    keys = character(0),
                                                    ...) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel,
    keys = keys
  )
}

#' @keywords internal
#' @export
init_filter_states.SummarizedExperiment <- function(data, # nolint
                                                    data_reactive = function(sid = "") NULL,
                                                    dataname,
                                                    datalabel = NULL,
                                                    ...) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
  }
  SEFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel
  )
}

#' Gets supported filterable variable names
#'
#' Gets filterable variable names from a given object. The names match variables
#' of classes in an vector `teal.slice:::.filterable_class`.
#' @param data
#'   the `R` object containing elements which class can be checked through `vapply` or `apply`.
#' @return `character` vector of variable names.
#'
#' @keywords internal
#' @export
get_supported_filter_varnames <- function(data) {
  UseMethod("get_supported_filter_varnames")
}

#' @keywords internal
#' @export
get_supported_filter_varnames.default <- function(data) { # nolint
  is_expected_class <- vapply(
    X = data,
    FUN = function(x) any(class(x) %in% .filterable_class),
    FUN.VALUE = logical(1)
  )
  names(is_expected_class[is_expected_class])
}

#' @keywords internal
#' @export
get_supported_filter_varnames.matrix <- function(data) { # nolint
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(colnames(data))) {
    colnames(data)
  } else {
    character(0)
  }
}

#' @keywords internal
#' @export
get_supported_filter_varnames.MultiAssayExperiment <- function(data) { # nolint
  data <- SummarizedExperiment::colData(data)
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(names(data))) {
    names(data)
  } else {
    character(0)
  }
}

#' Returns a `choices_labeled` object
#'
#' @param data (`data.frame` or `DFrame` or `list`)
#'   where labels can be taken from in case when `varlabels` is not specified.
#'   `data` must be specified if `varlabels` is not specified.
#' @param choices (`character`)
#'  the vector of chosen variables
#' @param varlabels (`character`)
#'  the labels of variables in data
#' @param keys (`character`)
#'  the names of the key columns in data
#' @return `character(0)` if choices are empty; a `choices_labeled` object otherwise
#' @keywords internal
data_choices_labeled <- function(data,
                                 choices,
                                 varlabels = teal.data::col_labels(data, fill = TRUE),
                                 keys = character(0)) {
  if (length(choices) == 0) {
    return(character(0))
  }
  choice_types <- variable_types(data = data, columns = choices)
  choice_types[keys] <- "primary_key"

  choices_labeled(
    choices = choices,
    labels = unname(varlabels[choices]),
    types = choice_types[choices]
  )
}

#' @noRd
#' @keywords internal
get_varlabels <- function(data) {
  if (!is.array(data)) {
    vapply(
      colnames(data),
      FUN = function(x) {
        label <- attr(data[[x]], "label")
        if (is.null(label)) {
          x
        } else {
          label
        }
      },
      FUN.VALUE = character(1)
    )
  } else {
    character(0)
  }
}
