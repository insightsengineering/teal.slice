#' Get classes of selected columns from dataset
#'
#' @param data (`data.frame`) data to determine variable types from
#' @param columns (atomic vector of `character` or `NULL`) column names chosen from `data`.
#'   The value of `NULL` will be interpreted to mean all columns.
#'
#' @return (atomic vector of `character`) classes of `columns` from provided `data`
#' @seealso check examples here `vignette("internal_functions_example", package = "teal.slice")`.
#' @keywords internal
#'
variable_types <- function(data, columns = NULL) {
  UseMethod("variable_types")
}

#' @export
variable_types.default <- function(data, columns = NULL) {
  checkmate::assert_character(columns, null.ok = TRUE, any.missing = FALSE)

  res <- if (is.null(columns)) {
    vapply(
      data,
      function(x) class(x)[[1]],
      character(1),
      USE.NAMES = FALSE
    )
  } else if (checkmate::test_character(columns, any.missing = FALSE)) {
    stopifnot(all(columns %in% names(data) | vapply(columns, identical, logical(1L), "")))
    vapply(
      columns,
      function(x) ifelse(x == "", "", class(data[[x]])[[1]]),
      character(1),
      USE.NAMES = FALSE
    )
  } else {
    character(0)
  }

  return(res)
}

#' @export
variable_types.data.frame <- function(data, columns = NULL) { # nolint: object_name_linter.
  variable_types.default(data, columns)
}

#' @export
variable_types.DataTable <- function(data, columns = NULL) {
  variable_types.default(data, columns)
}

#' @export
variable_types.DFrame <- function(data, columns = NULL) {
  variable_types.default(data, columns)
}

#' @export
variable_types.matrix <- function(data, columns = NULL) {
  checkmate::assert_character(columns, null.ok = TRUE, any.missing = FALSE)

  res <- if (is.null(columns)) {
    apply(
      data,
      2,
      function(x) class(x)[1]
    )
  } else if (checkmate::test_character(columns, any.missing = FALSE)) {
    stopifnot(
      all(
        columns %in% colnames(data) |
          vapply(columns, identical, logical(1L), "")
      )
    )
    vapply(
      columns,
      function(x) ifelse(x == "", "", class(data[, x])[1]),
      character(1),
      USE.NAMES = FALSE
    )
  } else {
    character(0)
  }

  return(res)
}
