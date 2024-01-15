#' Get classes of selected columns from dataset
#'
#' @param data (`matrix` or `data.frame`-like) object to determine variable types from.
#' @param columns (`character`) vector of columns in `data` to get classes from.
#'   Set to `NULL` to get classes of all columns.
#'
#' @return Character vector of classes of `columns` from provided `data`.
#' @examples
#' # use non-exported function from teal.slice
#' variable_types <- getFromNamespace("variable_types", "teal.slice")
#'
#' variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   ),
#'   "x"
#' )
#'
#' variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   ),
#'   c("x", "z")
#' )
#'
#' variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   )
#' )
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
