#' Get classes of selected columns from dataset
#'
#' @param data (`data.frame`) data to determine variable types from
#' @param columns (atomic vector of `character` or `NULL`) column names chosen from `data`.
#'   The value of `NULL` will be interpreted to mean all columns.
#'
#' @return (atomic vector of `character`) classes of `columns` from provided `data`
#' @keywords internal
#' @examples
#' teal.slice:::variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   ),
#'   "x"
#' )
#'
#' teal.slice:::variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   ),
#'   c("x", "z")
#' )
#'
#' teal.slice:::variable_types(
#'   data.frame(
#'     x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
#'     stringsAsFactors = FALSE
#'   )
#' )
variable_types <- function(data, columns = NULL) {
  checkmate::assert_multi_class(data, c("data.frame", "DFrame", "matrix"))
  checkmate::assert_character(columns, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_subset(columns, colnames(data))

  if (is.matrix(data)) {
    type <- typeof(data)
    if (type == "double") type <- "numeric"
    types <-
      if (length(columns) == 0L) {
        stats::setNames(rep_len(type, ncol(data)), nm = colnames(data))
      } else {
        stats::setNames(rep_len(type, length(columns)), nm = columns)
      }
  } else {
    types <- vapply(data, function(x) class(x)[1L], character(1L))
    if (length(columns) != 0L) types <- types[columns]
  }
  types
}
