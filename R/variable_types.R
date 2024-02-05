#' Get classes of selected columns from dataset
#'
#' @param data (`data.frame` or `DataFrame` or `matrix`) Object in which to determine variable types.
#' @param columns (`character`) Vector of columns in `data` for which to get types.
#'   Set to `NULL` to get types of all columns.
#'
#' @return Character vector of classes of `columns` from provided `data`.
#'
#' @keywords internal
#'
variable_types <- function(data, columns = NULL) {
  checkmate::assert_multi_class(data, c("data.frame", "DataFrame", "matrix"))
  checkmate::assert_character(columns, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_subset(columns, colnames(data))

  if (is.matrix(data)) {
    type <- typeof(data)
    if (type == "double") type <- "numeric"
    types <-
      if (is.null(columns)) {
        stats::setNames(rep_len(type, ncol(data)), nm = colnames(data))
      } else {
        stats::setNames(rep_len(type, length(columns)), nm = columns)
      }
  } else {
    types <- vapply(data, function(x) class(x)[1L], character(1L))
    if (!is.null(columns)) types <- types[columns]
    # alternative after R 4.4.0: `types <- types[columns %||% TRUE]`
  }
  types
}
