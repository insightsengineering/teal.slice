#' Get Label Attributes of Variables in a \code{data.frame}
#'
#' Variable labels can be stored as a \code{label} attribute for each variable.
#' This functions returns a named character vector with the variable labels
#' (empty sting if not specified)
#'
#' @param x a \code{data.frame} object
#' @param fill boolean in case the \code{label} attribute does not exist if
#'   \code{TRUE} the variable names is returned, otherwise \code{NA}
#'
#' @return a named character vector with the variable labels, the names
#'   correspond to the variable names
#'
#' @export
#'
#' @examples
#' x <- iris
#' formatters_var_labels(x)
formatters_var_labels <- function(x, fill = FALSE) {
  stopifnot(is.data.frame(x))
  if (NCOL(x) == 0) {
    return(character())
  }

  y <- Map(function(col, colname) {
    label <- attr(col, "label")

    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!is.character(label) && !(length(label) == 1)) {
        stop("label for variable ", colname, "is not a character string")
      }
      as.vector(label)
    }
  }, x, colnames(x))

  labels <- unlist(y, recursive = FALSE, use.names = TRUE)

  if (!is.character(labels)) {
    stop("label extraction failed")
  }

  labels
}
