#' Compose predicates
#'
#' Combines calls with a logical operator.
#'
#' This function is used to combine logical predicates produced by `FilterState` objects
#' to build a complete subset expression.
#'
#' @param calls (`list`)
#'   containing calls (or symbols) to be combined by `operator`
#' @param operator (`character(1)`)
#'   infix operator to use in predicate composition, _e.g._ `"&"`
#'
#' @return
#' A `call` where elements of `calls` are composed with `operator` or `NULL` if `calls` is an empty list.
#'
#' @examples
#' # use non-exported function from teal.slice
#' calls_combine_by <- getFromNamespace("calls_combine_by", "teal.slice")
#'
#' calls <- list(
#'   quote(SEX == "F"), # subsetting on factor
#'   quote(AGE >= 20 & AGE <= 50), # subsetting on range
#'   quote(!SURV) # subsetting on logical
#' )
#' calls_combine_by(calls, "&")
#'
#' @keywords internal
#'
calls_combine_by <- function(calls, operator) {
  checkmate::assert_list(calls)
  if (length(calls) > 0L) checkmate::assert_list(calls, types = c("call", "name"))
  checkmate::assert_string(operator)

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}
