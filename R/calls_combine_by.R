#' Compose predicates
#'
#' Combines calls with a logical operator.
#'
#' This function is used to combine logical predicates produced by `FilterState` objects
#' to build a complete subsetting expression.
#'
#' @param calls (`list`)\cr
#'   containing calls (or symbols) to be combined by `operator`
#' @param operator (`character(1)`)\cr
#'   infix operator to use in predicate composition, _e.g._ `"&"`
#'
#' @return
#' A `call` where elements of `calls` are composed with `operator` or `NULL` if `calls` is an empty list.
#'
#' @seealso examples found here: `vignette("internal_function_examples", package = "teal.slice")`.
#'
#' @keywords internal
calls_combine_by <- function(calls, operator) {
  checkmate::assert_list(calls)
  if (length(calls) > 0L) checkmate::assert_list(calls, types = c("call", "name"))
  checkmate::assert_string(operator)

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}
