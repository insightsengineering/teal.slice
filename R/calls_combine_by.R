#' Combine calls by operator
#'
#' Combine list of calls by specific operator
#'
#' @param calls (`list` of calls)\cr
#'   list containing calls to be combined by `operator`;
#'   if empty, NULL is returned
#' @param operator (`character(1)`)\cr
#'   name/symbol of the operator passed as character string
#'
#' @return call or NULL, if `calls` is an empty list
#' @seealso examples found here: `vignette("internal_function_examples", package = "teal.slice")`.
#' @return a combined `call`
#' @keywords internal
calls_combine_by <- function(calls, operator) {
  checkmate::assert_list(calls)
  if (length(calls) > 0L) checkmate::assert_list(calls, types = c("call", "name"))
  checkmate::assert_string(operator)

  calls <- Filter(x = calls, f = Negate(is.null))

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}
