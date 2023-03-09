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
#'
#' @examples
#' \dontrun{
#' calls <- list(
#'   call_condition_choice("SEX", "F"),
#'   call_condition_range("AGE", c(20, 50)),
#'   call_condition_choice("ARM", "ARM: A"),
#'   call_condition_logical("SURV", TRUE)
#' )
#' calls <- lapply(calls, str2lang)
#' calls_combine_by(calls, "&")
#' }
#' @return a combined `call`
#' @keywords internal
calls_combine_by <- function(calls, operator) {
  checkmate::assert_list(calls)
  if (length(calls) > 0L) checkmate::assert_list(calls, types = c("call", "name"))
  checkmate::assert_string(operator)

  calls <- Filter(x = calls, f = Negate(is.null)) # disabled filters

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}
