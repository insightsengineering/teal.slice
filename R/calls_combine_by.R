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
#' fs_ch <- ChoicesFilterState$new(x = c("F", "M"),varname = "SEX")
#' fs_ch$set_state(list(selected = c("F")))
#'
#' fs_rng <- RangeFilterState$new(x = 1:100, varname = "AGE")
#' fs_rng$set_state(list(selected = c(20, 50)))
#'
#' fs_log <- LogicalFilterState$new(x = c(TRUE, FALSE), varname = "SURV")
#' fs_log$set_state(list(selected = TRUE))
#'
#' calls <- list(
#'   shiny::isolate(fs_ch$get_call()),
#'   shiny::isolate(fs_rng$get_call()),
#'   shiny::isolate(fs_log$get_call())
#' )
#' calls_combine_by(calls, "&")
#' }
#' @return a combined `call`
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
