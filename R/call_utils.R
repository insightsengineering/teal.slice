#' Choices condition call
#'
#' Compose choices condition call from inputs.
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#'
#' @param choices (`vector`)\cr
#'   `varname` values to match using the `==` (single value) or
#'   `%in%` (vector) condition. `choices` can be vector of any type
#'   but for some output might be converted:
#'   \itemize{
#'     \item{`factor`}{ call is composed on choices converted to `character`}
#'     \item{`Date`}{ call is composed on choices converted to `character` using `format(choices)`}
#'     \item{`POSIXct`, `POSIXlt`}{ Call is composed on choices converted to `character` using
#'       `format(choices)`. One has to be careful here as formatted date-time variable might loose
#'       some precision (see `format` argument in \code{\link{format.POSIXlt}}) and output call
#'       could be insufficient for exact comparison. In this case one should specify
#'       `varname = trunc(<varname>)` and possibly convert `choices` to `character`)
#'     }
#'   }
#'
#' @examples
#' \donttest{
#' call_condition_choice("SEX", choices = c(1, 2))
#' call_condition_choice("SEX", choices = "F")
#' call_condition_choice("SEX", choices = c("F", "M"))
#' call_condition_choice("x$SEX", choices = Sys.Date())
#' call_condition_choice("x$SEX", choices = Sys.time())
#' }
#' @return a `call`
#' @keywords internal
call_condition_choice <- function(varname, choices) {
  checkmate::assert_string(varname)
  checkmate::assert_multi_class(
    choices,
    c("integer", "numeric", "character", "factor", "Date", "POSIXct", "POSIXlt")
  )

  if (is.numeric(choices)) {
    if (length(choices) == 1) {
      sprintf("%s == %s", varname, sprintf("%s", choices))
    } else {
      sprintf("%s %%in%% c(%s)", varname, toString(sprintf("%s", choices)))
    }
  } else {
    if (length(choices) == 1) {
      sprintf("%s == %s", varname, sprintf("\"%s\"", choices))
    } else {
      sprintf("%s %%in%% c(%s)", varname, toString(sprintf("\"%s\"", choices)))
    }
  }
}

#' `numeric` range condition call
#'
#' Compose `numeric` range condition call from inputs
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#'
#' @param range (`numeric(2)`)\cr
#'   range of the variable
#'
#' @return call
#' @examples
#' \donttest{
#' call_condition_range("AGE", range = c(1, 2))
#' call_condition_range(
#'   call_extract_list("ADSL", "AGE"),
#'   range = c(-1.2, 2.1)
#' )
#' }
#' @return a character string
#' @keywords internal
call_condition_range <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_numeric(range, len = 2, sorted = TRUE)

  sprintf("%s >= %.4f & %s <= %.4f", varname, range[1], varname, range[2])
}

#' `logical` variable condition call
#'
#' Compose `logical` variable condition call from inputs
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#'
#' @param choice (`logical(1)`)\cr
#'   chosen value
#'
#' @return call
#' @examples
#' \donttest{
#' call_condition_logical("event", choice = TRUE)
#' call_condition_logical("event", choice = FALSE)
#' }
#' @return character string
#' @keywords internal
call_condition_logical <- function(varname, choice) {
  checkmate::assert_string(varname)
  checkmate::assert_flag(choice)

  sprintf("%s == %s", varname, choice)
}


#' `POSIXct` range condition call
#'
#' Compose `POSIXct` range condition call from inputs.
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#'
#' @param range (`POSIXct`)\cr
#'   range of the variable. Be aware that output
#'   uses truncated range format `"%Y-%m-%d %H:%M:%S"`, which means that
#'   some precision might be lost.
#'
#' @param timezone (`character(1)`)\cr
#'   specifies the time zone to be used for the conversion.
#'   By default `Sys.timezone()` is used.
#'
#' @examples
#' \donttest{
#' call_condition_range_posixct(
#'   varname = "datetime",
#'   range = c(Sys.time(), Sys.time() + 1),
#'   timezone = "UTC"
#' )
#' }
#' @return a `call`
#' @keywords internal
call_condition_range_posixct <- function(varname, range, timezone = Sys.timezone()) {
  checkmate::assert_string(varname)
  checkmate::assert_posixct(range, len = 2, sorted = TRUE)
  checkmate::assert_string(timezone)

  sprintf(
    "%s >= as.POSIXct(\"%s\", tz = \"%s\") & %s < as.POSIXct(\"%s\", tz = \"%s\")",
    varname, format(range[1], tz = timezone), timezone,
    varname, format(range[2] + 1, tz = timezone), timezone
  )
}

#' `Date` range condition call
#'
#' Compose `Date` range condition call from inputs
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#'
#' @param range (`Date`)\cr
#'   range of the variable
#'
#' @examples
#' \donttest{
#' call_condition_range_date(
#'   "date",
#'   range = c(Sys.Date(), Sys.Date() + 1)
#' )
#' }
#' @return a `call`
#' @keywords internal
call_condition_range_date <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_date(range, len = 2)
  if (range[1] > range[2]) stop("\"range\" must be sorted")

  sprintf(
    "%s >= as.Date(\"%s\") & %s <= as.Date(\"%s\")",
    varname, as.character(range[1]), varname, as.character(range[2])
  )
}

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

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}
