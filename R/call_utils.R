#' Choices condition call
#'
#' Compose choices condition call from inputs.
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#' @param choices (`vector`)\cr
#'   `varname` values to match using the `==` (single value) or
#'   `%in%` (vector) condition. `choices` can be vector of any type
#'   but for some output might be converted:
#'   \itemize{
#'     \item{`factor`}{ call is composed on choices converted to `character`}
#'     \item{`Date`}{ call is composed on choices converted to `character` using `as.character(choices)`}
#'     \item{`POSIXct`, `POSIXlt`}{ Call is composed on choices converted to `character` using
#'       `format(choices)`. One has to be careful here as formatted date-time variable might loose
#'       some precision (see `format` argument in \code{\link{format.POSIXlt}}) and output call
#'       could be insufficient for exact comparison. In this case one should specify
#'       `varname = trunc(<varname>)` and possibly convert `choices` to `character`)
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#' call_condition_choice("SEX", choices = c(1, 2))
#' call_condition_choice("SEX", choices = "F")
#' call_condition_choice("SEX", choices = c("F", "M"))
#' call_condition_choice("x$SEX", choices = Sys.Date())
#' call_condition_choice("x$SEX", choices = Sys.time())
#' }
#' @return a character string
#' @keywords internal
call_condition_choice <- function(varname, choices, ...) {
  checkmate::assert_string(varname)
  checkmate::assert_multi_class(
    choices,
    c("integer", "numeric", "character", "factor", "Date", "POSIXct", "POSIXlt")
  )

  if (is.numeric(choices)) {
    if (length(choices) == 1L) {
      sprintf("%s == %s", varname, round(choices, 10))
    } else {
      sprintf("%s %%in%% c(%s)", varname, toString(round(choices, 10)))
    }
  } else if (inherits(choices, "Date")) {
    if (length(choices) == 1L) {
      sprintf("%s == as.Date(\"%s\")", varname, as.character(choices))
    } else {
      sprintf("%s %%in%% as.Date(c(%s))", varname, toString(sprintf("\"%s\"", as.character(choices))))
    }
  } else if (inherits(choices, c("POSIXct", "POSIXlt"))) {
    class <- class(choices)[1L]
    tzone <- Find(function(x) x != "", attr(as.POSIXlt(choices), "tzone"))

    if (length(choices) == 1L) {
      sprintf(
        "%s == %s(\"%s\", tz = \"%s\")",
        varname,
        switch(class,
          "POSIXct" = "as.POSIXct",
          "POSIXlt" = "as.POSIXlt"
        ),
        as.character(choices),
        tzone
      )
    } else {
      sprintf(
        "%s %%in%% %s(c(%s), tz = \"%s\")",
        varname,
        switch(class,
          "POSIXct" = "as.POSIXct",
          "POSIXlt" = "as.POSIXlt"
        ),
        toString(sprintf("\"%s\"", as.character(choices))),
        tzone
      )
    }
  } else {
    if (length(choices) == 1L) {
      sprintf("%s == \"%s\"", varname, choices)
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
#' @param range (`numeric(2)`)\cr
#'   range of the variable
#'
#' @return call
#' @examples
#' \dontrun{
#' call_condition_range("AGE", range = c(1, 2))
#' call_condition_range("ADSL$AGE", range = c(-1.2, 2.1))
#' }
#' @return a character string
#' @keywords internal
call_condition_range <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_numeric(range, len = 2, sorted = TRUE)

  sprintf("%1$s >= %2$.10f & %1$s <= %3$.10f", varname, range[1], range[2])
}

#' `logical` variable condition call
#'
#' Compose `logical` variable condition call from inputs
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#' @param choice (`logical(1)`)\cr
#'   chosen value
#'
#' @return call
#' @examples
#' \dontrun{
#' call_condition_logical("event", choice = TRUE)
#' call_condition_logical("event", choice = FALSE)
#' }
#' @return character string
#' @keywords internal
call_condition_logical <- function(varname, choice) {
  checkmate::assert_string(varname)
  checkmate::assert_flag(choice)

  sprintf("%s%s", if (choice) "" else "!", varname)
}


#' `POSIXct` or `POSIXlt` range condition call
#'
#' Compose `POSIXct`/`POSIXlt` range condition call from inputs.
#'
#' @param varname (`character(1)`)\cr
#'   name of the variable
#' @param range (`POSIXct` or `POSIXlt`)\cr
#'   range of the variable
#'
#' @examples
#' \dontrun{
#' call_condition_range_posix(
#'   varname = "datetime",
#'   range = c(Sys.time(), Sys.time() + 1)
#' )
#' call_condition_range_posix(
#'   varname = "datetime",
#'   range = as.POSIXlt(c(Sys.time(), Sys.time() + 1))
#' )
#' }
#' @return A character string ready to be converted to a call
#'         filtering on the same class of date-time as the one in `range`.
#' @keywords internal
call_condition_range_posix <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_multi_class(range, c("POSIXct", "POSIXlt"))
  checkmate::assert(
    checkmate::check_true(length(range) == 2L),
    checkmate::check_true(range[1L] <= range[2L]),
    combine = "and"
  )

  class <- class(range)[1L]
  tzone <- Find(function(x) x != "", attr(as.POSIXlt(range), "tzone"))

  sprintf(
    "%1$s >= %2$s(\"%3$s\", tz = \"%5$s\") & %1$s < %2$s(\"%4$s\", tz = \"%5$s\")",
    varname,
    switch(class,
      "POSIXct" = "as.POSIXct",
      "POSIXlt" = "as.POSIXlt"
    ),
    as.character(range[1]),
    as.character(range[2] + 1),
    tzone
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
#' \dontrun{
#' call_condition_range_date(
#'   "date",
#'   range = c(Sys.Date(), Sys.Date() + 1)
#' )
#' }
#' @return a character string
#' @keywords internal
call_condition_range_date <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_date(range, len = 2)
  if (range[1] > range[2]) stop("\"range\" must be sorted")

  sprintf(
    "%1$s >= as.Date(\"%2$s\") & %1$s <= as.Date(\"%3$s\")",
    varname, as.character(range[1]), as.character(range[2])
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
