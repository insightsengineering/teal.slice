
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
#' call_condition_choice(as.name("SEX"), choices = "F")
#' call_condition_choice("SEX", choices = c("F", "M"))
#' call_condition_choice("SEX", choices = factor(c("F", "M")))
#' call_condition_choice("x$SEX", choices = Sys.Date())
#' call_condition_choice("trunc(x$SEX)", choices = Sys.time())
#' }
#' @return a `call`
#' @keywords internal
call_condition_choice <- function(varname, choices) {
  checkmate::assert_string(varname)
  checkmate::assert_multi_class(
    choices,
    c("integer", "numeric", "character", "factor", "Date", "POSIXct", "POSIXlt")
  )

  varname <- str2lang(varname)

  if (is.factor(choices)) {
    choices <- as.character(choices)
  } else if (inherits(choices, "Date")) {
    choices <- format(choices)
  } else if (inherits(choices, c("POSIXct", "POSIXlt"))) {
    choices <- format(choices)
  }

  if (length(choices) == 1) {
    call("==", varname, choices)
  } else {
    c_call <- do.call(
      "call",
      append(list("c"), choices)
    )
    # c_call needed because it needs to be vector call
    # instead of vector. SummarizedExperiment.subset
    # handles only vector calls
    call("%in%", varname, c_call)
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
#' call_condition_range(as.name("AGE"), range = c(-1.2, 2.1))
#' call_condition_range(
#'   call_extract_list("ADSL", "AGE"),
#'   range = c(-1.2, 2.1)
#' )
#' }
#' @return a `call`
#' @keywords internal
call_condition_range <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_numeric(range, len = 2, sorted = TRUE)

  varname <- str2lang(varname)
  call(
    "&",
    call(">=", varname, range[1]),
    call("<=", varname, range[2])
  )
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
#' @return a `call`
#' @keywords internal
call_condition_logical <- function(varname, choice) {
  checkmate::assert_string(varname)
  checkmate::assert_flag(choice)

  if (choice) {
    str2lang(varname)
  } else if (!choice) {
    call("!", str2lang(varname))
  } else {
    stop(
      "Unknown filter state", toString(choice),
      " for logical var ", varname
    )
  }
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

  varname <- str2lang(varname)

  range[1] <- trunc(range[1], units = c("secs"))
  range[2] <- trunc(range[2] + 1, units = c("secs"))

  range <- format(
    range,
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )

  call(
    "&",
    call(">=", varname, call("as.POSIXct", range[1], tz = timezone)),
    call("<", varname, call("as.POSIXct", range[2], tz = timezone))
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
#'   as.name("date"),
#'   range = c(Sys.Date(), Sys.Date() + 1)
#' )
#' }
#' @return a `call`
#' @keywords internal
call_condition_range_date <- function(varname, range) {
  checkmate::assert_string(varname)
  checkmate::assert_date(range, len = 2)
  checkmate::assert_true(range[2] >= range[1])

  varname <- str2lang(varname)

  call(
    "&",
    call(">=", varname, call("as.Date", as.character(range[1]))),
    call("<=", varname, call("as.Date", as.character(range[2])))
  )
}

#' Get call to subset and select array
#'
#' Get call to subset and select array
#' @param dataname (`character(1)`, `name` or `call`)\cr
#' @param row (`name`, `call`, `logical`, `integer`, `character`)\cr
#'   optional, name of the `row` or condition
#' @param column (`name`, `call`, `logical`, `integer`, `character`)\cr
#'   optional, name of the `column` or condition
#' @param aisle (`name`, `call`, `logical`, `integer`, `character`)\cr
#'   optional, name of the `row` or condition
#' @return `[` call with all conditions included
#' @examples
#' \donttest{
#' call_extract_array(
#'   dataname = "my_array",
#'   row = call_condition_choice("my_array$SEX", "M"),
#'   column = call("c", "SEX", "AGE"),
#'   aisle = "RNAseq_rnaaccess"
#' )
#' call_extract_array(
#'   "mae_object",
#'   column = call_condition_choice("SEX", "M")
#' )
#' }
#' @return specific \code{\link[base]{Extract}} `call` for 3-dimensional array
#' @keywords internal
call_extract_array <- function(dataname = ".", row = NULL, column = NULL, aisle = NULL) {
  checkmate::assert(
    checkmate::check_string(dataname),
    checkmate::check_class(dataname, "call"),
    checkmate::check_class(dataname, "name")
  )
  checkmate::assert_multi_class(row, c("name", "call", "character", "logical", "integer"), null.ok = TRUE)
  checkmate::assert_multi_class(column, c("name", "call", "character", "logical", "integer"), null.ok = TRUE)
  checkmate::assert(
    checkmate::check_true(is.null(aisle)),
    checkmate::check_true(is.name(aisle)),
    checkmate::check_true(is.call(aisle)),
    checkmate::check_true(is.language(aisle)),
    checkmate::check_character(aisle, null.ok = TRUE),
    checkmate::check_logical(aisle, null.ok = TRUE),
    checkmate::check_integerish(aisle, null.ok = TRUE)
  )

  if (is.language(dataname)) {
    dataname <- paste(trimws(deparse(dataname, width.cutoff = 500L)), collapse = "\n")
  }
  row <- if (is.null(row)) {
    ""
  } else {
    paste(trimws(deparse(row, width.cutoff = 500L)), collapse = "\n")
  }
  column <- if (is.null(column)) {
    ""
  } else {
    paste(trimws(deparse(column, width.cutoff = 500L)), collapse = "\n")
  }
  aisle <- if (is.null(aisle)) {
    ""
  } else {
    paste(trimws(deparse(aisle, width.cutoff = 500L)), collapse = "\n")
  }

  parse(
    text = sprintf("%s[%s, %s, %s]", dataname, row, column, aisle),
    keep.source = FALSE
  )[[1]]
}

#' Get call to subset and select matrix
#'
#' Get call to subset and select matrix
#' @param dataname (`character(1)`, `name` or `call`)\cr
#' @param row (`name`, `call`, `logical`, `integer`, `character`)\cr
#'   optional, name of the `row` or condition
#' @param column (`name`, `call`, `logical`, `integer`, `character`)\cr
#'   optional, name of the `column` or condition
#' @return `[` call with all conditions included
#' @examples
#' \donttest{
#' call_extract_matrix(
#'   dataname = "my_array",
#'   row = call_condition_choice("my_array$SEX", "M"),
#'   column = call("c", "SEX", "AGE")
#' )
#' call_extract_matrix(
#'   "mae_object",
#'   column = call_condition_choice("SEX", "M")
#' )
#' }
#' @return specific \code{\link[base]{Extract}} `call` for matrix
#' @keywords internal
call_extract_matrix <- function(dataname = ".", row = NULL, column = NULL) {
  checkmate::assert(
    checkmate::check_string(dataname),
    checkmate::check_class(dataname, "call"),
    checkmate::check_class(dataname, "name")
  )
  checkmate::assert_multi_class(row, c("name", "call", "character", "logical", "integer"), null.ok = TRUE)
  checkmate::assert_multi_class(column, c("name", "call", "character", "logical", "integer"), null.ok = TRUE)

  if (is.language(dataname)) {
    dataname <- paste(trimws(deparse(dataname, width.cutoff = 500L)), collapse = "\n")
  }
  row <- if (is.null(row)) {
    ""
  } else {
    paste(trimws(deparse(row, width.cutoff = 500L)), collapse = "\n")
  }
  column <- if (is.null(column)) {
    ""
  } else {
    paste(trimws(deparse(column, width.cutoff = 500L)), collapse = "\n")
  }

  parse(
    text = sprintf("%s[%s, %s]", dataname, row, column),
    keep.source = FALSE
  )[[1]]
}


#' Compose extract call with `$` operator
#'
#' Compose extract call with `$` operator
#'
#' @param dataname (`character(1)`, `name` or `call`)\cr
#'   name of the object
#' @param varname (`character(1)`, `name` or `call`)\cr
#'   name of the slot in data
#' @param dollar (`logical(1)`)\cr
#'   whether returned call should use `$` or `[[` operator
#'
#' @return `$` or `[[` call
#' @examples
#' \donttest{
#' call_extract_list("ADSL", "SEX")
#' call_extract_list("ADSL", "named element")
#' call_extract_list("ADSL", "AGE", dollar = FALSE)
#' }
#' @keywords internal
call_extract_list <- function(dataname, varname, dollar = TRUE) {
  checkmate::assert(
    checkmate::check_string(dataname),
    checkmate::check_class(dataname, "name"),
    checkmate::check_class(dataname, "call")
  )
  checkmate::assert(
    checkmate::check_string(varname),
    checkmate::check_class(varname, "name"),
    checkmate::check_class(varname, "call")
  )
  checkmate::assert_flag(dollar)

  if (is.call(varname) && isTRUE(dollar)) {
    stop("\"dollar\" must be FALSE if \"varname\" is of type \"call\"")
  }

  if (is.character(dataname)) {
    dataname <- str2lang(dataname)
  }

  if (dollar) {
    call("$", dataname, varname)
  } else {
    call("[[", dataname, varname)
  }
}

#' Create a call using a function in a given namespace
#'
#' The arguments in ... need to be quoted because they will be evaluated otherwise
#'
#' @md
#' @param name `character`
#'   function name, possibly using namespace colon `::`, also
#'   works with `:::` (sometimes needed, but strongly discouraged)
#' @param ...
#'   arguments to pass to function with name `name`
#' @param unlist_args `list`
#'   additional arguments passed in a single list,
#'   avoids the use of `do.call` with this function
#'
#' @examples
#' \donttest{
#' print_call_and_eval <- function(x) {
#'   eval(print(x))
#' }
#'
#' print_call_and_eval(
#'   call_with_colon("base::print", x = 10)
#' )
#' \dontrun{
#' # mtcars$cyl evaluated
#' print_call_and_eval(
#'   call_with_colon("dplyr::filter", as.name("mtcars"), mtcars$cyl == 6)
#' )
#'
#' # mtcars$cyl argument not evaluated immediately (in call expression)
#' print_call_and_eval(
#'   call_with_colon("dplyr::filter", as.name("mtcars"), quote(cyl == 6))
#' )
#'
#' # does not work because argument is evaluated and the
#' # non-dplyr filter does not look inside mtcars
#' # cannot eval becausee it does not pass checks because of non-standard evaluation
#' call("filter", as.name("mtcars"), quote(cyl == 6))
#' # works, but non-dplyr filter is taken
#' call("filter", as.name("mtcars"), mtcars$cyl == 6)
#'
#' nb_args <- function(...) nargs()
#' print_call_and_eval(
#'   call_with_colon("nb_args", arg1 = 1, unlist_args = list(arg2 = 2, args3 = 3))
#' )
#' # duplicate arguments
#' print_call_and_eval(
#'   call_with_colon("nb_args", arg1 = 1, unlist_args = list(arg2 = 2, args2 = 2))
#' )
#' }
#' }
#' @keywords internal
call_with_colon <- function(name, ..., unlist_args = list()) {
  checkmate::assert_string(name)
  checkmate::assert_list(unlist_args)
  as.call(c(
    parse(text = name, keep.source = FALSE)[[1]],
    c(list(...), unlist_args)
  ))
}

#' Combine calls by operator
#'
#' Combine list of calls by specific operator
#'
#' @param operator (`character(1)`)\cr
#'   name/symbol of the operator passed as character string
#'
#' @param calls (`list` of calls)\cr
#'   list containing calls to be combined by `operator`
#'
#' @return call
#' @examples
#' \donttest{
#' calls_combine_by(
#'   "&",
#'   calls = list(
#'     call_condition_choice("SEX", "F"),
#'     call_condition_range("AGE", c(20, 50)),
#'     call_condition_choice("ARM", "ARM: A"),
#'     TRUE
#'   )
#' )
#' }
#' @return a combined `call`
#' @keywords internal
calls_combine_by <- function(operator, calls) {
  checkmate::assert_string(operator)
  # checkmate::assert_list(calls, types = "call")

  stopifnot(
    all(
      vapply(
        X = calls,
        FUN.VALUE = logical(1),
        FUN = function(x) is.language(x) || is.logical(x)
      )
    )
  )

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}
