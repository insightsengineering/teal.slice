#' Initializes `FilterState`
#'
#' Initializes `FilterState` depending on a variable class.\cr
#' @param x (`vector`)\cr
#'   values of the variable used in filter
#' @param x_reactive (`reactive`)\cr
#'   returning vector of the same type as `x`. Is used to update
#'   counts following the change in values of the filtered dataset.
#'   If it is set to `reactive(NULL)` then counts based on filtered
#'   dataset are not shown.
#' @param dataname (`character(1)`)\cr
#'   optional name of dataset where `x` is taken from. Must be specified
#'   if `extract_type` argument is not empty.
#' @param varname (`character(1)`)\cr
#'   name of the variable.
#' @param choices (`atomic`)\cr
#'   vector specifying allowed selection values
#' @param selected (`atomic`, `NULL`)\cr
#'   vector specifying selection
#' @param keep_na (`logical(1)`, `NULL`)\cr
#'   flag specifying whether to keep missing values
#' @param keep_inf (`logical(1)`, `NULL`)\cr
#'   flag specifying whether to keep infinite values
#' @param fixed (`logical(1)`)\cr
#'   flag specifying whether the `FilterState` is initiated fixed
#' @param disabled (`logical(1)`)\cr
#'   flag specifying whether the `FilterState` is initiated disabled
#' @param extract_type (`character(0)`, `character(1)`)\cr
#' whether condition calls should be prefixed by dataname. Possible values:
#' \itemize{
#' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
#' \item{`"list"`}{ `varname` in the condition call will be returned as `<dataname>$<varname>`}
#' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<dataname>[, <varname>]`}
#' }
#' @param ... additional arguments to be saved as a list in `private$extras` field
#'
#' @keywords internal
#'
#' @examples
#' filter_state <- teal.slice:::RangeFilterState$new(
#'   x = c(1:10, NA, Inf),
#'   x_reactive = reactive(c(1:10, NA, Inf)),
#'   varname = "x",
#'   dataname = "dataname",
#'   extract_type = "matrix"
#' )
#'
#' shiny::isolate(filter_state$get_call())
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     filter_state$ui(id = "app"),
#'     verbatimTextOutput("call")
#'   ),
#'   server = function(input, output, session) {
#'     filter_state$server("app")
#'
#'     output$call <- renderText(
#'       deparse1(filter_state$get_call(), collapse = "\n")
#'     )
#'   }
#' )
#' }
#' @return `FilterState` object
init_filter_state <- function(x,
                              x_reactive = reactive(NULL),
                              dataname,
                              varname,
                              choices = NULL,
                              selected = NULL,
                              keep_na = NULL,
                              keep_inf = NULL,
                              fixed = FALSE,
                              disabled = FALSE,
                              extract_type = character(0),
                              ...) {
  checkmate::assert_class(x_reactive, "reactive")
  checkmate::assert_string(dataname)
  checkmate::assert_string(varname)
  checkmate::assert_flag(keep_na, null.ok = TRUE)
  checkmate::assert_flag(keep_inf, null.ok = TRUE)
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(disabled)
  checkmate::assert_character(extract_type, max.len = 1, any.missing = FALSE)
  if (length(extract_type) == 1) {
    checkmate::assert_choice(extract_type, choices = c("list", "matrix"))
  }

  if (all(is.na(x))) {
    args <- list(
      x = x,
      x_reactive = x_reactive,
      dataname = dataname,
      varname = varname,
      choices = choices,
      selected = selected,
      keep_na = keep_na,
      keep_inf = keep_inf,
      fixed = fixed,
      disabled = disabled,
      extract_type = extract_type
    )
    args <- append(args, list(...))

    do.call(EmptyFilterState$new, args)
  } else {
    UseMethod("init_filter_state")
  }
}

#' @keywords internal
#' @export
init_filter_state.default <- function(x,
                                      x_reactive = reactive(NULL),
                                      dataname,
                                      varname,
                                      choices = NULL,
                                      selected = NULL,
                                      keep_na = NULL,
                                      keep_inf = NULL,
                                      fixed = FALSE,
                                      disabled = FALSE,
                                      extract_type = character(0),
                                      ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  do.call(FilterState$new, args)
}

#' @keywords internal
#' @export
init_filter_state.logical <- function(x,
                                      x_reactive = reactive(NULL),
                                      dataname,
                                      varname,
                                      choices = NULL,
                                      selected = NULL,
                                      keep_na = NULL,
                                      keep_inf = NULL,
                                      fixed = FALSE,
                                      disabled = FALSE,
                                      extract_type = character(0),
                                      ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  do.call(LogicalFilterState$new, args)
}

#' @keywords internal
#' @export
init_filter_state.numeric <- function(x,
                                      x_reactive = reactive(NULL),
                                      dataname,
                                      varname,
                                      choices = NULL,
                                      selected = NULL,
                                      keep_na = NULL,
                                      keep_inf = NULL,
                                      fixed = FALSE,
                                      disabled = FALSE,
                                      extract_type = character(0),
                                      ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  if (length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup")) {
    do.call(ChoicesFilterState$new, args)
  } else {
    do.call(RangeFilterState$new, args)
  }
}

#' @keywords internal
#' @export
init_filter_state.factor <- function(x,
                                     x_reactive = reactive(NULL),
                                     dataname,
                                     varname,
                                     choices = NULL,
                                     selected = NULL,
                                     keep_na = NULL,
                                     keep_inf = NULL,
                                     fixed = FALSE,
                                     disabled = FALSE,
                                     extract_type = character(0),
                                     ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  do.call(ChoicesFilterState$new, args)
}

#' @keywords internal
#' @export
init_filter_state.character <- function(x,
                                        x_reactive = reactive(NULL),
                                        dataname,
                                        varname,
                                        choices = NULL,
                                        selected = NULL,
                                        keep_na = NULL,
                                        keep_inf = NULL,
                                        fixed = FALSE,
                                        disabled = FALSE,
                                        extract_type = character(0),
                                        ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  do.call(ChoicesFilterState$new, args)
}

#' @keywords internal
#' @export
init_filter_state.Date <- function(x,
                                   x_reactive = reactive(NULL),
                                   dataname,
                                   varname,
                                   choices = NULL,
                                   selected = NULL,
                                   keep_na = NULL,
                                   keep_inf = NULL,
                                   fixed = FALSE,
                                   disabled = FALSE,
                                   extract_type = character(0),
                                   ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  if (length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup")) {
    do.call(ChoicesFilterState$new, args)
  } else {
    do.call(DateFilterState$new, args)
  }
}

#' @keywords internal
#' @export
init_filter_state.POSIXct <- function(x,
                                      x_reactive = reactive(NULL),
                                      dataname,
                                      varname,
                                      choices = NULL,
                                      selected = NULL,
                                      keep_na = NULL,
                                      keep_inf = NULL,
                                      fixed = FALSE,
                                      disabled = FALSE,
                                      extract_type = character(0),
                                      ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  if (length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup")) {
    do.call(ChoicesFilterState$new, args)
  } else {
    do.call(DatetimeFilterState$new, args)
  }
}

#' @keywords internal
#' @export
init_filter_state.POSIXlt <- function(x,
                                      x_reactive = reactive(NULL),
                                      dataname,
                                      varname,
                                      choices = NULL,
                                      selected = NULL,
                                      keep_na = NULL,
                                      keep_inf = NULL,
                                      fixed = FALSE,
                                      disabled = FALSE,
                                      extract_type = character(0),
                                      ...) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled,
    extract_type = extract_type
  )
  args <- append(args, list(...))

  do.call(FilterState$new, args)

  if (length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup")) {
    do.call(ChoicesFilterState$new, args)
  } else {
    do.call(DatetimeFilterState$new, args)
  }
}


#' Initialize a `FilterStateExpr` object
#'
#' Initialize a `FilterStateExpr` object
#' @param id (`character(1)`)\cr
#'   identifier of the filter
#' @param title (`character(1)`)\cr
#'   title of the filter
#' @param dataname (`character(1)`)\cr
#'   name of the dataset where `expr` could be executed on.
#' @param expr (`character(1)`)\cr
#'   logical expression written in executable way. By "executable" means
#'   that `subset` call should be able to evaluate this without failure. For
#'   example `MultiAssayExperiment::subsetByColData` requires variable names prefixed
#'   by dataname (e.g. `data$var1 == "x" & data$var2 > 0`). For `data.frame` call
#'   can be written without prefixing `var1 == "x" & var2 > 0`.
#' @param disabled (`logical(1)`)\cr
#'   flag specifying whether the `FilterState` is initiated disabled
#' @param ... additional arguments to be saved as a list in `private$extras` field
#'
#' @return `FilterStateExpr` object
#' @keywords internal
init_filter_state_expr <- function(id, title, dataname, expr, disabled, ...) {
  FilterStateExpr$new(id = id, title = title, dataname = dataname, expr = expr, disabled = disabled, ...)
}

#' Check that a given range is valid
#'
#' @param subinterval (`numeric` or `date`)\cr vector of length 2 to be compared against the full range.
#' @param range (`numeric` or `date`)\cr vector of length 2 containing the full range to validate against.
#' @param pre_msg `character` message to print before error for additional context.
#'
#' @return `NULL` if `subinterval` is a valid range or error with message otherwise.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' teal.slice:::check_in_range(c(3, 1), c(1, 3))
#' teal.slice:::check_in_range(c(0, 3), c(1, 3))
#' teal.slice:::check_in_range(
#'   c(as.Date("2020-01-01"), as.Date("2020-01-20")),
#'   c(as.Date("2020-01-01"), as.Date("2020-01-02"))
#' )
#' }
check_in_range <- function(subinterval, range, pre_msg = "") {
  epsilon <- .Machine$double.eps^0.5 # needed for floating point arithmetic; same value as in base::all.equal()
  if ((length(subinterval) != 2)) {
    stop(
      sprintf(
        "%s range length should be 2 while it is %s",
        pre_msg,
        length(subinterval)
      )
    )
  }
  if (subinterval[[2]] + epsilon < subinterval[[1]]) {
    stop(sprintf(
      "%s unexpected: the upper bound of the range lower than the lower bound \n %s < %s",
      pre_msg,
      subinterval[[2]],
      subinterval[[1]]
    ))
  }

  if ((subinterval[[1]] + epsilon < range[[1]]) || (subinterval[[2]] - epsilon > range[[2]])) {
    stop(
      sprintf(
        "%s range (%s) not valid for full range (%s)",
        pre_msg, toString(subinterval), toString(range)
      )
    )
  }
}

#' Check that one set is a subset of another
#'
#' Raises an error message if not and says which elements are not in the allowed `choices`.
#'
#' @param subset,choices atomic vectors
#' @param pre_msg `character` message to print before error should there be any errors
#' @keywords internal
#'
#' @examples
#' \donttest{
#' teal.slice:::check_in_subset(c("a", "b"), c("a", "b", "c"))
#' \dontrun{
#' teal.slice:::check_in_subset(c("a", "b"), c("b", "c"), pre_msg = "Error: ")
#' # truncated because too long
#' teal.slice:::check_in_subset("a", LETTERS, pre_msg = "Error: ")
#' }
#' }
check_in_subset <- function(subset, choices, pre_msg = "") {
  checkmate::assert_atomic(subset)
  checkmate::assert_atomic(choices)
  checkmate::assert_string(pre_msg)

  subset <- unique(subset)
  choices <- unique(choices)

  if (any(!(subset %in% choices))) {
    stop(paste0(
      pre_msg,
      "(", toString(subset[!(subset %in% choices)], width = 30), ")",
      " not in valid choices ",
      "(", toString(choices, width = 30), ")"
    ), call. = FALSE)
  }
  return(invisible(NULL))
}

#' Find containing limits for interval.
#'
#' Given an interval and a numeric vector,
#' find the smallest interval within the numeric vector that contains the interval.
#'
#' This is a helper function for `RangeFilterState` that modifies slider selection
#' so that the _subsetting call_ includes the value specified by the filter API call.
#'
#' Regardless of the underlying numeric data, the slider always presents 100 steps.
#' The ticks on the slider do not represent actual observations but rather borders between virtual bins.
#' Since the value selected on the slider is passed to `private$selected` and that in turn
#' updates the slider selection, programmatic selection of arbitrary values may inadvertently shift
#' the selection to the closest tick, thereby dropping the actual value set (if it exists in the data).
#'
#' This function purposely shifts the selection to the closest ticks whose values form an interval
#' that will contain the interval defined by the filter API call.
#'
#' @param x `numeric(2)` interval to contain
#' @param range `numeric(>=2)` vector of values to contain `x` in
#'
#' @return Numeric vector of length 2 that lies within `range`.
#'
#' @keywords internal
#'
#' @examples
#' \donttest{
#' ticks <- 1:10
#' values1 <- c(3, 5)
#' teal.slice:::contain_interval(values1, ticks)
#' values2 <- c(3.1, 5.7)
#' teal.slice:::contain_interval(values2, ticks)
#' values3 <- c(0, 20)
#' teal.slice:::contain_interval(values3, ticks)
#'}
contain_interval <- function(x, range) {
  checkmate::assert_numeric(x, len = 2L, any.missing = FALSE, sorted = TRUE)
  checkmate::assert_numeric(range, min.len = 2L, any.missing = FALSE, sorted = TRUE)

  x[1] <- Find(function(i) i <= x[1], range, nomatch = min(range), right = TRUE)
  x[2] <- Find(function(i) i >= x[2], range, nomatch = max(range))
  x
}


#' Formats selected values of a RangeFilterState for display in the header summary.
#'
#' If a number has more significant digits than the threshhold, it will be
#'  formatted in scientific notation. The resulting number will have 'threshold'
#'  significant digits. If any `NA`s are present, they will be converted to the
#'  character "NA". Similarly `Inf` and `-Inf` will be converted to the strings
#'  "Inf" and "-Inf" respectively.
#'
#'
#' @param values Vector of values to format. Can contain `NA`, `Inf`, `-Inf`.
#' @param threshold Number of significant digits above which the number will be
#'  formatted in scientific notation.
#'
#' @return Vector of `length(values)` as a string suitable for display.
#' @keywords internal
#' @noRd
format_range_for_summary <- function(values, threshold = 4) {
  checkmate::assert_numeric(values, min.len = 1)
  checkmate::assert_number(threshold, lower = 1, finite = TRUE)

  ops <- options(scipen = 9999)
  on.exit(options(ops))

  # convert to a string representation
  values_str <- vapply(
    values,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(value) {
      if (is.na(value) || is.finite(value)) {
        format(value)
      } else {
        as.character(value)
      }
    })

  n_digits <- n_sig_digits(values_str)

  mapply(
    values_str,
    n_digits,
    USE.NAMES = FALSE,
    MoreArgs = list(threshold = threshold),
    FUN = function(value, digits, threshold) {
      if (digits == -1) { # special case for Inf, -Inf, NA
        value
      } else if (digits > threshold) {
        val <- format(as.numeric(value), digits = threshold, scientific = TRUE)
        val <- sub("e", "E", val)
        val
      } else {
        value
      }
    }
  )
}

#' Count the number of significant digits in a number.
#'
#' Adapted from https://www.r-bloggers.com/2010/04/significant-figures-in-r-and-info-zeros/
#'  The supplied vector should be numbers represented as a character. `NA`, `Inf`,
#'  and `-Inf` should be coded as the strings "NA", "Inf", and "-Inf". In these
#'  cases, a count of -1 is returned.
#'
#' @param nums A vector of numbers that have been converted to character.
#'
#' @return Vector of `length(nums)` with counts of significant digits.
#' @keywords internal
#' @noRd
n_sig_digits <- function(nums) {
  checkmate::assert_character(nums, any.missing = FALSE)

  vapply(nums, FUN.VALUE = numeric(1), USE.NAMES = FALSE, FUN = function(num) {

    if (grepl("e", num)) return(-1)
    if (num == "NA") return(-1)
    if (num == "Inf" || num == "-Inf") return(-1)

    sig_digits <- 1
    i <- 0

    if (grepl("\\.", num)) {

      num_split <- unlist(strsplit(num, "\\."))
      num_str <- paste(num_split[1], num_split[2], sep = "")
      current_n_digits <- nchar(num_str)

      while (i < current_n_digits) {

        if (substr(num_str, i + 1, i + 1) == "0") {
          i <- i + 1
          next
        } else {
          sig_digits <- current_n_digits - i
          break
        }
      }
    } else {

      num_str <- num
      current_n_digits <- nchar(num_str)

      while (i < current_n_digits) {
        if (substr(num_str, current_n_digits - i, current_n_digits - i) == "0") {
          i <- i + 1
          next
        } else {
          sig_digits <- current_n_digits - i
          break
        }
      }
    }

    return(sig_digits)
  })
}
