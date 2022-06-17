#' Ensure the ellipsis, ..., in method arguments are empty
#'
#' Ellipsis, ..., are needed as part of method arguments to allow for its arguments to be different from its generic's
#' arguments and for this to pass check(). Hence, ..., should always be empty. This function will check for this
#' condition.
#'
#' @param ... it should literally just be ...
#' @param stop TRUE to raise an error; FALSE will output warning message
#' @param allowed_args character vector naming arguments that are allowed in the \code{...}.
#'   to allow for unnamed arguments, let "" be one of the elements in this character vector.
#'
#' @return \code{NULL} if ... is empty
#'
#' @keywords internal
#'
#' @examples
#' method.class <- function(a, b, c, ...) {
#'   check_ellipsis(...)
#' }
#' method.class <- function(a, b, c, ...) {
#'   check_ellipsis(..., allowed_args = c("y", "z"))
#' }
check_ellipsis <- function(..., stop = FALSE, allowed_args = character(0)) {
  if (!missing(...)) {
    checkmate::assert_flag(stop)
    checkmate::assert_character(allowed_args, min.len = 0, null.ok = TRUE, any.missing = FALSE)
    args <- list(...)
    arg_names <- names(args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(args))
    }
    extra_args <- arg_names[!is.element(arg_names, allowed_args)]
    if (length(extra_args) == 0) {
      return(invisible(NULL))
    }
    message <- paste(length(extra_args), "total unused argument(s).")

    named_extra_args <- extra_args[!vapply(extra_args, identical, logical(1), "")]
    if (length(named_extra_args) > 0) {
      message <- paste0(
        message,
        " ",
        length(named_extra_args),
        " with name(s): ",
        paste(named_extra_args, collapse = ", "),
        "."
      )
    }
    if (stop) {
      stop(message)
    } else {
      warning(message)
    }
  }
}

#' Whether the variable name is good to use within Show R Code
#'
#' Spaces are problematic because the variables must be escaped with backticks.
#' Also, they should not start with a number as R may silently make it valid by changing it.
#' Therefore, we only allow alphanumeric characters with underscores.
#' The first character of the `name` must be an alphabetic character and can be followed by alphanumeric characters.
#'
#' @md
#'
#' @note
#'   The suffix '_FILTERED' is reserved for filtered data and is not
#'   allowed in the dataset name.
#'
#' @param name `character, single or vector` name to check
#' @keywords internal
#'
#' @examples
#' teal.slice:::check_simple_name("aas2df")
#' teal.slice:::check_simple_name("ADSL")
#' teal.slice:::check_simple_name("ADSLmodified")
#' teal.slice:::check_simple_name("ADSL_modified")
#' teal.slice:::check_simple_name("ADSL_2")
#' teal.slice:::check_simple_name("a1")
#' # the following fail
#' \dontrun{
#' teal.slice:::check_simple_name("1a")
#' teal.slice:::check_simple_name("ADSL.modified")
#' teal.slice:::check_simple_name("a1...")
#' teal.slice:::check_simple_name("ADSL_FILTERED")
#' }
check_simple_name <- function(name) {
  checkmate::assert_character(name, min.len = 1, any.missing = FALSE)
  if (!grepl("^[[:alpha:]][a-zA-Z0-9_]*$", name, perl = TRUE)) {
    stop(
      "name '",
      name,
      "' must only contain alphanumeric characters (with underscores)",
      " and the first character must be an alphabetic character"
    )
  }
  if (grepl("_FILTERED$", name, perl = TRUE)) {
    stop("name '", name, "' cannot end with the special string '_FILTERED'")
  }
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
#' check_in_range(c(3, 1), c(1, 3))
#' check_in_range(c(0, 3), c(1, 3))
#' check_in_range(
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
#' @param subset `collection-like` should be a subset of the second argument `choices`
#' @param choices `collection-like` superset
#' @param pre_msg `character` message to print before error should there be any errors
#' @keywords internal
#'
#' @examples
#' check_in_subset <- teal.slice:::check_in_subset
#' check_in_subset(c("a", "b"), c("a", "b", "c"))
#' \dontrun{
#' check_in_subset(c("a", "b"), c("b", "c"), pre_msg = "Error: ")
#' # truncated because too long
#' check_in_subset("a", LETTERS, pre_msg = "Error: ")
#' }
check_in_subset <- function(subset, choices, pre_msg = "") {
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

#' Set state of `FilterState`
#'
#' @description
#' Set state of `FilterState`. Function can change states in [`FilterState`] in two ways:
#' - changing `reactive` state fields which triggers observers in the `FilterState`.
#' - change state directly.
#'
#' For more, please see section "Modifying state" in [`FilterState`]
#'
#' @inheritParams init_filter_state
#' @param value (named `list`)\cr see `set_state` method in [`FilterState`].
#' @param is_reactive (`logical(1)`)\cr
#'  - `TRUE` to change `reactive` fields which triggers observers in the `FilterState`
#'  - `FALSE` to change the state directly.
#' @return invisible `NULL`
#' @keywords internal
set_state <- function(x, value, is_reactive = shiny::isRunning()) {
  checkmate::assert_class(x, "FilterState")
  checkmate::assert_list(value)
  if (is_reactive) {
    x$set_state_reactive(value)
  } else {
    x$set_state(value)
  }
  invisible(NULL)
}

