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
}

#' Resolve the expected bootstrap theme
#' @keywords internal
get_teal_bs_theme <- function() {
  bs_theme <- getOption("teal.bs_theme")
  if (is.null(bs_theme)) {
    NULL
  } else if (!inherits(bs_theme, "bs_theme")) {
    warning("teal.bs_theme has to be of a bslib::bs_theme class, the default shiny bootstrap is used.")
    NULL
  } else {
    bs_theme
  }
}

#' Include `JS` files from `/inst/js/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) pattern of files to be included, passed to `system.file`
#' @param except (`character`) vector of basename filenames to be excluded
#'
#' @return HTML code that includes `JS` files
#' @keywords internal
include_js_files <- function(pattern) {
  checkmate::assert_character(pattern, min.len = 1, null.ok = TRUE)
  js_files <- list.files(
    system.file("js", package = "teal.slice", mustWork = TRUE),
    pattern = pattern,
    full.names = TRUE
  )
  return(singleton(lapply(js_files, includeScript)))
}


# helper for unit testing filter states
# sets `choices` field to NULL in all `teal_slice`s in a `teal_slices`
#' @keywords internal
adjust_states <- function(tss) {
  checkmate::assert_class(tss, "teal_slices")

  ans <- lapply(tss, function(x) {
    x <- unclass(x)
    x$choices = NULL
    x
  })
  unname(do.call(filter_settings, lapply(ans, as.teal_slice)))
}
