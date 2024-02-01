#' Test whether variable name can be used within `Show R Code`
#'
#' Variable names containing spaces are problematic and must be wrapped in backticks.
#' Also, they should not start with a number as `R` may silently make it valid by changing it.
#' Therefore, we only allow alphanumeric characters with underscores.
#' The first character of the `name` must be an alphabetic character and can be followed by alphanumeric characters.
#'
#' @md
#'
#' @param name (`character`) vector of names to check
#' @return Returns `NULL` or raises error.
#' @keywords internal
#'
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

#' Include `JS` files from `/inst/js/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method.
#'
#' @param pattern (`character`) pattern of files to be included, passed to `system.file`
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
  singleton(lapply(js_files, includeScript))
}

#' Build concatenating call
#'
#' This function takes a vector of values and returns a `c` call. If the vector
#' has only one element, the element is returned directly.
#'
#' @param choices A vector of values.
#'
#' @return A `c` call.
#'
#' @examples
#' # use non-exported function from teal.slice
#' make_c_call <- getFromNamespace("make_c_call", "teal.slice")
#' make_c_call(1:3)
#' make_c_call(1)
#'
#' @keywords internal
make_c_call <- function(choices) {
  if (length(choices) > 1) {
    do.call("call", append(list("c"), choices))
  } else {
    choices
  }
}
