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

#' Destroys inputs and observers stored in `private$session_bindings`
#'
#' @description
#' Call a `destroy` method to remove `observer` and `input` from obsolete session which happens
#' when `filter_panel_srv` is called again in new `FilteredData` object.
#' Inputs are not stored directly in a field as they don't have `destroy` method. Instead, we
#' store callback `destroy` function for inputs which removes bindings from a `session`.
#' @param self,private slots of a `R6` class
#' @return `NULL` invisibly
#' @keywords internal
.finalize_session_bindings <- function(self, private) {
  if (length(private$session_bindings) > 0) lapply(private$session_bindings, function(x) x$destroy())
  invisible(NULL)
}



#' Encodes ids to be used in JavaScript and Shiny
#'
#' @description
#' Replaces non-ASCII characters into a format that can be used in HTML,
#' JavaScript and Shiny.
#'
#' When the id has a character that is not allowed, it is replaced with `"_"`
#' and a hash of the original id is added to the beginning of the id.
#'
#' @param id (`character(1)`) The id string.
#'
#' @return Sanitized string that removes special characters and spaces.
#'
#' @keywords internal
sanitize_id <- function(id) {
  id_converted <- make.names(id)
  if (identical(make.names(id), id)) {
    return(id)
  }
  id_converted <- gsub("\\.", "_", id_converted)
  if (!grepl("^X", id)) {
    id_converted <- gsub("^X", "", id_converted)
  }
  paste0(substr(rlang::hash(as.character(id)), 1, 4), "_", id_converted)
}

#' `NS` wrapper to sanitize ids for shiny
#'
#' Special characters and spaces are not allowed in shiny ids (in JS)
#'
#' @noRd
NS <- function(namespace, id = NULL) { # nolint: object_name.
  if (!missing(id)) {
    return(shiny::NS(namespace, sanitize_id(id)))
  }

  function(id) {
    shiny::NS(namespace, sanitize_id(id))
  }
}

#' `moduleServer` wrapper to sanitize ids for shiny
#'
#' Special characters and spaces are not allowed in shiny ids (in JS)
#'
#' @noRd
moduleServer <- function(id, module, session = getDefaultReactiveDomain()) { # nolint: object_name.
  id <- sanitize_id(id)
  shiny::moduleServer(id, module, session)
}
