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
#' if (interactive()) {
#'   teal.slice:::check_simple_name("1a")
#'   teal.slice:::check_simple_name("ADSL.modified")
#'   teal.slice:::check_simple_name("a1...")
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

#' This function takes a vector of values and returns a `c` call. If the vector
#' has only one element, the element is returned directly.
#'
#' @param choices A vector of values.
#'
#' @return A `c` call.
#'
#' @examples
#' teal.slice:::make_c_call(1:3)
#' # [1] 1 2 3
#'
#' teal.slice:::make_c_call(1)
#' # [1] 1
#' @keywords internal
make_c_call <- function(choices) {
  if (length(choices) > 1) {
    do.call("call", append(list("c"), choices))
  } else {
    choices
  }
}

#' Initialize `FilterStates` object
#'
#' Initialize `FilterStates` object
#' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
#'   the R object which `subset` function is applied on.
#' @param data_reactive (`function(sid)`)\cr
#'   should return an object of the same type as `data` or `NULL`.
#'   This object is needed for the `FilterState` shiny module to update
#'   counts if filtered data changes.
#'   If function returns `NULL` then filtered counts
#'   are not shown. Function has to have `sid` argument being a character which
#'   is related to `sid` argument in the `get_call` method.
#' @param dataname (`character(1)`)\cr
#'   name of the data used in the expression
#'   specified to the function argument attached to this `FilterStates`.
#' @param datalabel (`character(0)` or `character(1)`)\cr
#'   text label value.
#' @param ... (optional)
#'   additional arguments for specific classes: keys.
#' @keywords internal
#' @export
#' @examples
#' library(shiny)
#' df <- data.frame(
#'   character = letters,
#'   numeric = seq_along(letters),
#'   date = seq(Sys.Date(), length.out = length(letters), by = "1 day"),
#'   datetime = seq(Sys.time(), length.out = length(letters), by = "33.33 hours")
#' )
#' rf <- teal.slice:::init_filter_states(
#'   data = df,
#'   dataname = "DF"
#' )
#' app <- shinyApp(
#'   ui = fluidPage(
#'     actionButton("clear", span(icon("xmark"), "Remove all filters")),
#'     rf$ui_add(id = "add"),
#'     rf$ui_active("states"),
#'     verbatimTextOutput("expr"),
#'   ),
#'   server = function(input, output, session) {
#'     rf$srv_add(id = "add")
#'     rf$srv_active(id = "states")
#'     output$expr <- renderText({
#'       deparse1(rf$get_call(), collapse = "\n")
#'     })
#'     observeEvent(input$clear, rf$state_list_empty())
#'   }
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
init_filter_states <- function(data,
                               data_reactive = reactive(NULL),
                               dataname,
                               datalabel = NULL,
                               ...) {
  UseMethod("init_filter_states")
}

#' @keywords internal
#' @export
init_filter_states.data.frame <- function(data, # nolint
                                          data_reactive = function(sid = "") NULL,
                                          dataname,
                                          datalabel = NULL,
                                          keys = character(0),
                                          ...) {
  DFFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel,
    keys = keys
  )
}

#' @keywords internal
#' @export
init_filter_states.matrix <- function(data, # nolint
                                      data_reactive = function(sid = "") NULL,
                                      dataname,
                                      datalabel = NULL,
                                      ...) {
  MatrixFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel
  )
}

#' @keywords internal
#' @export
init_filter_states.MultiAssayExperiment <- function(data, # nolint
                                                    data_reactive = function(sid = "") NULL,
                                                    dataname,
                                                    datalabel = "subjects",
                                                    keys = character(0),
                                                    ...) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel,
    keys = keys
  )
}

#' @keywords internal
#' @export
init_filter_states.SummarizedExperiment <- function(data, # nolint
                                                    data_reactive = function(sid = "") NULL,
                                                    dataname,
                                                    datalabel = NULL,
                                                    ...) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
  }
  SEFilterStates$new(
    data = data,
    data_reactive = data_reactive,
    dataname = dataname,
    datalabel = datalabel
  )
}

#' Gets supported filterable variable names
#'
#' Gets filterable variable names from a given object. The names match variables
#' of classes in an vector `teal.slice:::.filterable_class`.
#' @param data (`object`)\cr
#'   the R object containing elements which class can be checked through `vapply` or `apply`.
#'
#' @examples
#' df <- data.frame(
#'   a = letters[1:3],
#'   b = 1:3,
#'   c = Sys.Date() + 1:3,
#'   d = Sys.time() + 1:3,
#'   z = complex(3)
#' )
#' teal.slice:::get_supported_filter_varnames(df)
#' @return `character` vector of matched element names
#' @keywords internal
get_supported_filter_varnames <- function(data) {
  UseMethod("get_supported_filter_varnames")
}

#' @keywords internal
#' @export
get_supported_filter_varnames.default <- function(data) { # nolint
  is_expected_class <- vapply(
    X = data,
    FUN = function(x) any(class(x) %in% .filterable_class),
    FUN.VALUE = logical(1)
  )
  names(is_expected_class[is_expected_class])
}

#' @keywords internal
#' @export
get_supported_filter_varnames.matrix <- function(data) { # nolint
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(colnames(data))) {
    colnames(data)
  } else {
    character(0)
  }
}

#' @keywords internal
#' @export
get_supported_filter_varnames.MultiAssayExperiment <- function(data) { # nolint
  data <- SummarizedExperiment::colData(data)
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(names(data))) {
    names(data)
  } else {
    character(0)
  }
}

#' @title Returns a `choices_labeled` object
#'
#' @param data (`data.frame`, `DFrame`, `list`)\cr
#'   where labels can be taken from in case when `varlabels` is not specified.
#'   `data` must be specified if `varlabels` is not specified.
#' @param choices (`character`)\cr
#'  the vector of chosen variables
#' @param varlabels (`character`)\cr
#'  the labels of variables in data
#' @param keys (`character`)\cr
#'  the names of the key columns in data
#' @return `character(0)` if choices are empty; a `choices_labeled` object otherwise
#' @keywords internal
data_choices_labeled <- function(data,
                                 choices,
                                 varlabels = teal.data::col_labels(data, fill = TRUE),
                                 keys = character(0)) {
  if (length(choices) == 0) {
    return(character(0))
  }
  choice_types <- stats::setNames(variable_types(data = data, columns = choices), choices)
  choice_types[keys] <- "primary_key"

  choices_labeled(
    choices = choices,
    labels = unname(varlabels[choices]),
    types = choice_types[choices]
  )
}

get_varlabels <- function(data) {
  if (!is.array(data)) {
    vapply(
      colnames(data),
      FUN = function(x) {
        label <- attr(data[[x]], "label")
        if (is.null(label)) {
          x
        } else {
          label
        }
      },
      FUN.VALUE = character(1)
    )
  } else {
    character(0)
  }
}
