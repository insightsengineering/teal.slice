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


#' Get hex code of the current Bootstrap theme color.
#'
#' Determines the color specification for the currently active Bootstrap color theme and returns one queried color.
#'
#' @param color `character(1)` naming one of the available theme colors
#' @param alpha either a `numeric(1)` or `character(1)` specifying transparency
#'              in the range of `0-1` or a hexadecimal value `00-ff`, respectively;
#'              set to NULL to omit adding the alpha channel
#'
#' @return Named `character(1)` containing a hexadecimal color representation.
#'
#' @examples
#' teal.slice:::fetch_bs_color("primary")
#' teal.slice:::fetch_bs_color("danger", 0.35)
#' teal.slice:::fetch_bs_color("danger", "80")
#'
#' @keywords internal
#'
fetch_bs_color <- function(color, alpha = NULL) {
  checkmate::assert_string(color)
  checkmate::assert(
    checkmate::check_number(alpha, lower = 0, upper = 1, null.ok = TRUE),
    checkmate::check_string(alpha, pattern = "[0-9a-f]{2}", null.ok = TRUE)
  )

  # locate file that describes the current theme
  ## TODO this is not ideal
  sass_file <- bslib::bs_theme()[["layers"]][[2]][["defaults"]][[1]]
  sass_file <- attr(sass_file, "sass_file_path")

  # load scss file that encodes variables
  variables_file <- readLines(sass_file)
  # locate theme color variables
  ind <- grep("// scss-docs-(start|end) theme-color-variables", variables_file)
  color_definitions <- variables_file[(ind[1] + 1L):(ind[2] - 1L)]

  # extract colors names
  color_names <- sub("(\\$)(\\w.+)(:.+)", "\\2", color_definitions)

  # verify that an available color was requested
  checkmate::assert_choice(color, color_names)

  # extract color references
  color_references <- sub("(\\$)(\\w.+)(:\\s.+\\$)(\\w.+)(\\s.+)", "\\4", color_definitions)

  # translate references to color codes
  color_specification <- structure(color_references, names = color_names)
  color_specification <- vapply(color_specification, function(x) {
    line <- grep(sprintf("^\\$%s:\\s+#\\w{6}\\s+!default", x), variables_file, value = TRUE)
    code <- sub("(.+)(#\\w{6})(\\s+.+)", "\\2", line)
    code
  }, character(1L))

  if (!is.null(alpha)) {
    if (is.numeric(alpha)) alpha <- as.hexmode(ceiling(255 * alpha))
  }

  paste0(color_specification[color], alpha)
}
