#' Initialize `FilterState`
#'
#' Initializes a `FilterState` object corresponding to the class of the filtered variable.
#'
#' @param x (`vector`)
#'   variable to be filtered.
#' @param x_reactive (`reactive`)
#'   returning vector of the same type as `x`. Is used to update
#'   counts following the change in values of the filtered dataset.
#'   If it is set to `reactive(NULL)` then counts based on filtered
#'   dataset are not shown.
#' @param slice (`teal_slice`)
#'   specification of this filter state.
#'   `teal_slice` is stored in the object and `set_state` directly manipulates values within `teal_slice`.
#'   `get_state` returns `teal_slice` object which can be reused in other places.
#'   Note that `teal_slice` is a `reactiveValues`, which means it has reference semantics, i.e.
#'   changes made to an object are automatically reflected in all places that refer to the same `teal_slice`.
#' @param extract_type (`character`)
#'   specifying whether condition calls should be prefixed by `dataname`. Possible values:
#' - `character(0)` (default) `varname` in the condition call will not be prefixed
#' - `"list"` `varname` in the condition call will be returned as `<dataname>$<varname>`
#' - `"matrix"` `varname` in the condition call will be returned as `<dataname>[, <varname>]`
#'
#' @examples
#' # use non-exported function from teal.slice
#' init_filter_state <- getFromNamespace("init_filter_state", "teal.slice")
#'
#' library(shiny)
#'
#' filter_state <- init_filter_state(
#'   x = c(1:10, NA, Inf),
#'   x_reactive = reactive(c(1:10, NA, Inf)),
#'   slice = teal_slice(
#'     varname = "varname",
#'     dataname = "dataname"
#'   ),
#'   extract_type = "matrix"
#' )
#'
#' isolate(filter_state$get_call())
#'
#' # working filter in an app
#'
#' ui <- bslib::page_fluid(
#'   filter_state$ui(id = "app"),
#'   verbatimTextOutput("call")
#' )
#' server <- function(input, output, session) {
#'   filter_state$server("app")
#'
#'   output$call <- renderText(
#'     deparse1(filter_state$get_call(), collapse = "\n")
#'   )
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @return `FilterState` object
#' @keywords internal
init_filter_state <- function(x,
                              x_reactive = reactive(NULL),
                              slice,
                              extract_type = character(0)) {
  checkmate::assert_class(x_reactive, "reactive")
  checkmate::assert_character(extract_type, max.len = 1, any.missing = FALSE)
  checkmate::assert_class(slice, "teal_slice")
  if (length(extract_type) == 1) {
    checkmate::assert_choice(extract_type, choices = c("list", "matrix"))
  }

  if (all(is.na(x))) {
    EmptyFilterState$new(
      x = x,
      x_reactive = x_reactive,
      slice = slice,
      extract_type = extract_type
    )
  } else {
    UseMethod("init_filter_state")
  }
}

#' @keywords internal
#' @export
init_filter_state.default <- function(x,
                                      x_reactive = reactive(NULL),
                                      slice,
                                      extract_type = character(0)) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    extract_type = extract_type,
    slice
  )

  do.call(FilterState$new, args)
}

#' @keywords internal
#' @export
init_filter_state.logical <- function(x,
                                      x_reactive = reactive(NULL),
                                      slice,
                                      extract_type = character(0)) {
  LogicalFilterState$new(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.numeric <- function(x,
                                      x_reactive = reactive(NULL),
                                      slice,
                                      extract_type = character(0)) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )

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
                                     slice,
                                     extract_type = character(0)) {
  ChoicesFilterState$new(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.character <- function(x,
                                        x_reactive = reactive(NULL),
                                        slice,
                                        extract_type = character(0)) {
  ChoicesFilterState$new(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.Date <- function(x,
                                   x_reactive = reactive(NULL),
                                   slice,
                                   extract_type = character(0)) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )

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
                                      slice,
                                      extract_type = character(0)) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )

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
                                      slice,
                                      extract_type = character(0)) {
  args <- list(
    x = x,
    x_reactive = x_reactive,
    slice = slice,
    extract_type = extract_type
  )

  if (length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup")) {
    do.call(ChoicesFilterState$new, args)
  } else {
    do.call(DatetimeFilterState$new, args)
  }
}


#' Initialize a `FilterStateExpr` object
#'
#' @param slice (`teal_slice_expr`)
#'   specifying this filter state.
#'
#' @return `FilterStateExpr` object
#' @keywords internal
init_filter_state_expr <- function(slice) {
  FilterStateExpr$new(slice)
}


#' Get hex code of the current Bootstrap theme color.
#'
#' Determines the color specification for the currently active Bootstrap color theme and returns one queried color.
#'
#' @param color (`character(1)`) naming one of the available theme colors
#' @param alpha either a `numeric(1)` or `character(1)` specifying transparency
#'              in the range of `0-1` or a hexadecimal value `00-ff`, respectively;
#'              set to NULL to omit adding the alpha channel
#'
#' @return Named `character(1)` containing a hexadecimal color representation.
#'
#' @examples
#' fetch_bs_color <- getFromNamespace("fetch_bs_color", "teal.slice")
#' fetch_bs_color("primary")
#' fetch_bs_color("danger", 0.35)
#' fetch_bs_color("danger", "80")
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
  sass_file <- if (utils::packageVersion("bslib") < as.package_version("0.5.1.9000")) {
    bslib::bs_theme()[["layers"]][[2]][["defaults"]][[1]]
  } else {
    bslib::bs_theme()[["layers"]][[2]][["defaults"]][[1]][[1]]
  }
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
