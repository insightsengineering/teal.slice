#' Initializes `FilterState`
#'
#' Initializes `FilterState` depending on a variable class.\cr
#' @param x (`vector`)\cr
#'   values of the variable used in filter
#'
#' @param varname (`character(1)`, `name`)\cr
#'   name of the variable
#'
#' @param varlabel (`character(1)`)\cr
#'   label of the variable (optional).
#'
#' @param input_dataname (`name` or `call`)\cr
#'   name of dataset where `x` is taken from. Must be specified if `extract_type` argument
#'   is not empty.
#'
#' @param extract_type (`character(0)`, `character(1)`)\cr
#' whether condition calls should be prefixed by dataname. Possible values:
#' \itemize{
#' \item{`character(0)` (default)}{ `varname` in the condition call will not be prefixed}
#' \item{`"list"`}{ `varname` in the condition call will be returned as `<input_dataname>$<varname>`}
#' \item{`"matrix"`}{ `varname` in the condition call will be returned as `<input_dataname>[, <varname>]`}
#' }
#' @keywords internal
#'
#' @examples
#' filter_state <- teal.slice:::RangeFilterState$new(
#'   c(1:10, NA, Inf),
#'   varname = "x",
#'   varlabel = "Pretty name",
#'   input_dataname = as.name("dataname"),
#'   extract_type = "matrix"
#' )
#'
#' filter_state$get_varname()
#' filter_state$get_varlabel()
#' isolate(filter_state$get_call())
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     isolate(filter_state$ui(id = "app")),
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
                              varname,
                              varlabel = attr(x, "label"),
                              input_dataname = NULL,
                              extract_type = character(0)) {
  checkmate::assert(
    checkmate::check_string(varname),
    checkmate::check_class(varname, "name")
  )
  checkmate::assert_character(varlabel, max.len = 1L, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_multi_class(input_dataname, c("name", "call"), null.ok = TRUE)
  checkmate::assert_character(extract_type, max.len = 1L, any.missing = FALSE)
  if (length(extract_type) == 1)
    checkmate::assert_choice(extract_type, choices = c("list", "matrix"))
  if (length(extract_type) == 1 && is.null(input_dataname))
    stop("if extract_type is specified, input_dataname must also be specified")

  if (is.null(varlabel)) varlabel <- character(0L)

  if (all(is.na(x))) {
    return(
      EmptyFilterState$new(
        x = x,
        varname = varname,
        varlabel = varlabel,
        input_dataname = input_dataname,
        extract_type = extract_type
      )
    )
  }
  UseMethod("init_filter_state")
}

#' @keywords internal
#' @export
init_filter_state.default <- function(x,
                                      varname,
                                      varlabel = attr(x, "label"),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  FilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.logical <- function(x,
                                      varname,
                                      varlabel = attr(x, "label"),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  LogicalFilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.numeric <- function(x,
                                      varname,
                                      varlabel = attr(x, "label"),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  if (length(unique(x[!is.na(x)])) < getOption("teal.threshold_slider_vs_checkboxgroup")) {
    ChoicesFilterState$new(
      x = x,
      varname = varname,
      varlabel = varlabel,
      input_dataname = input_dataname,
      extract_type = extract_type
    )
  } else {
    RangeFilterState$new(
      x = x,
      varname = varname,
      varlabel = varlabel,
      input_dataname = input_dataname,
      extract_type = extract_type
    )
  }
}

#' @keywords internal
#' @export
init_filter_state.factor <- function(x,
                                     varname,
                                     varlabel = attr(x, "label"),
                                     input_dataname = NULL,
                                     extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  ChoicesFilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.character <- function(x,
                                        varname,
                                        varlabel = attr(x, "label"),
                                        input_dataname = NULL,
                                        extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  ChoicesFilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.Date <- function(x,
                                   varname,
                                   varlabel = attr(x, "label"),
                                   input_dataname = NULL,
                                   extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  DateFilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.POSIXct <- function(x,
                                      varname,
                                      varlabel = attr(x, "label"),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  DatetimeFilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
}

#' @keywords internal
#' @export
init_filter_state.POSIXlt <- function(x,
                                      varname,
                                      varlabel = attr(x, "label"),
                                      input_dataname = NULL,
                                      extract_type = character(0)) {
  if (is.null(varlabel)) varlabel <- character(0)
  DatetimeFilterState$new(
    x = x,
    varname = varname,
    varlabel = varlabel,
    input_dataname = input_dataname,
    extract_type = extract_type
  )
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
