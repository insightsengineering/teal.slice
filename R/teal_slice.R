#' Filter specification
#'
#' @details
#' `teal_slice` object fully describes filter state and can be used to create,
#' modify, and delete a filter state. A `teal_slice` contains a number of common fields
#' (all named arguments of `teal_slice`), some of which are mandatory, but only
#' `dataname` and  either `varname` or `expr` must be specified, while the others have default
#' values.
#'
#' Setting any of the other values to NULL means that those properties will not be modified
#' (when setting an existing state) or that they will be determined by data (when creating new a new one).
#' Entire object is `FilterState` class member and can be accessed with `FilterState$get_state()`.
#'
#' A `teal_slice` can come in two flavors:
#' 1. `teal_slice_var` -
#' this describes a typical interactive filter that refers to a single variable, managed by the `FilterState` class.
#' This class is created when `varname is specified.
#' The object retains all fields specified in the call. `id` can be created by default and need not be specified.
#' 2. `teal_slice_expr` -
#' this describes a filter state that refers to an expression, which can potentially include multiple variables,
#' managed by the `FilterStateExpr` class.
#' This class is created when `expr` is specified.
#' `dataname` and `locked` are retained, `fixed` is set to `TRUE`, `id` becomes mandatory, `title`
#' remains optional, while other arguments are disregarded.
#'
#' A teal_slice can be passed `FilterState`/`FilterStateExpr` constructors to instantiate an object.
#' It can also be passed to `FilterState$set_state` to modify the state.
#' However, once a `FilterState` is created, only the mutable features can be set with a teal_slice:
#' `selected`, `keep_na` and `keep_inf`.
#'
#' Special consideration is given to two fields: `fixed` and `locked`.
#' These are always immutable logical flags that default to `FALSE`.
#' In a `FilterState` instantiated with `fixed = TRUE` the features
#' `selected`, `keep_na`, `keep_inf` cannot be changed.
#' Note that a `FilterStateExpr` is always considered to have `fixed = TRUE`.
#' A `FilterState` instantiated with `locked = TRUE` cannot be removed.
#'
#' @section Filters in `SumarizedExperiment` and `MultiAssayExperiment` objects:
#'
#' To establish a filter on a column in a `data.frame`, `dataname` and `varname` are sufficient.
#' `MultiAssayExperiment` objects can be filtered either on their `colData` slot (which contains subject information)
#' or on their experiments, which are stored in the `experimentList` slot.
#' For filters referring to `colData` no extra arguments are needed.
#' If a filter state is created for an experiment, that experiment name must be specified in the `experiment` argument.
#' Furthermore, to specify filter for an `SummarizedExperiment` one must also set `arg`
#' (`"subset"`  or `"select`, arguments in the [subset()] function for `SummarizedExperiment`)
#' in order to determine whether the filter refers to the `SE`'s `rowData` or `colData`.
#'
#' @param dataname (`character(1)`) name of data set
#' @param varname (`character(1)`) name of variable
#' @param id (`character(1)`) identifier of the filter. Must be specified when `expr` is set.
#'  When `varname` is specified then `id` is set to `"{dataname} {varname}"` by default.
#' @param expr (`character(1)`) string providing a logical expression.
#'  Must be a valid R expression which can be evaluated in the context of the data set.
#'  For a `data.frame` `var == "x"` is sufficient, but `MultiAssayExperiment::subsetByColData`
#'  requires `dataname` prefix, *e.g.* `data$var == "x"`.
#' @param choices (optional `vector`) specifying allowed choices;
#' When specified it should be a subset of values in variable denoted by `varname`;
#' Type and size depends on variable type.
#' @param selected (optional `vector`) of selected values from `choices`;
#' Type and size depends on variable type.
#' @param multiple (optional `logical(1)`) flag specifying whether more than one value can be selected;
#' only applicable to `ChoicesFilterState` and `LogicalFilterState`
#' @param keep_na (optional `logical(1)`) flag specifying whether to keep missing values
#' @param keep_inf (optional `logical(1)`) flag specifying whether to keep infinite values
#' @param fixed (`logical(1)`) flag specifying whether to fix this filter state (forbid setting state)
#' @param locked (`logical(1)`) flag specifying whether to lock this filter state (forbid removing and inactivating)
#' @param title (optional `character(1)`) title of the filter. Ignored when `varname` is set.
#' @param ... in `teal_slice` method these are additional arguments which can be handled by extensions
#'  of `teal.slice` classes. In other methods these are further arguments passed to or from other methods.
#' @param x (`teal.slice`) object.
#' @param show_all (`logical(1)`) indicating whether to show all fields. If set to `FALSE`,
#'  only non-NULL elements will be printed.
#' @param trim_lines (`logical(1)`) indicating whether to trim lines when printing.
#'
#' @return A `teal.slice` object. Depending on whether `varname` or `expr` was specified, the resulting
#' `teal_slice` also receives class `teal_slice_var` or `teal_slice_expr`, respectively.
#'
#' @examples
#' x1 <- teal_slice(
#'   dataname = "data",
#'   id = "Female adults",
#'   expr = "SEX == 'F' & AGE >= 18",
#'   title = "Female adults"
#' )
#' x2 <- teal_slice(
#'   dataname = "data",
#'   varname = "var",
#'   choices = c("F", "M", "U"),
#'   selected = "F",
#'   keep_na = TRUE,
#'   keep_inf = TRUE,
#'   fixed = FALSE,
#'   locked = FALSE,
#'   multiple = TRUE,
#'   id = "Gender",
#'   extra_arg = "extra"
#' )
#'
#' is.teal_slice(x1)
#' as.list(x1)
#' as.teal_slice(list(dataname = "a", varname = "var"))
#' format(x1)
#' format(x1, show_all = TRUE, trim_lines = FALSE)
#' print(x1, show_all = FALSE, trim_lines = FALSE)
#' print(x1, show_all = FALSE, trim_lines = TRUE)
#'
#' @export
teal_slice <- function(dataname,
                       varname,
                       id,
                       expr,
                       choices = NULL,
                       selected = NULL,
                       keep_na = NULL,
                       keep_inf = NULL,
                       fixed = FALSE,
                       locked = FALSE,
                       multiple = TRUE,
                       title = NULL,
                       ...) {
  checkmate::assert_string(dataname)
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(locked)

  formal_args <- as.list(environment())
  if (!missing(expr) && !missing(varname)) {
    stop("Must provide either `expr` or `varname`.")
  } else if (!missing(expr)) {
    fixed <- TRUE
    ts_expr_args <- c("dataname", "id", "expr", "fixed", "locked", "title")
    formal_args <- formal_args[ts_expr_args]
    checkmate::assert_string(id)
    checkmate::assert_string(title)
    checkmate::assert_string(expr)
    ans <- do.call(shiny::reactiveValues, c(formal_args, list(...)))
    class(ans) <- c("teal_slice_expr", "teal_slice", class(ans))
    ans
  } else if (!missing(varname)) {
    ts_var_args <- c(
      "dataname", "varname", "id", "choices", "selected", "keep_na", "keep_inf",
      "fixed", "locked", "multiple"
    )
    formal_args <- formal_args[ts_var_args]
    args <- c(formal_args, list(...))
    checkmate::assert_string(varname)
    checkmate::assert_multi_class(choices, .filterable_class, null.ok = TRUE)
    checkmate::assert_multi_class(selected, .filterable_class, null.ok = TRUE)
    checkmate::assert_flag(keep_na, null.ok = TRUE)
    checkmate::assert_flag(keep_inf, null.ok = TRUE)
    checkmate::assert_flag(multiple)
    if (missing(id)) {
      args$id <- paste(
        Filter(length, args[c("dataname", "varname", "experiment", "arg")]),
        collapse = " "
      )
    } else {
      checkmate::assert_string(id)
    }
    ans <- do.call(shiny::reactiveValues, args)
    class(ans) <- c("teal_slice_var", "teal_slice", class(ans))
    ans
  } else {
    stop("Must provide either `expr` or `varname`.")
  }
}

#' @rdname teal_slice
#' @export
#' @keywords internal
#'
is.teal_slice <- function(x) { # nolint
  inherits(x, "teal_slice")
}

#' @rdname teal_slice
#' @export
#' @keywords internal
#'
as.teal_slice <- function(x) { # nolint
  checkmate::assert_list(x, names = "named")
  do.call(teal_slice, x)
}

#' @rdname teal_slice
#' @export
#' @keywords internal
#'
as.list.teal_slice <- function(x, ...) {
  formal_args <- setdiff(names(formals(teal_slice)), "...")

  x <- if (shiny::isRunning()) {
    shiny::reactiveValuesToList(x)
  } else {
    shiny::isolate(shiny::reactiveValuesToList(x))
  }

  formal_args <- intersect(formal_args, names(x))
  extra_args <- rev(setdiff(names(x), formal_args))

  x[c(formal_args, extra_args)]
}


#' @rdname teal_slice
#' @export
#' @keywords internal
#'
format.teal_slice <- function(x, show_all = FALSE, trim_lines = TRUE, ...) {
  checkmate::assert_flag(show_all)
  checkmate::assert_flag(trim_lines)

  x_list <- as.list(x)
  if (!show_all) x_list <- Filter(Negate(is.null), x_list)

  jsonify(x_list, trim_lines)
}

#' @rdname teal_slice
#' @export
#' @keywords internal
#'
print.teal_slice <- function(x, ...) {
  cat(format(x, ...))
}


# format utils -----

#' Convert a list to a justified `JSON` string
#'
#' This function takes a list and converts it to a `JSON` string.
#' The resulting `JSON` string is then optionally justified to improve readability
#' and trimmed to easier fit in the console when printing.
#'
#' @param x (`list`), possibly recursive, obtained from `teal_slice` or `teal_slices`.
#' @param trim_lines (`logical(1)`) flag specifying whether to trim lines of the `JSON` string.
#' @return A `JSON` string representation of the input list.
#' @keywords internal
#'
jsonify <- function(x, trim_lines) {
  checkmate::assert_list(x)

  x_json <- to_json(x)
  x_json_justified <- justify_json(x_json)
  if (trim_lines) x_json_justified <- trim_lines_json(x_json_justified)
  paste(x_json_justified, collapse = "\n")
}

#' Converts a list to a `JSON` string
#'
#' Converts a list representation of `teal_slice` or `teal_slices` into a `JSON` string.
#' Ensures proper unboxing of list elements.
#' This function is used by the `format` methods for `teal_slice` and `teal_slices`.
#' @param x `list`, possibly recursive, obtained from `teal_slice` or `teal_slices`.
#' @return A `JSON` string.
#' @keywords internal
#
#' @param x (`list`) representation of `teal_slices` object.
#' @keywords internal
#'
to_json <- function(x) {
  no_unbox <- function(x) {
    vars <- c("selected", "choices")
    if (is.list(x)) {
      for (var in vars) {
        if (!is.null(x[[var]])) x[[var]] <- I(x[[var]])
      }
      lapply(x, no_unbox)
    } else {
      x
    }
  }

  jsonlite::toJSON(no_unbox(x), pretty = TRUE, auto_unbox = TRUE, digits = 16, null = "null")
}

#' Justify Colons in `JSON` String
#'
#' This function takes a `JSON` string as input, splits it into lines, and pads element names
#' with spaces so that colons are justified between lines.
#'
#' @param json (`character(1)`) a `JSON` string.
#'
#' @return A list of character strings, which can be collapsed into a `JSON` string.
#'
justify_json <- function(json) {
  format_name <- function(name, name_width) {
    if (nchar(name) == 1 || nchar(gsub("\\s", "", name)) <= 2) {
      return(name)
    } else if (grepl("slices|attributes", name)) {
      paste0(name, ":")
    } else {
      paste(format(name, width = name_width), ":")
    }
  }
  json_lines <- strsplit(json, "\n")[[1]]
  json_lines_split <- regmatches(json_lines, regexpr(":", json_lines), invert = TRUE)
  name_width <- max(unlist(regexpr(":", json_lines))) - 1
  vapply(json_lines_split, function(x) paste0(format_name(x[1], name_width), stats::na.omit(x[2])), character(1))
}

#' Trim Lines in `JSON` String
#'
#' This function takes a `JSON` string as input and returns a modified version of the
#' input where the values portion of each line is trimmed for a less messy console output.
#'
#' @param x A character string.
#'
#' @return A character string trimmed after a certain hard-coded number of characters in the value portion.
#'
#' @keywords internal
#'
trim_lines_json <- function(x) {
  name_width <- max(unlist(gregexpr(":", x))) - 1
  trim_position <- name_width + 17L
  x_trim <- substr(x, 1, trim_position)
  substr(x_trim, trim_position - 2, trim_position) <- "..."
  x_trim
}
