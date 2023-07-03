#' Filter metadata
#'
#' @details `teal_slice` object fully describes filter state and can be used to create,
#' modify, and delete a filter state. A `teal_slice` contains a number of common fields
#' (all named arguments of `teal_slice`), some of which are mandatory, but only
#' `dataname` and  either `varname` or `expr` must be specified, while the others have default
#' values.
#'
#' Setting any of the other values to NULL means that those properties will not be modified
#' (when setting an existing state) or that they will be determined by data (when creating new a new one).
#' Entire object is `FilterState` class member and can be accessed with `FilterState$get_state()`.
#'
#' `teal_slice()` returns two types of `teal_slice` objects:
#' 1. `teal_slice_var` - returned when `varname` is set. The object keep information about the
#' variable name, possible choices, and selected values and is used to create an interactive
#' filter.
#' 2. `teal_slice_expr` - returned when `expr` is set. The object keeps information about the
#' expression and is used to create a static filter which evaluates always the same expression.
#' When `expr` is specified `varname`, `choices`, `selected`, `multiple`, keep_na`, `keep_inf`
#' are ignored.
#'
#' All `teal_slice` fields can be passed as arguments to `FilterState` constructors.
#' A `teal_slice` object can be passed to `FilterState$set_state`, which will modify the state.
#' However, once a `FilterState` is created, only the **mutable** features can be set with a `teal_slice`:
#' `selected`, `keep_na` and `keep_inf`.
#'
#' Special consideration is given to two fields: `fixed` and `locked`.
#' These are always immutable logical flags that default to FALSE.
#' In a `FilterState` instantiated with `fixed = TRUE` the features
#' `selected`, `keep_na`, `keep_inf` cannot be changed.
#'
#' @section Filters in `SumarizedExperiment` and `MultiAssayExperiment` objects:
#'
#' To establish a filter on a column in a `data.frame`, `dataname` and `varname` are sufficient.
#' For filters refering to `colData` of `MultiAssayExperiment` object (subjects filter) no extra arguments are needed.
#' Filter states  created for `experiments` require more information as each variable is either located in
#' the specific `ExperimentList` slot. To correctly specify filter for an `SummarizedExperiment` one must set:
#' - `experiment` (`character(1)`) name of the experiment in `MultiAssayExperiment` object.
#' - `arg` (`"subset"  or `"select`) to refer to the particular argument in the [subset()] function for
#' `SummarizedExperiment`.
#'
#' @param dataname (`character(1)`) name of data set
#' @param varname (`character(1)`) name of variable
#' @param id (`character(1)`) identifier of the filter. Must be specified when `expr` is set.
#'  When `varname` is specified then `id` is set to a `"{dataname} {varname}"` .
#' @param expr (`character(1)`) string providing a logical expression;
#' Must be a valid R expression which can be evaluated in the context of the data set;
#' For a `data.frame` `var == "x"` is sufficient, but `MultiAssayExperiment::subsetByColData`
#' requires `dataname` prefix `data$var == "x"`.
#' @param choices (optional `vector`) specifying allowed choices;
#' When specified it should be a subset of values in variable denoted by `varname`;
#' Type and size depends on variable type.
#' @param selected (optional `vector`) of selected values from `choices`;
#' Type and size depends on variable type.
#' @param multiple (optional `logical(1)`) flag specifying whether more than one value can be selected;
#' only applicable to `ChoicesFilterState` and `LogicalFilterState`
#' @param keep_na (optional `logical(1)` or `NULL`) flag specifying whether to keep missing values
#' @param keep_inf (optional `logical(1)` or `NULL`) flag specifying whether to keep infinite values
#' @param fixed (`logical(1)`) flag specifying whether to fix this filter state (forbid setting state)
#' @param locked (`logical(1)`) flag specifying whether to lock this filter state (forbid disabling and removing)
#' @param title (optional `character(1)`) title of the filter. Ignored when `varname` is set.
#' @param ... in `teal_slice` method these are additional arguments which can be handled by extensions
#'  of `teal.slice` classes. In other methods these are further arguments passed to or from other methods.
#' @param x (`teal.slice`) object.
#' @param show_all (`logical(1)`) in `format`, `print` - indicating whether to show all fields.
#' @param trim_lines (`logical(1)`) in `format`, `print` - indicating whether to trim lines.
#'
#' @return `teal.slice` object
#'
#' @examples
#' x <- teal_slice(
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
#' is.teal_slice(x)
#' as.list(x)
#' as.teal_slice(list(dataname = "a", varname = "var"))
#' format(x, show_all = FALSE, trim_lines = TRUE)
#' print(x, show_all = FALSE, trim_lines = TRUE)
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
                       fixed = if (!missing(expr)) TRUE else FALSE,
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
  fun <- teal_slice
  do.call(fun, x)
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
  extra_args <- setdiff(names(x), formal_args)

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

#' Convert a list to a justified JSON string
#'
#' This function takes a list and converts it to a JSON string. The resulting JSON string is then
#' justified to improve readability. Additionally, the function has an option to trim the lines of
#' the JSON string.
#'
#' @param x `list` containing JSON strings.
#' @param trim_lines (`function`) or not to trim lines of the JSON string (default is FALSE).
#' @return A justified JSON string representation of the input list.
#' @keywords internal
#'
jsonify <- function(x, trim_lines) {
  checkmate::assert_list(x)

  x_json <- to_json(x)
  x_json_justified <- justify_json(x_json)
  if (trim_lines) x_json_justified <- trim_lines(x_json_justified)
  paste(x_json_justified, collapse = "\n")
}

#' Converts a list to a JSON string
#'
#' Converts a list representation of `teal_slices` into a JSON string. This function is used by the
#' `format` method for `teal_slices` objects.
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

#' Justify Colons in JSON String
#'
#' This function takes a JSON string as input and returns a modified version of the input where colons are justified in each line.
#'
#' @param json (`character(1)`) representing the input JSON.
#'
#' @return A character string with justified colons in each line of the JSON.
#' @keywords internal
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

#' Justify Colons in JSON String
#'
#' This function takes a JSON string as input and returns a modified version of the input where colons are justified in each line.
#'
#' @param json A character string representing the input JSON.
#'
#' @return A character string with justified colons in each line of the JSON.
#' @keywords internal
#'
trim_lines <- function(x) {
  name_width <- max(unlist(gregexpr(":", x))) - 1
  trim_position <- name_width + 17L
  x_trim <- substr(x, 1, trim_position)
  substr(x_trim, trim_position - 2, trim_position) <- "..."
  x_trim
}
