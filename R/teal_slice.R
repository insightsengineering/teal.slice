#' Create a `teal_slice` object
#'
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
#' @param ... additional arguments which can be handled by extensions of `teal.slice` classes.
#'
#' @return `teal.slice` object
#'
#' @examples
#' teal_slice(
#'   dataname = "data",
#'   id = "Female adults",
#'   expr = "SEX == 'F' & AGE >= 18",
#'   title = "Female adults"
#' )
#' teal_slice(
#'  dataname = "data",
#'  varname = "var",
#'  choices = c("F", "M", "U"),
#'  selected = "F",
#'  keep_na = TRUE,
#'  keep_inf = TRUE,
#'  fixed = FALSE,
#'  locked = FALSE,
#'  multiple = TRUE,
#'  id = "Gender",
#'  extra_arg = "extra"
#' )
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
    ts_var_args <- c("dataname", "varname", "id", "choices", "selected", "keep_na", "keep_inf",
                      "fixed", "locked", "multiple")
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

#' Manage filter state(s).
#'
#' `teal_slices()` collates multiple `teal_slice` objects into `teal_slices`,
#' a complete filter specification. This is used by all classes above `FilterState`
#' as well as `filter_panel_api` wrapper functions.
#' `teal_slices` also specifies which variables cannot be filtered
#' and how observations are tallied, which is resolved by `FilterStates`.
#'
#' `include_varnames` and `exclude_varnames` in attributes in `teal_slices`
#' determine which variables can have filters assigned.
#' The former enumerates allowed variables, the latter enumerates forbidden values.
#' Since these can be mutually exclusive in some cases, they cannot both be set in one `teal_slices` object.
#'
#' @param ... any number of `teal_slice` objects
#' @param include_varnames,exclude_varnames `named list`s of `character` vectors where list names
#'  match names of data sets and vector elements match variable names in respective data sets;
#'  specify which variables are allowed to be filtered; see `Details`
#' @param count_type `character(1)` string specifying how observations are tallied by these filter states.
#'  Possible options:
#'  - `"all"` to have counts of single `FilterState` to show number of observation in filtered
#'   and unfiltered dataset.
#'  - `"none"` to have counts of single `FilterState` to show unfiltered number only.
#' @param module_add `logical(1)` logical flag specifying whether the user will be able to add new filters
#' @return
#' `teal_slices`, which is an unnamed list of `teal_slice` objects.
#'
#' @examples
#' filter_1 <- teal_slice(
#'   dataname = "dataname1",
#'   varname = "varname1",
#'   choices = letters,
#'   selected = "b",
#'   keep_na = TRUE,
#'   fixed = FALSE,
#'   extra1 = "extraone"
#' )
#' filter_2 <- teal_slice(
#'   dataname = "dataname1",
#'   varname = "varname2",
#'   choices = 1:10,
#'   keep_na = TRUE,
#'   selected = 2,
#'   fixed = TRUE,
#'   locked = FALSE,
#'   extra2 = "extratwo"
#' )
#' filter_3 <- teal_slice(
#'   dataname = "dataname2",
#'   varname = "varname3",
#'   choices = 1:10 / 10,
#'   keep_na = TRUE,
#'   selected = 0.2,
#'   fixed = TRUE,
#'   locked = FALSE,
#'   extra1 = "extraone",
#'   extra2 = "extratwo"
#' )
#'
#' all_filters <- teal_slices(
#'   filter_1,
#'   filter_2,
#'   filter_3,
#'   exclude_varnames = list(
#'     "dataname1" = "varname2"
#'   )
#' )
#' @export
teal_slices <- function(...,
                            exclude_varnames = NULL,
                            include_varnames = NULL,
                            count_type = NULL,
                            module_add = TRUE) {
  slices <- list(...)
  checkmate::assert_list(slices, types = "teal_slice", any.missing = FALSE)
  slices_id <- shiny::isolate(vapply(slices, `[[`, character(1L), "id"))
  if (any(duplicated(slices_id))) {
    stop(
      "Some teal_slice objects have the same id:\n",
      toString(unique(slices_id[duplicated(slices_id)]))
    )
  }
  checkmate::assert_list(exclude_varnames, names = "named", types = "character", null.ok = TRUE, min.len = 1)
  checkmate::assert_list(include_varnames, names = "named", types = "character", null.ok = TRUE, min.len = 1)
  checkmate::assert_character(count_type, len = 1, null.ok = TRUE)
  checkmate::assert_subset(count_type, choices = c("all", "none"), empty.ok = TRUE)
  checkmate::assert_logical(module_add)

  structure(
    slices,
    exclude_varnames = exclude_varnames,
    include_varnames = include_varnames,
    count_type = count_type,
    module_add = module_add,
    class = c("teal_slices", class(slices))
  )
}
