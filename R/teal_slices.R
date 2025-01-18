#' Complete filter specification
#'
#' Create `teal_slices` object to package multiple filters and additional settings.
#' Check out [`teal_slices-utilities`] functions for working with `teal_slices` object.
#'
#' `teal_slices()` collates multiple `teal_slice` objects into a `teal_slices` object,
#' a complete filter specification. This is used by all classes above `FilterState`
#' as well as `filter_panel_api` wrapper functions.
#' `teal_slices` has attributes that modify the behavior of the filter panel, which are resolved by different classes.
#'
#' `include_varnames` and `exclude_varnames` determine which variables can have filters assigned.
#' The former enumerates allowed variables, the latter enumerates forbidden values.
#' Since these could be mutually exclusive, it is impossible to set both allowed and forbidden
#' variables for one data set in one `teal_slices`.
#'
#' @param ... any number of `teal_slice` objects.
#' @param include_varnames,exclude_varnames (`named list`s of `character`) where list names
#'  match names of data sets and vector elements match variable names in respective data sets;
#'  specify which variables are allowed to be filtered; see `Details`.
#' @param count_type `r lifecycle::badge("experimental")`
#' _This is a new feature. Do kindly share your opinions on
#' [`teal.slice`'s GitHub repository](https://github.com/insightsengineering/teal.slice/)._
#'
#'  (`character(1)`) string specifying how observations are tallied by these filter states.
#'  Possible options:
#'  - `"none"` (default) to have counts of single `FilterState` to show unfiltered number only.
#'  - `"all"` to have counts of single `FilterState` to show number of observation in filtered
#'   and unfiltered dataset. Note, that issues were reported when using this option with `MultiAssayExperiment`.
#'   Please make sure that adding new filters doesn't fail on target platform before deploying for production.
#' @param allow_add (`logical(1)`) logical flag specifying whether the user will be able to add new filters
#'
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
#'   anchored = FALSE,
#'   extra2 = "extratwo"
#' )
#' filter_3 <- teal_slice(
#'   dataname = "dataname2",
#'   varname = "varname3",
#'   choices = 1:10 / 10,
#'   keep_na = TRUE,
#'   selected = 0.2,
#'   fixed = TRUE,
#'   anchored = FALSE,
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
#'
#' is.teal_slices(all_filters)
#' all_filters[1:2]
#' c(all_filters[1], all_filters[2])
#' print(all_filters)
#' print(all_filters, trim_lines = FALSE)
#'
#' @seealso
#' - [`teal_slice`] for creating constituent elements of `teal_slices`
#' - `teal::slices_store` for robust utilities for saving and loading `teal_slices` in `JSON` format
#' - [`is.teal_slices`], [`as.teal_slices`], [`as.list.teal_slices`], [`[.teal_slices`], [`c.teal_slices`]
#' [`print.teal_slices`], [`format.teal_slices`]
#'
#' @export
#'
teal_slices <- function(...,
                        exclude_varnames = NULL,
                        include_varnames = NULL,
                        count_type = NULL,
                        allow_add = TRUE) {
  slices <- list(...)
  checkmate::assert_list(slices, types = "teal_slice", any.missing = FALSE)
  slices_id <- isolate(vapply(slices, `[[`, character(1L), "id"))
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
  checkmate::assert_logical(allow_add)

  duplicated_datasets <- intersect(names(include_varnames), names(exclude_varnames))
  if (length(duplicated_datasets)) {
    stop(
      "Some datasets are specified in both, include_varnames and exclude_varnames:\n",
      toString(duplicated_datasets)
    )
  }

  structure(
    slices,
    exclude_varnames = exclude_varnames,
    include_varnames = include_varnames,
    count_type = count_type,
    allow_add = allow_add,
    class = c("teal_slices", class(slices))
  )
}

#' `teal_slices` utility functions
#'
#' Helper functions for working with [`teal_slices`] object.
#' @param x object to test for `teal_slices`, object to convert to `teal_slices` or a `teal_slices` object
#' @param i (`character` or `numeric` or `logical`) indicating which elements to extract
#' @param recursive (`logical(1)`) flag specifying whether to also convert to list the elements of this `teal_slices`
#' @param ... additional arguments passed to other functions.
#' @name teal_slices-utilities
#' @inherit teal_slices examples
#' @keywords internal

#' @rdname teal_slices-utilities
#' @export
#'
is.teal_slices <- function(x) { # nolint
  inherits(x, "teal_slices")
}

#' @rdname teal_slices-utilities
#' @export
#'
as.teal_slices <- function(x) { # nolint
  checkmate::assert_list(x)
  lapply(x, checkmate::assert_list, names = "named", .var.name = "list element")

  attrs <- attributes(unclass(x))
  ans <- lapply(x, function(x) if (is.teal_slice(x)) x else as.teal_slice(x))
  do.call(teal_slices, c(ans, attrs))
}


#' @rdname teal_slices-utilities
#' @export
#'
as.list.teal_slices <- function(x, recursive = FALSE, ...) { # nolint
  ans <- unclass(x)
  if (recursive) ans[] <- lapply(ans, as.list)
  ans
}


#' @rdname teal_slices-utilities
#' @export
#'
`[.teal_slices` <- function(x, i) {
  if (missing(i)) i <- seq_along(x)
  if (length(i) == 0L) {
    return(x[0])
  }
  if (is.logical(i) && length(i) > length(x)) stop("subscript out of bounds")
  if (is.numeric(i) && max(i) > length(x)) stop("subscript out of bounds")
  if (is.character(i)) {
    if (!all(is.element(i, names(x)))) stop("subscript out of bounds")
    i <- which(is.element(i, names(x)))
  }

  y <- NextMethod("[")
  attrs <- attributes(x)
  attrs$names <- attrs$names[i]
  attributes(y) <- attrs
  y
}


#' @rdname teal_slices-utilities
#' @export
#'
c.teal_slices <- function(...) {
  x <- list(...)
  checkmate::assert_true(all(vapply(x, is.teal_slices, logical(1L))), .var.name = "all arguments are teal_slices")

  all_attributes <- lapply(x, attributes)
  all_attributes <- coalesce_r(all_attributes)
  all_attributes <- all_attributes[names(all_attributes) != "class"]

  do.call(
    teal_slices,
    c(
      unique(unlist(x, recursive = FALSE)),
      all_attributes
    )
  )
}


#' @rdname teal_slices-utilities
#' @param show_all (`logical(1)`) whether to display non-null elements of constituent `teal_slice` objects
#' @param trim_lines (`logical(1)`) whether to trim lines
#' @export
#'
format.teal_slices <- function(x, show_all = FALSE, trim_lines = TRUE, ...) {
  checkmate::assert_flag(show_all)
  checkmate::assert_flag(trim_lines)

  x <- as.list(x, recursive = TRUE)
  attrs <- attributes(x)
  attributes(x) <- NULL
  slices_list <- list(slices = x, attributes = attrs)
  slices_list <- Filter(Negate(is.null), slices_list) # drop attributes if empty

  if (!show_all) slices_list$slices <- lapply(slices_list$slices, function(slice) Filter(Negate(is.null), slice))

  jsonify(slices_list, trim_lines)
}

#' @rdname teal_slices-utilities
#' @export
#'
print.teal_slices <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}


#' `setdiff` method for `teal_slices`
#'
#' Compare two teal slices objects and return `teal_slices` containing slices present in `x` but not in `y`.
#' @param x,y (`teal_slices`)
#' @return `teal_slices`
#' @keywords internal
#'
setdiff_teal_slices <- function(x, y) {
  Filter(
    function(xx) {
      !any(vapply(y, function(yy) identical(yy, xx), logical(1)))
    },
    x
  )
}

#' Recursively coalesce list elements.
#'
#' Returns first element of list that it not `NULL`, recursively.
#'
#' Given a list of atomic vectors, the first non-null element is returned.
#' Given a list of lists, for all `names` found in all elements of the list
#' the first non-null element of a given name is returned.
#'
#' This function is used internally in `c.teal_slices` to manage `teal_slices` attributes.
#'
#' @param x (`list`), either of atomic vectors or of named lists
#' @return
#' Either an atomic vector of length 1 or a (potentially nested) list.
#'
#' @keywords internal
#'
coalesce_r <- function(x) {
  checkmate::assert_list(x)
  xnn <- Filter(Negate(is.null), x)
  if (all(vapply(xnn, is.atomic, logical(1L)))) {
    return(xnn[[1L]])
  }
  lapply(x, checkmate::assert_list, names = "named", null.ok = TRUE, .var.name = "list element")
  all_names <- unique(unlist(lapply(x, names)))
  sapply(all_names, function(nm) coalesce_r(lapply(x, `[[`, nm)), simplify = FALSE)
}
