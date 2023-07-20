#' Complete filter specification.
#'
#' Create `teal_slices` object to package multiple filters and additional settings.
#'
#' @details
#' `teal_slices()` collates multiple `teal_slice` objects into a `teal_slices` object,
#' a complete filter specification. This is used by all classes above `FilterState`
#' as well as `filter_panel_api` wrapper functions.
#' `teal_slices` has attributes that modify the behavior of the filter panel, which are resolved by different classes.
#'
#' `include_varnames` and `exclude_varnames` determine which variables can have filters assigned.
#' The former enumerates allowed variables, the latter enumerates forbidden values.
#' Since these can be mutually exclusive in some cases, they cannot both be set in one `teal_slices` object.
#'
#' @param ... any number of `teal_slice` objects. For `print` and `format`,
#'  additional arguments passed to other functions.
#' @param include_varnames,exclude_varnames (`named list`s of `character`) where list names
#'  match names of data sets and vector elements match variable names in respective data sets;
#'  specify which variables are allowed to be filtered; see `Details`
#' @param count_type (`character(1)`) string specifying how observations are tallied by these filter states.
#'  Possible options:
#'  - `"none"` (default) to have counts of single `FilterState` to show unfiltered number only.
#'  - `"all"` to have counts of single `FilterState` to show number of observation in filtered
#'   and unfiltered dataset. Note, that issues were reported when using this option with `MultiAssayExperiment`.
#'   Please make sure that adding new filters doesn't fail on target platform before deploying for production.
#' @param allow_add (`logical(1)`) logical flag specifying whether the user will be able to add new filters
#' @param i (`character` or `numeric` or `logical`) indicating which elements to extract
#' @param x (`teal_slices`) object.
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
#'     "dataname1" = "varname3"
#'   )
#' )
#'
#' is.teal_slices(all_filters)
#' all_filters[1:2]
#' c(all_filters[1], all_filters[2])
#' print(all_filters)
#' print(all_filters, trim_lines = FALSE)
#'
#' @seealso [`teal_slice`]
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
  slices_id <- shiny::isolate(vapply(slices, `[[`, character(1L), "id"))
  if (any(duplicated(slices_id))) {
    stop(
      "Some teal_slice objects have the same id:\n",
      toString(unique(slices_id[duplicated(slices_id)]))
    )
  }

  varnames <- shiny::isolate(setNames(
    lapply(slices, function(slice) slice$varname),
    sapply(slices, function(slice) slice$dataname)
  ))
  excluded_datanames <- unlist(intersect(names(varnames), names(exclude_varnames)))
  if (!is.null(excluded_datanames) && length(excluded_datanames) > 0) {
    lapply(excluded_datanames, function(name) {
      checkmate::assert_disjunct(exclude_varnames[[name]], varnames[[name]])
    })
  }
  checkmate::assert_list(exclude_varnames, names = "named", types = "character", null.ok = TRUE, min.len = 1)
  checkmate::assert_list(include_varnames, names = "named", types = "character", null.ok = TRUE, min.len = 1)
  checkmate::assert_character(count_type, len = 1, null.ok = TRUE)
  checkmate::assert_subset(count_type, choices = c("all", "none"), empty.ok = TRUE)
  checkmate::assert_logical(allow_add)

  structure(
    slices,
    exclude_varnames = exclude_varnames,
    include_varnames = include_varnames,
    count_type = count_type,
    allow_add = allow_add,
    class = c("teal_slices", class(slices))
  )
}



#' @rdname teal_slices
#' @export
#' @keywords internal
#'
is.teal_slices <- function(x) { # nolint
  inherits(x, "teal_slices")
}

#' @rdname teal_slices
#' @export
#' @keywords internal
#'
as.teal_slices <- function(x) { # nolint
  checkmate::assert_list(x, names = "named")
  is_bottom <- function(x) {
    isTRUE(is.list(x) && any(names(x) %in% c("selected", "keep_na", "keep_inf"))) ||
      identical(x, list()) ||
      is.atomic(x)
  }
  make_args <- function(object, dataname, varname, experiment = NULL, arg = NULL) {
    args <- list(
      dataname = dataname,
      varname = varname
    )
    if (!is.null(experiment)) args$experiment <- experiment
    if (!is.null(arg)) args$arg <- arg
    if (is.list(object)) {
      args <- c(args, object)
    } else if (is.atomic(object)) {
      args$selected <- object
    }
    args
  }
  slices <- vector("list")

  for (dataname in names(x)) {
    item <- x[[dataname]]
    for (name_i in names(item)) {
      subitem <- item[[name_i]]
      if (is_bottom(subitem)) {
        args <- make_args(
          subitem,
          dataname = dataname,
          varname = name_i
        )
        slices <- c(slices, list(as.teal_slice(args)))
      } else {
        # MAE zone
        for (name_ii in names(subitem)) {
          subsubitem <- subitem[[name_ii]]
          if (is_bottom(subsubitem)) {
            args <- make_args(
              subsubitem,
              dataname = dataname,
              experiment = if (name_i != "subjects") name_i,
              varname = name_ii
            )
            slices <- c(slices, list(as.teal_slice(args)))
          } else {
            for (name_iii in names(subsubitem)) {
              subsubsubitem <- subsubitem[[name_iii]]
              if (is_bottom(subsubsubitem)) {
                args <- make_args(
                  subsubsubitem,
                  dataname = dataname,
                  experiment = name_i,
                  arg = name_ii,
                  varname = name_iii
                )
                slices <- c(slices, list(as.teal_slice(args)))
              }
            }
          }
        }
      }
    }
  }

  if (length(slices) == 0L && length(x) != 0L) {
    stop("conversion to filter_slices failed")
  }

  do.call(teal_slices, c(slices, list(include_varnames = attr(x, "filterable"))))
}


#' @rdname teal_slices
#' @export
#' @keywords internal
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


#' @rdname teal_slices
#' @export
#' @keywords internal
#'
c.teal_slices <- function(...) {
  x <- list(...)
  checkmate::assert_true(all(vapply(x, is.teal_slices, logical(1L))), .var.name = "all arguments are teal_slices")

  excludes <- lapply(x, attr, "exclude_varnames")
  names(excludes) <- NULL
  excludes <- unlist(excludes, recursive = FALSE)
  excludes <- excludes[!duplicated(names(excludes))]

  includes <- lapply(x, attr, "include_varnames")
  names(includes) <- NULL
  includes <- unlist(includes, recursive = FALSE)
  includes <- includes[!duplicated(names(includes))]

  count_types <- lapply(x, attr, "count_type")
  count_types <- unique(unlist(count_types))

  do.call(
    teal_slices,
    c(
      unique(unlist(x, recursive = FALSE)),
      list(
        include_varnames = if (length(includes)) includes,
        exclude_varnames = if (length(excludes)) excludes,
        count_type = count_types
      )
    )
  )
}


#' @rdname teal_slices
#' @param show_all (`logical(1)`) whether to display non-null elements of constituent `teal_slice` objects
#' @param trim_lines (`logical(1)`) whether to trim lines
#' @export
#' @keywords internal
#'
format.teal_slices <- function(x, show_all = FALSE, trim_lines = TRUE, ...) {
  checkmate::assert_flag(show_all)
  checkmate::assert_flag(trim_lines)

  slices_list <- slices_to_list(x)

  if (!show_all) slices_list$slices <- lapply(slices_list$slices, function(slice) Filter(Negate(is.null), slice))

  jsonify(slices_list, trim_lines)
}

#' @rdname teal_slices
#' @export
#' @keywords internal
#'
print.teal_slices <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' Convert `teal_slices` to list
#' @param tss (`teal_slices`) object
#' @return A list of length 2, the first element holding all `teal_slice` contained in `tss`
#'  (converted to list) and the second element holding the all non-NULL attributes of `tss`.
#' @keywords internal
#'
slices_to_list <- function(tss) {
  slices_list <- lapply(tss, as.list)
  attrs <- attributes(unclass(tss))
  tss_list <- list(slices = slices_list, attributes = attrs)
  Filter(Negate(is.null), tss_list) # drop attributes if empty
}

#' `setdiff` method for `teal_slices`
#'
#' Compare two teal slices objects and return `teal_slices` containing slices present in `x` but not in `y`.
#' @param x,y `teal_slices` objects
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
