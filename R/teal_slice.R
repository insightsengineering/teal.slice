#' Manage filter state(s).
#'
#' Functions for passing filter state information between objects.
#'
#' These functions create and manage filter state specifications.
#' A single filter state can be fully described by a `teal_slice` object and such
#' objects will be used to create, modify, and delete a filter state.
#'
#' A `teal_slice` contains a number of common fields (all named arguments of `filter_var`)
#' but only `dataname` and `varname` are mandatory, while the others have default values.
#' Setting any of the other values to NULL means that these parameters will not be modified
#' (when setting an existing state) or that they will be determined by data (when creating new a new one).
#' Each of the common fields corresponds to one private field in `FilterState`
#' where it is stored and from where it is retrieved when calling `FiterState$get_state`.
#'
#' A `teal_slice` can also contain any number of additional fields, passed to `...`
#' as `name:value` pairs. These are collated into a list and stored in the
#' `private$extras` field.
#'
#' All `teal_slice` fields can be passed as arguments to `FilterState` constructors.
#' A `teal_slice` can be passed to `FilterState$set_state`, which will modify the state.
#' However, once a `FilterState` is created, only the **mutable** features can be set with a `teal_slice`:
#' `selected`, `keep_na`, `keep_inf`, and `disabled`.
#'
#' Special consideration is given to the `fixed` field. This is always a logical flag that defaults to FALSE.
#' In a `FilterState` instantiated with `fixed = TRUE` the features `selected`, `keep_na`, `keep_inf`
#' cannot be changed but the`disabled` can.
#'
#' `filter_var` creates a `teal_slice` object, which specifies a filter for a single variable,
#' passed to and resolved by `FilterState` objects.
#' `filter_settings` collates multiple `teal_slice` objects into `teal_slices`,
#' a complete filter specification. This is used by all classes above `FilterState`
#' as well as `filter_panel_api` wrapper functions.
#' `teal_slices` also specifies which variables cannot be filtered
#' and how observations are tallied, which is resolved by `FilterStates`.
#'
#' @section Filters in `SumarizedExperiment` and `MultiAssayExperiment` objects:
#'
#' To establish a filter on a column in a `data.frame`, `dataname` and `varname` are sufficient.
#' Filter states created created for `SummarizedExperiments` require more information
#' as each variable is either located in the `rowData` or `colData` slots.
#' Thus, `teal_slice` objects that refer to such filter states must also contain the field `target`
#' that specifies "subset" for variales in `rowData` and "select" for those in `colData`.
#'
#' Likewise, observations in a `MultiAssayExpeeiment` can be filtered based on the content of the `colData` slot
#' or based on the contents of `rowData` and `colData` of any of its experiments. Hence, another field is necessary.
#' `teal_slice` objects refering to `MultiAssayExperiment` objects must contain the field `datalabel`
#' that names either an experiment (as listed in `experimentList(<MAE>)`) or "subjects"
#' if it referes to the MAE's `colData`. They must **also** specify `target` as "subset" or "select"
#' for experiments and as "y" for `colData`.
#'
#' @param dataname `character(1)` name of data set
#' @param varname `character(1)` name of variable
#' @param choices optional vector specifying allowed choices;
#'  possibly a subset of values in data; type and size depends on variable type
#' @param selected optional vector specifying selection;
#'  type and size depends on variable type
#' @param keep_na `logical(1)` or `NULL` optional logical flag specifying whether to keep missing values
#' @param keep_inf `logical(1)` or `NULL` optional logical flag specifying whether to keep infinite values
#' @param fixed `logical(1)` logical flag specifying whether to fix this filter state (i.e. forbid setting state)
#' @param disabled `logical(1)`logical flag specifying whether to disable this filter state
#' @param include_varnames `named list` of `character` vectors where list names match names of data sets
#'  and vector elements match variable names in respective data sets;
#'  specifies which variables are not allowed to be filtered.
#'  Both `include_varnames` and `exclude_varnames` can't be specified for the same dataset in the same call.
#' @param exclude_varnames `named list` of `character` vectors where list names match names of data sets
#'  and vector elements match variable names in respective data sets;
#'  specifies which variables are not allowed to be filtered.
#'  Both `include_varnames` and `exclude_varnames` can't be specified for the same dataset in the same call.
#' @param count_type `character(1)` string specifying how observations are tallied by these filter states.
#'  Possible options:
#'  - `"all"` to have counts of single `FilterState` to show number of observation in filtered
#'   and unfiltered dataset.
#'  - `"none"` to have counts of single `FilterState` to show unfiltered number only.
#'
#' @param show_all `logical(1)` specifying whether NULL elements should also be printed
#' @param tss `teal_slices`
#' @param field `character(1)` name of `teal_slice` element
#' @param expr `character` string representing an expression that evaluates to a single `logical`;
#'             will be evaluated in individual `teal_slice` objects
#' @param drop `character` vector of fields to set to NULL; defaults to all except
#'             `datamane`, `varname`, `datalabel`, `target` (these have to be specified explicitly)
#' @param set `named list` specifying new values of desired fields, given as `name:value` pairs
#' @param ... for `filter_var` any number of additional fields given as `name:value` pairs\cr
#'            for `filter_settings` any number of `teal_slice` objects\cr
#'            for other functions arguments passed to other methods
#'
#' @return
#' `filter_var` returns object of class `teal_slice`, which is a named list.
#' `filter_settings` returns object of class `teal_slices`, which is an unnamed list of `teal_slice` objects.
#'
#' @examples
#' filter_1 <- filter_var("dataname1", "varname1", letters, "b", FALSE, extra1 = "extraone")
#' filter_2 <- filter_var("dataname1", "varname2", 1:10, 2, TRUE, FALSE, extra2 = "extratwo")
#' filter_3 <- filter_var("dataname2", "varname3", 1:10 / 10, 0.2, TRUE, FALSE,
#'   extra1 = "extraone", extra2 = "extratwo"
#' )
#'
#' all_filters <- filter_settings(
#'   filter_1,
#'   filter_2,
#'   filter_3,
#'   exclude = list(
#'     "dataname1" = "varname2"
#'   )
#' )
#'
#' teal.slice:::slices_which(all_filters, 'dataname == "dataname2"')
#' x <- "dataname2"
#' teal.slice:::slices_which(all_filters, sprintf('dataname == "%s"', x))
#' teal.slice:::slices_field(all_filters, "dataname")
#'
#' @name teal_slice
NULL


#' @export
#' @rdname teal_slice
#'
filter_var <- function(
    dataname,
    varname,
    choices = NULL,
    selected = NULL,
    keep_na = NULL,
    keep_inf = NULL,
    fixed = FALSE,
    disabled = FALSE,
    ...) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(varname)
  checkmate::assert_multi_class(choices, .filterable_class, null.ok = TRUE)
  checkmate::assert_multi_class(selected, .filterable_class, null.ok = TRUE)
  checkmate::assert_flag(keep_na, null.ok = TRUE)
  checkmate::assert_flag(keep_inf, null.ok = TRUE)
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(disabled)

  ans <- list(
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed,
    disabled = disabled
  )
  ans <- append(ans, list(...))

  class(ans) <- c("teal_slice", class(ans))
  ans
}


#' @export
#' @rdname teal_slice
#'
filter_settings <- function(
    ...,
    exclude_varnames = NULL,
    include_varnames = NULL,
    count_type = NULL) {
  slices <- list(...)
  checkmate::assert_list(slices, types = "teal_slice", any.missing = FALSE)
  checkmate::assert_list(exclude_varnames, names = "named", types = "character", null.ok = TRUE, min.len = 1)
  checkmate::assert_list(include_varnames, names = "named", types = "character", null.ok = TRUE, min.len = 1)
  checkmate::assert_character(count_type, len = 1, null.ok = TRUE)
  checkmate::assert_subset(count_type, choices = c("all", "none"), empty.ok = TRUE)

  structure(
    slices,
    exclude_varnames = exclude_varnames,
    include_varnames = include_varnames,
    count_type = count_type,
    class = c("teal_slices", class(slices))
  )
}


# check for teal_slice
#' @rdname teal_slice
#' @keywords internal
#'
is.teal_slice <- function(x) {
  inherits(x, "teal_slice")
}


# convert list to teal_slice
#' @rdname teal_slice
#' @keywords internal
#'
as.teal_slice <- function(x) {
  checkmate::assert_list(x, names = "named")
  do.call(filter_var, x)
}


# concatenate method for teal_slice
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
c.teal_slice <- function(...) {
  ans <- unlist(list(...), recursive = FALSE)
  if (anyDuplicated(names(ans))) {
    ans <- ans[!duplicated(names(ans))]
    warning("duplicate field names were discarded")
  }
  class(ans) <- c("teal_slice", class(ans))
  ans
}


# format method for teal_slice
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
format.teal_slice <- function(x, show_all = FALSE, ...) {
  name_width <- max(nchar(names(x)))
  format_value <- function(v) {
    if (is.null(v)) {
      return("NULL")
    }
    if (inherits(v, c("character", "factor"))) {
      v <- dQuote(v, q = FALSE)
    }
    v <- paste(v, collapse = " ")
    if (nchar(v) > 30L) {
      v <- paste0(substr(v, 1, 26), "...")
    }
    v
  }
  ind <- intersect(names(x), names(formals(filter_var)))
  xx <- x[ind]
  hm <- "teal_slice"
  for (i in seq_along(xx)) {
    element_name <- format(names(xx)[i], width = name_width)
    if (is.null(xx[[i]]) & !show_all) next
    element_value <- format_value(xx[[i]])
    hm <- append(hm, sprintf(" $ %s: %s", element_name, element_value))
  }

  ind <- setdiff(names(x), names(formals(filter_var)))
  if (length(ind) != 0L) {
    xx <- x[ind]
    hm <- append(hm, " .. additional information")
    for (i in seq_along(xx)) {
      element_name <- format(names(xx)[i], width = name_width)
      element_value <- format_value(xx[[i]])
      hm <- append(hm, sprintf("     $ %s: %s", element_name, element_value))
    }
  }
  paste(c(hm, ""), collapse = "\n")
}


# print method for teal_slice
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
print.teal_slice <- function(x, ...) {
  cat(format(x, ...))
}


# check for teal_slices
#' @rdname teal_slice
#' @keywords internal
#'
is.teal_slices <- function(x) {
  inherits(x, "teal_slices")
}



# convert nested list to teal_slices
# this function is not overly robust, it covers cases that are encountered in teal at this time
#' @rdname teal_slice
#' @keywords internal
#'
as.teal_slices <- function(x) {
  checkmate::assert_list(x, names = "named")
  is.bottom <- function(x) {
    isTRUE(is.list(x) && any(names(x) %in% c("selected", "keep_na", "keep_inf")))
  }
  make_args <- function() {
    list(
      dataname = NULL,
      varname = NULL,
      choices = NULL,
      selected = NULL,
      keep_na = NULL,
      keep_inf = NULL,
      fixed = FALSE,
      disabled = FALSE
    )
  }
  args <- make_args()
  slices <- vector("list")

  for (i in seq_along(x)) {
    item <- x[[i]]
    for (ii in seq_along(x[[i]])) {
      subitem <- item[[ii]]
      if (is.bottom(subitem)) {
        args$dataname <- names(x)[i]
        args$varname <- names(item)[[ii]]
        args$choices <- subitem$choices
        args$selected <- subitem$selected
        args$keep_na <- subitem$keep_na
        args$keep_inf <- subitem$keep_inf
        slices[[length(slices) + 1]] <- as.teal_slice(Filter(Negate(is.null), args))
        args <- make_args()
      } else {
        for (iii in seq_along(subitem)) {
          subsubitem <- subitem[[iii]]
          if (is.bottom(subsubitem)) {
            args$dataname <- names(x)[i]
            args$varname <- names(subitem)[iii]
            args$choices <- subsubitem$choices
            args$selected <- subsubitem$selected
            args$keep_na <- subsubitem$keep_na
            args$keep_inf <- subsubitem$keep_inf
            args$datalabel <- names(item)[ii]
            if (args$datalabel == "subjects") args$target <- "y"
            slices[[length(slices) + 1]] <- as.teal_slice(Filter(Negate(is.null), args))
            args <- make_args()
          } else {
            for (iiii in seq_along(subsubitem)) {
              subsubsubitem <- subsubitem[[iiii]]
              if (is.bottom(subsubsubitem)) {
                args$dataname <- names(x)[i]
                args$varname <- names(subsubitem)[iiii]
                args$choices <- subsubsubitem$choices
                args$selected <- subsubsubitem$selected
                args$keep_na <- subsubsubitem$keep_na
                args$keep_inf <- subsubsubitem$keep_inf
                args$datalabel <- names(item)[ii]
                args$target <- names(subitem)[iii]
                slices[[length(slices) + 1]] <- as.teal_slice(Filter(Negate(is.null), args))
                args <- make_args()
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

  do.call(filter_settings, c(slices, list(include_varnames = attr(x, "filterable"))))
}


# subset method for teal_slices
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
`[.teal_slices` <- function(x, i) {
  if (missing(i)) i <- seq_along(x)
  if (length(i) == 0L) {
    return(x[0])
  }
  if (is.logical(i) & length(i) > length(x)) stop("subscript out of bounds")
  if (is.numeric(i) & max(i) > length(x)) stop("subscript out of bounds")
  if (is.character(i)) {
    if (!all(is.element(i, names(x)))) stop("subscript out of bounds")
    i <- which(is.element(i, names(x)))
  }

  y <- NextMethod("[")
  attrs <- attributes(x)
  attrs$names <- attrs$names[i]
  datanames <- unique(unlist(vapply(y, function(ts) ts[["dataname"]], character(1L))))
  attrs[["exclude_varnames"]] <- Filter(Negate(is.null), attr(x, "exclude_varnames")[datanames])
  attrs[["include_varnames"]] <- Filter(Negate(is.null), attr(x, "include_varnames")[datanames])
  attributes(y) <- attrs
  y
}


# concatenate method for teal_slices
#' @export
#' @rdname teal_slice
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
    filter_settings,
    c(
      unlist(x, recursive = FALSE),
      list(
        include_varnames = includes,
        exclude_varnames = excludes,
        count_type = count_types
      )
    )
  )
}


# format method for teal_slices
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
format.teal_slices <- function(x, show_all = FALSE, ...) {
  res <- character(0)
  for (i in seq_along(x)) {
    ind <- names(x)[i]
    if (is.null(ind)) ind <- sprintf("[[%d]]", i)
    res <- append(res, ind)
    res <- append(res, format(x[[i]], show_all = show_all, ...))
  }

  includes <- attr(x, "include_varnames")
  if (length(includes) > 0L) {
    res <- append(res, "\nfilterable variables:")
    for (i in seq_along(includes)) {
      res <- append(res, sprintf(" $ %s: %s", names(includes)[i], toString(includes[[i]])))
    }
  }

  excludes <- attr(x, "exclude_varnames")
  if (length(excludes) > 0L) {
    res <- append(res, "\nnon-filterable variables:")
    for (i in seq_along(excludes)) {
      res <- append(res, sprintf(" $ %s: %s", names(excludes)[i], toString(excludes[[i]])))
    }
  }

  ct <- attr(x, "count_type")
  res <- append(res, sprintf("\ncount type: %s", ct))
  paste(c(res, ""), collapse = "\n")
}


# print method for teal_slices
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
print.teal_slices <- function(x, ...) {
  cat(format(x, ...))
}


# get field from all slices
#' @rdname teal_slice
#' @keywords internal
#'
slices_field <- function(tss, field) {
  checkmate::assert_class(tss, "teal_slices")
  unique(unlist(lapply(tss, function(x) x[[field]])))
}


# get slices where logical predicate is TRUE
#' @rdname teal_slice
#' @keywords internal
#'
slices_which <- function(tss, expr) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_string(expr)
  expr <- str2lang(expr)
  Filter(function(x) isTRUE(eval(expr, x)), tss)
}


# sets selected fields to NULL in all `teal_slice`s in a `teal_slices`
#' @rdname teal_slice
#' @keywords internal
#'
slices_drop <- function(tss, drop = NULL) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_character(drop, min.len = 1L, null.ok = TRUE)

  if (is.null(drop)) {
    drop <- setdiff(
      unique(unlist(lapply(tss, names))),
      c("dataname", "varname", "varlabel", "target")
    )
  }
  attrs <- Filter(Negate(is.null), attributes(tss)[c("include_varnames", "exclude_varnames", "count_type")])

  for (i in seq_along(drop)) {
    tss <- lapply(tss, function(x) {
      x <- unclass(x)
      x[[drop[i]]] <- NULL
      x
    })
  }

  unname(do.call(filter_settings, c(lapply(tss, as.teal_slice), attrs)))
}


# sets selected fields in all `teal_slice`s in a `teal_slices` to specified values
#' @rdname teal_slice
#' @keywords internal
#'
slices_set <- function(tss, set) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_list(set, names = "named")

  attrs <- Filter(Negate(is.null), attributes(tss)[c("include_varnames", "exclude_varnames", "count_type")])

  for (i in seq_along(set)) {
    tss <- lapply(tss, function(x) {
      within(x, assign(x = names(set[i]), value = set[[i]]))
    })
  }
  ans <- unname(do.call(filter_settings, c(tss, attrs)))

  ans
}
