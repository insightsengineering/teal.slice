#' Manage filter state(s).
#'
#' Functions for passing filter state information between objects.
#'
#' These functions create and manage filter state specifications.
#' A single filter state can be fully described by a `teal_slice` object and such
#' objects will be used to create, modify, and delete a filter state.
#'
#' A `teal_slice` contains a number of common fields (all named arguments of `filter_var`),
#' some of which are mandatory, but only `dataname` and `varname` must be specified,
#' while the others have default values.
#' Setting any of the other values to NULL means that those properties will not be modified
#' (when setting an existing state) or that they will be determined by data (when creating new a new one).
#' Each of the common fields corresponds to one private field in `FilterState`
#' where it is stored and from where it is retrieved when calling `FiterState$get_state`.
#'
#' A `teal_slice` can also contain any number of additional fields, passed to `...`
#' as `name:value` pairs. These are collated into a list and stored in the
#' `private$extras` field in `FilterState`.
#'
#' All `teal_slice` fields can be passed as arguments to `FilterState` constructors.
#' A `teal_slice` can be passed to `FilterState$set_state`, which will modify the state.
#' However, once a `FilterState` is created, only the **mutable** features can be set with a `teal_slice`:
#' `selected`, `keep_na` and `keep_inf`.
#'
#' Special consideration is given to two fields: `fixed` and `locked`.
#' These are always immutable logical flags that default to FALSE.
#' In a `FilterState` instantiated with `fixed = TRUE` the features `selected`, `keep_na`, `keep_inf`
#' cannot be changed.
#'
#' `filter_var` creates a `teal_slice` object, which specifies a filter for a single variable,
#' passed to and resolved by `FilterState` objects.
#' `filter_settings` collates multiple `teal_slice` objects into `teal_slices`,
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
#' @section Filters in `SumarizedExperiment` and `MultiAssayExperiment` objects:
#'
#' To establish a filter on a column in a `data.frame`, `dataname` and `varname` are sufficient.
#' Filter states created created for `SummarizedExperiments` require more information
#' as each variable is either located in the `rowData` or `colData` slots.
#' Thus, `teal_slice` objects that refer to such filter states must also contain the field `arg`
#' that specifies "subset" for variables in `rowData` and "select" for those in `colData`.
#'
#' Likewise, observations in a `MultiAssayExpeeiment` can be filtered based on the content of the `colData` slot
#' or based on the contents of `rowData` and `colData` of any of its experiments. Hence, another field is necessary.
#' `teal_slice` objects referring to `MultiAssayExperiment` objects must contain the field `datalabel`
#' that names either an experiment (as listed in `experimentList(<MAE>)`) or "subjects"
#' if it refers to the `MultiAssaysExperiment` `colData`. They must **also** specify `arg` as "subset" or "select"
#' for experiments and as "y" for `colData`.
#'
#' @param dataname `character(1)` name of data set
#' @param varname `character(1)` name of variable
#' @param choices optional vector specifying allowed choices;
#'  possibly a subset of values in data; type and size depends on variable type
#' @param selected optional vector specifying selection;
#'  type and size depends on variable type
#' @param multiple (`logical(1)`)\cr
#'   flag specifying whether the `FilterState` more than one value can be selected;
#'   only applicable to `FilterStateChoices` and `FilterStateLogical`
#' @param keep_na `logical(1)` or `NULL` optional logical flag specifying whether to keep missing values
#' @param keep_inf `logical(1)` or `NULL` optional logical flag specifying whether to keep infinite values
#' @param fixed `logical(1)` logical flag specifying whether to fix this filter state (forbid setting state)
#' @param locked `logical(1)` logical flag specifying whether to lock this filter state (forbid disabling and removing)
#' @param include_varnames,exclude_varnames `named list`s of `character` vectors where list names
#'  match names of data sets and vector elements match variable names in respective data sets;
#'  specify which variables are allowed to be filtered; see `Details`
#' @param count_type `character(1)` string specifying how observations are tallied by these filter states.
#'  Possible options:
#'  - `"all"` to have counts of single `FilterState` to show number of observation in filtered
#'   and unfiltered dataset.
#'  - `"none"` to have counts of single `FilterState` to show unfiltered number only.
#' @param id (`character(1)`)\cr
#'   identifier of the filter
#' @param title (`reactive`)\cr
#'   title of the filter (used by `filter_expr`)
#' @param expr (`language`)\cr
#'   logical expression written in executable way, see `Details`
#'   where "executable" means
#'   that a `subset` call should be able to evaluate this without failure. For
#'   example `MultiAssayExperiment::subsetByColData` requires variable names prefixed
#'   by `dataname` (e.g. `data$var1 == "x" & data$var2 > 0`). For `data.frame` call
#'   can be written without prefixing `var1 == "x" & var2 > 0`.
#' @param ... additional arguments to be saved as a list in `private$extras` field
#' @param show_all `logical(1)` specifying whether NULL elements should also be printed
#' @param tss `teal_slices`
#' @param field `character(1)` name of `teal_slice` element
#' @param ... for `filter_var` and `filter_expr` any number of additional fields given as `name:value` pairs\cr
#'            for `filter_settings` any number of `teal_slice` objects\cr
#'            for other functions arguments passed to other methods
#'
#' @return
#' `filter_var` returns object of class `teal_slice`, which is a named list.
#' `filter_expr` returns object of class `teal_slice_expr`, which inherits from `teal_slice`.
#' `filter_settings` returns object of class `teal_slices`, which is an unnamed list of `teal_slice` objects.
#'
#' @examples
#' filter_1 <- filter_var(
#'   dataname = "dataname1",
#'   varname = "varname1",
#'   choices = letters,
#'   selected = "b",
#'   keep_na = TRUE,
#'   fixed = FALSE,
#'   extra1 = "extraone"
#' )
#' filter_2 <- filter_var(
#'   dataname = "dataname1",
#'   varname = "varname2",
#'   choices = 1:10,
#'   keep_na = TRUE,
#'   selected = 2,
#'   fixed = TRUE,
#'   locked = FALSE,
#'   extra2 = "extratwo"
#' )
#' filter_3 <- filter_var(
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
#' all_filters <- filter_settings(
#'   filter_1,
#'   filter_2,
#'   filter_3,
#'   exclude_varnames = list(
#'     "dataname1" = "varname2"
#'   )
#' )
#'
#' @name teal_slice
NULL

# filter ----------------------------------------------------------------------------------------------------------

#' @export
#' @rdname teal_slice
#'
filter_var <- function(dataname,
                       varname,
                       choices = NULL,
                       selected = NULL,
                       keep_na = NULL,
                       keep_inf = NULL,
                       fixed = FALSE,
                       locked = FALSE,
                       multiple = TRUE,
                       id,
                       ...) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(varname)
  checkmate::assert_multi_class(choices, .filterable_class, null.ok = TRUE)
  checkmate::assert_multi_class(selected, .filterable_class, null.ok = TRUE)
  checkmate::assert_flag(keep_na, null.ok = TRUE)
  checkmate::assert_flag(keep_inf, null.ok = TRUE)
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(locked)
  checkmate::assert_flag(multiple, null.ok = TRUE)
  ans <- c(as.list(environment()), list(...))
  if (missing(id)) {
    ans$id <- paste(Filter(length, ans[c("dataname", "varname", "datalabel", "arg")]), collapse = " ")
  }
  checkmate::assert_string(ans$id, .var.name = "id")
  ans <- do.call(shiny::reactiveValues, ans)
  class(ans) <- c("teal_slice", class(ans))
  ans
}

#' @export
#' @rdname teal_slice
#' @examples
#' filter_expr(
#'   dataname = "data",
#'   id = "FA",
#'   title = "Female adults",
#'   expr = "SEX == 'F' & AGE >= 18"
#' )
filter_expr <- function(dataname, id, title, expr, locked = FALSE, ...) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(id)
  checkmate::assert_string(title)
  checkmate::assert_string(expr)
  ans <- c(as.list(environment()), list(...))
  ans <- do.call(shiny::reactiveValues, ans)
  class(ans) <- c("teal_slice_expr", "teal_slice", class(ans))
  ans
}


#' @export
#' @rdname teal_slice
#'
filter_settings <- function(...,
                            exclude_varnames = NULL,
                            include_varnames = NULL,
                            count_type = NULL) {
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

  structure(
    slices,
    exclude_varnames = exclude_varnames,
    include_varnames = include_varnames,
    count_type = count_type,
    class = c("teal_slices", class(slices))
  )
}


# teal_slice ------------------------------------------------------------------------------------------------------

# check for teal_slice
#' @rdname teal_slice
#' @keywords internal
#'
is.teal_slice <- function(x) { # nolint
  inherits(x, "teal_slice")
}


# convert list to teal_slice
#' @rdname teal_slice
#' @keywords internal
#'
as.teal_slice <- function(x) { # nolint
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

#' @keywords internal
#' @export
as.list.teal_slice <- function(x) {
  formals <- if (inherits(x, "teal_slice_expr")) {
    formals(filter_expr)
  } else {
    formals(filter_var)
  }

  x <- if (shiny::isRunning()) {
    shiny::reactiveValuesToList(x)
  } else {
    shiny::isolate(shiny::reactiveValuesToList(x))
  }

  formal_args <- setdiff(names(formals), "...")
  extra_args <- setdiff(names(x), formal_args)

  x[c(formal_args, extra_args)]
}


#' @export
#' @param show_all `logical(1)` should parameters set to NULL be returned
#' @param center `logical(1)` should the output be centered and trimmed
#' @rdname teal_slice
#' @keywords internal
#'
format.teal_slice <- function(x, show_all = FALSE, center = TRUE, ...) {
  checkmate::assert_flag(show_all)
  checkmate::assert_flag(center)

  x_list <- as.list(x)
  if (!show_all) x_list <- Filter(Negate(is.null), x_list)

  if(!is.null(x_list$selected)) {
    x_list$selected <- I(x_list$selected) # so it is not unboxed
  }
  if(!is.null(x_list$choices)) {
    x_list$choices <- I(x_list$choices) # so it is not unboxed
  }
  x_json <- jsonlite::toJSON(x_list, pretty = TRUE, auto_unbox = TRUE, digits = 16, null = "null")
  x_json_s <- strsplit(x_json, split = "\n")[[1]]

  if (center) x_json_s <- center_json(x_json_s)

  paste(c("teal_slice", x_json_s), collapse = "\n")
}

# centering of json output for `teal_slices` object JSON representation
#' @param json a result of `jsonlite::toJSON(as.list(teal_slice), pretty = TRUE, auto_unbox = TRUE)`
#' @keywords internal
#'
center_json <- function(json) {
  json_s <- strsplit(json, split = ":", fixed = TRUE)

  name_width <- max(unlist(lapply(json_s, function(x) nchar(x[1]))))

  format_value <- function(v) {
    if (is.null(v)) {
      return("NULL")
    }
    if (is.language(v)) {
      v <- deparse1(v)
    }
    v <- paste(v, collapse = " ")
    if (nchar(v) > 30L) {
      v <- paste0(substr(v, 1, 26), "...")
    }
    v
  }

  ints <- setdiff(seq_along(json_s), c(1, length(json_s)))

  for (i in ints) {
    json_s[[i]][1] <- paste0(format(json_s[[i]][1], width = name_width), ":")
    json_s[[i]][2] <- format_value(json_s[[i]][2])
  }

  unlist(lapply(json_s, paste0, collapse = ""))
}


#' @export
#' @rdname teal_slice
#' @keywords internal
#'
print.teal_slice <- function(x, ...) {
  cat(format(x, ...), "\n")
}


# check for teal_slices
#' @rdname teal_slice
#' @keywords internal
#'
is.teal_slices <- function(x) { # nolint
  inherits(x, "teal_slices")
}


# teal_slices -----------------------------------------------------------------------------------------------------

# convert nested list to teal_slices
# this function is not overly robust, it covers cases that are encountered in teal at this time
#' @rdname teal_slice
#' @keywords internal
#'
as.teal_slices <- function(x) { # nolint
  checkmate::assert_list(x, names = "named")
  is_bottom <- function(x) {
    isTRUE(is.list(x) && any(names(x) %in% c("selected", "keep_na", "keep_inf"))) ||
      identical(x, list()) ||
      is.atomic(x)
  }
  make_args <- function(object, dataname, varname, datalabel, arg = NULL) {
    args <- list(
      dataname = dataname,
      varname = varname
    )
    if (!missing(datalabel)) args$datalabel <- datalabel
    if (!missing(arg)) args$arg <- arg
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
              datalabel = name_i,
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
                  datalabel = name_i,
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

  do.call(filter_settings, c(slices, list(include_varnames = attr(x, "filterable"))))
}


# subset method for `teal_slices`
#' @export
#' @rdname teal_slice
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
  datanames <- unique(unlist(vapply(y, function(ts) shiny::isolate(ts[["dataname"]]), character(1L))))
  attrs[["exclude_varnames"]] <- Filter(Negate(is.null), attr(x, "exclude_varnames")[datanames])
  attrs[["include_varnames"]] <- Filter(Negate(is.null), attr(x, "include_varnames")[datanames])
  attributes(y) <- attrs
  y
}


# concatenate method for `teal_slices`
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
      unique(unlist(x, recursive = FALSE)),
      list(
        include_varnames = if (length(includes)) includes,
        exclude_varnames = if (length(excludes)) excludes,
        count_type = count_types
      )
    )
  )
}

#' @param tss `teal_slices` object
#' @param file `character(1)` specifying path to save to
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
store_slices <- function(tss, file) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "json")

  cat(format(x, show_all = TRUE, center = FALSE), "\n", file = file)
}

#' @param file `character(1)` specifying path to read from
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
restore_slices <- function(file) {
  checkmate::assert_file_exists(file, access = "r", extension = "json")

  tss_txt <- paste0(readLines(file), collapse = "")
  tss_txt_split <- strsplit(tss_txt, split = "attributes")[[1]]

  attributes <- jsonlite::fromJSON(tss_txt_split[2])
  tss_s <- strsplit(tss_txt_split[1], split = "teal_slice")[[1]][-1]
  tss_l <- lapply(tss_s, jsonlite::fromJSON)

  tss_elements <-
    lapply(tss_l, function(x) {
      x <- Filter(Negate(is.null), x)

      fun <- if ("expr" %in% names(x)) {
        filter_expr
      } else {
        filter_var
      }

      do.call(fun, x)
    })

  tss <- do.call(filter_settings, c(`...` = tss_elements, attributes))
  names(tss) <- NULL
  tss
}



#' @param x `teal_slice` object
#' @param show_all `logical(1)` should parameters set to NULL be returned
#' @param center `logical(1)` should the output be centered and trimmed
#' @export
#' @rdname teal_slice
#' @keywords internal
#'
format.teal_slices <- function(x, show_all = FALSE, center = TRUE, ...) {
  checkmate::assert_flag(show_all)
  checkmate::assert_flag(center)

  x_format <- lapply(x, format, show_all = show_all, center = center)

  attributes <- attributes(x)
  attributes$class <- NULL
  attributes <- paste0("attributes\n", jsonlite::toJSON(attributes, pretty = TRUE, auto_unbox = TRUE))

  paste0(c(unlist(x_format), attributes), collapse = "\n")
}

#' @export
#' @rdname teal_slice
#' @keywords internal
#'
print.teal_slices <- function(x, ...) {
  cat(format(x, ...), "\n")
}


# get field from all slices
#' @rdname teal_slice
#' @keywords internal
#'
slices_field <- function(tss, field) {
  checkmate::assert_string(field)
  checkmate::assert_class(tss, "teal_slices")
  unique(unlist(lapply(tss, function(x) x[[field]])))
}
