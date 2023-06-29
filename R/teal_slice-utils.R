

#' Method to Check if an Object is of Class "teal_slice"
#' This method checks whether an object belongs to the class "teal_slice".
#' @param x An R object to be checked.
#' @return A logical value indicating whether the object belongs to the class "teal_slice".
#' @export
#' @keywords internal
#'
is.teal_slice <- function(x) { # nolint
  inherits(x, "teal_slice")
}

#' Method to Convert a List to a "teal_slice" Object
#'
#' This method converts a list to a "teal_slice" object.
#' @param x (`list`) to be converted.
#' @return A "teal_slice" object.
#' @export
#' @keywords internal
#'
as.teal_slice <- function(x) { # nolint
  checkmate::assert_list(x, names = "named")
  fun <- teal_slice
  do.call(fun, x)
}

# concatenate method for teal_slice
#' Concatenate "teal_slice" Objects
#'
#' This method concatenates "teal_slice" objects.
#' @param ... (`teal_slice`) objects to be concatenated.
#' @return A "teal_slice" object.
#' @export
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

#' Convert a `teal_slice` object to a list
#'
#' Convert a `teal_slice` object to a list
#' @param x `teal_slice` object
#' @param ... additional arguments to the `teal_slice` function
#' @return A list representation of the `teal_slice` object
#' @export
#' @keywords internal
#'
as.list.teal_slice <- function(x, ...) {
  formals <- formals(teal_slice)

  x <- if (shiny::isRunning()) {
    shiny::reactiveValuesToList(x)
  } else {
    shiny::isolate(shiny::reactiveValuesToList(x))
  }

  formal_args <- setdiff(names(formals), "...")
  extra_args <- setdiff(names(x), formal_args)

  x[c(formal_args, extra_args)]
}


#' Format `teal_slice` object for printing
#'
#' Format `teal_slice` object for printing
#' @param x (`teal_slice`) object
#' @param show_all (`logical(1)`) indicating whether to show all fields
#' @param trim_lines (`logical(1)`) indicating whether to trim lines
#' @param ... additional arguments to the [jsonify()] function
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

#' Print `teal_slice` object
#'
#' Print `teal_slice` object
#' @param x (`teal_slice`) object
#' @param ... additional arguments to the [format()] function
#' @export
#' @keywords internal
#'
print.teal_slice <- function(x, ...) {
  cat(format(x, ...))
}

# teal_slices methods ----

#' Method to Check if an Object is of Class `"teal_slices"`
#'
#' This method checks whether an object belongs to the class `"teal_slices"`.
#' @param x An R object to be checked.
#' @return A logical value indicating whether the object belongs to the class `"teal_slices"`.
#' @keywords internal
#'
is.teal_slices <- function(x) { # nolint
  inherits(x, "teal_slices")
}

#' Convert a nested list to a `"teal_slices"` object
#'
#' This function is not overly robust, it covers cases that are encountered in teal at this time.
#' @param x (`list`) to be converted
#' @return A `"teal_slices"` object
#'
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


#' Extract or Replace Parts of a `teal_slices` Object
#' @param x (`teal_slices`) object
#' @param i (`character` or `numeric` or `logical`) indicating which elements to extract
#' @return A `teal_slices` object
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
  datanames <- unique(unlist(vapply(y, function(ts) shiny::isolate(ts[["dataname"]]), character(1L))))
  attrs[["exclude_varnames"]] <- Filter(Negate(is.null), attr(x, "exclude_varnames")[datanames])
  attrs[["include_varnames"]] <- Filter(Negate(is.null), attr(x, "include_varnames")[datanames])
  attributes(y) <- attrs
  y
}


# concatenate method for `teal_slices`
#' Concatenate `teal_slices` Objects
#' @param ... (`teal_slices`) objects to be concatenated
#' @return A `teal_slices` object
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

#' Format `teal_slices` Objects
#' @param x (`teal_slices`) object
#' @param show_all (`logical(1)`) whether to show all slices
#' @param trim_lines (`logical(1)`) whether to trim lines
#' @param ... additional arguments passed to `jsonify`
#' @return A `character` vector
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

#' Print `teal_slices` Objects
#' @param x (`teal_slices`) object
#' @param ... additional arguments passed to `format`
#' @export
#' @keywords internal
#'
print.teal_slices <- function(x, ...) {
  cat(format(x, ...), "\n")
}

# helpers ----

# get field from all slices
#' Extract unique values from field of `teal_slice` Objects.
#' @param tss (`teal_slices`) object
#' @param field (`character(1)`) `teal_slice` field name
#' @return A vector of unique values
#' @keywords internal
#'
slices_field <- function(tss, field) {
  checkmate::assert_string(field)
  checkmate::assert_class(tss, "teal_slices")
  unique(unlist(lapply(tss, function(x) x[[field]])))
}

#' Convert `teal_slices` to list
#' @param tss (`teal_slices`) object
#' @return A list
#' @keywords internal
#'
slices_to_list <- function(tss) {
  slices_list <- lapply(tss, as.list)
  attrs <- attributes(unclass(tss))
  tss_list <- list(slices = slices_list, attributes = attrs)
  Filter(Negate(is.null), tss_list) # drop attributes if empty
}
