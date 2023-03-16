
#' single variable filter
#'
#' Specifies a filter state.
#'
#' @param dataname `character(1)` name of data set
#' @param varname `character(1)` name of variable
#' @param choices vector specifying allowed choices;
#'                possibly a subset of values in data;
#'                type and size depends on variable type
#' @param selected vector specifying selection;
#'                 type and size depends on variable type
#' @param keep_na `logical(0-1)` optional logical flag specifying whether to keep missing values
#' @param keep_inf `logical(0-1)` optional logical flag specifying whether to keep infinite values
#' @param fixed `logical(1)` logical flag specifying whether to fix this filter state (i.e. forbid setting state)
#' @param ... any number of additional features given as `name:value` pairs
#'
#' @return
#' Object of class `teal_slice`, which is a named list.
#'
#' @examples
#' filter_one <- filter_var("dataname1", "varname1", letters, "b", FALSE, extra1 = "extraone")
#' filter_two <- filter_var("dataname1", "varname2", 1:10, 2, TRUE, FALSE, extra2 = "extratwo")
#' filter_three <- filter_var("dataname2", "varname3", 1:10/10, 0.2, TRUE, FALSE, extra1 = "extraone", extra2 = "extratwo")
#'
#' @export
#' @rdname new_api
#'
filter_var <- function(
    dataname,
    varname,
    choices = NULL,
    selected = NULL,
    keep_na = NULL,
    keep_inf = NULL,
    fixed = FALSE,
    ...
) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(varname)
  checkmate::assert_atomic(choices)
  checkmate::assert_atomic(selected)
  if (!is.null(choices) && !is.null(selected)) {
    if (inherits(choices, c("integer", "numeric", "Date", "POSIXt"))) {
      rc <- range(choices, na.rm = TRUE)
      rs <- range(selected, na.rm = TRUE)
      checkmate::assert_true(
        rs[1L] >= rc[1L] && rs[2L] <= rc[2L],
        .var.name = "range of \"selected\" is within range of \"choices\""
      )
    } else if (inherits(choices, c("logical", "character", "factor"))) {
      checkmate::assert_subset(selected, choices)
    } else {
      stop("filter_var cannot handle \"choices\" of type: ", toString(class(choices)))
    }
  }
  checkmate::assert_flag(keep_na, null.ok = TRUE)
  checkmate::assert_flag(keep_inf, null.ok = TRUE)
  checkmate::assert_flag(fixed)

  ans <- list(
    dataname = dataname,
    varname = varname,
    choices = choices,
    selected = selected,
    keep_na = keep_na,
    keep_inf = keep_inf,
    fixed = fixed
  )
  extras <- list(...)
  if (length(extras) != 0L) ans$extras <- extras

  class(ans) <- c("teal_slice", class(ans))
  ans
}

c.teal_slice <- function(...) {
  ans <- unlist(list(...), recursive = FALSE)
  class(ans) <- "teal_slice"
  ans
}
print.teal_slice <- function(x) {
  name_width <- max(nchar(names(x)))
  format_value <- function(v) {
    if (is.null(v)) return("NULL")
    if (inherits(v,  c("character", "factor"))) {
      v <- dQuote(v, q = FALSE)
    }
    v <- paste(v, collapse = " ")
    if (nchar(v) > 20L) {
      v <- paste(substr(v, 1, 16), "...")
    }
    v
  }

  ind <- intersect(names(x), names(formals(filter_var)))
  xx <- x[ind]
  cat("teal_slice", "\n")
  for (i in seq_along(xx)) {
    element_name <- format(names(xx)[i], width = name_width)
    element_value <- format_value(xx[[i]])
    cat(sprintf(" $ %s: %s", element_name, element_value), "\n")
  }

  ind <- setdiff(names(x), names(formals(filter_var)))
  if (length(ind) != 0L) {
    xx <- x[ind]
    cat(" .. additional information", "\n")
    for (i in seq_along(xx)) {
      # element_name <- format(names(xx)[i], width = name_width)
      # element_value <- format_value(xx[[i]])
      # cat(sprintf("     $ %s: %s", element_name, element_value), "\n")
      str(xx[[i]])
    }
  }
}


#' complete filter settings
#'
#' Collate single variable filter states into a complete filter specification.
#'
#' @param ... any number of `teal_slice` objects
#' @param slices optional `list` of `teal_slice` objects
#' @exclude `named list` of `character` vectors where list names match names of data sets
#'          and vectors elements match variable names in respective data sets;
#'          specifies which variables are not allowed to be filtered
#' @count_type `character(1)` string specifying how observations are tallied by these filter states
#'
#' @return
#' Object of class `teal_slices`, which is an unnamed list of `teal_slice` objects.
#'
#' @examples
#' all_filters <- filter_settings(
#'   filter_one,
#'   filter_two,
#'   filter_three,
#'   exclude = list(
#'     "dataname1" = "varname2"
#'   )
#' )
#'
#' @export
#' @rdname new_api
#'
filter_settings <- function(
    ...,
    exclude = list(),
    count_type = c("none", "all", "hierarchical")
) {
  slices <- list(...)
  checkmate::assert_list(slices, types = "teal_slice", any.missing = FALSE)
  checkmate::assert_list(exclude, names = "named", types = "character")
  count_type <- match.arg(count_type)

  attr(slices, "exclude") <- exclude
  attr(slices, "count_type") <- count_type
  class(slices) <- c("teal_slices", class(slices))
  slices
}

c.teal_slices <- function(...) {
  x <- list(...)
  excludes <- lapply(x, attr, "exclude")
  names(excludes) <- NULL
  excludes <- unlist(excludes, recursive = FALSE)
  excludes <- excludes[!duplicated(names(excludes))]
  count_types <- lapply(x, attr, "count_type")
  count_types <- unique(unlist(count_types))
  checkmate::assert_string(count_types)

  do.call(filter_settings, c(unlist(x, recursive = FALSE), list(exclude = excludes, count_type = count_types)))
}

is.teal_slice <- function(x) {
  inherits(x, "teal_slice")
}

`[.teal_slices` <- function(x, i) {
  y <- NextMethod("[")
  attributes(y) <- attributes(x)
  excludes <- unique(unlist(vapply(y, function(ts) ts[["dataname"]], character(1L))))
  attr(y, "exclude") <- Filter(Negate(is.null), attr(x, "exclude")[excludes])
  y
}

print.teal_slices <- function(x) {
  f <- attr(x, "exclude")
  ct <- attr(x, "count_type")
  for (i in seq_along(x)) {
    ind <- names(x)[i]
    if (is.null(ind)) ind <- sprintf("[[%d]]", i)
    cat(ind, "\n")
    print(x[[i]])
  }
  cat("\nnon-filterable variables:")
  if (is.list(f) & length(f) == 0L) {
    cat(" none\n")
  } else {
    cat("\n")
    for (i in seq_along(f)) {
      cat(sprintf(" $ %s: %s", names(f)[i], toString(f[[i]])), "\n")
    }
  }
  cat(sprintf("\ncount type: %s", ct), "\n")
}

is.teal_slices <- function(x) {
  inherits(x, "teal_slices")
}


filter_one <- filter_var("dataname1", "varname1", letters, "b", FALSE, extra1 = "extraone")
filter_two <- filter_var("dataname1", "varname2", 1:10, 2, TRUE, FALSE, extra2 = "extratwo")
filter_three <- filter_var("dataname2", "varname3", 1:10/10, 0.2, TRUE, FALSE, extra1 = "extraone", extra2 = "extratwo")

all_filters <- filter_settings(
  filter_one,
  filter_two,
  filter_three,
  exclude = list(
    "dataname1" = "varname2"
  )
)
# all_filters
# c(all_filters[1], all_filters[2])


# get slices where feature is value
extract_by_feat <- function(tss, feature, value) {
  checkmate::assert_class(tss, "teal_slices")
  Filter(function(x) x[[feature]] == value, tss)
}
extract_by_feat(all_filters, "dataname", "dataname1")

# get feature from all slices
extract_feat <- function(tss, feature) {
  checkmate::assert_class(tss, "teal_slices")
  lapply(tss, function(x) x[[feature]])
}
extract_feat(all_filters, "dataname")

# get slices where logical predicate is TRUE
extract_fun <- function(tss, expr) {
  checkmate::assert_class(tss, "teal_slices")
  expr <- substitute(expr)
  checkmate::assert_class(expr, "call")
  Filter(function(x) isTRUE(eval(expr, x)), tss)
}
extract_fun(all_filters, dataname == "dataname2")
extract_fun(all_filters, extras$extra2 == "extratwo")

# string version
extract_fun_s <- function(tss, expr) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_string(expr)
  expr <- str2lang(expr)
  Filter(function(x) isTRUE(eval(expr, x)), tss)
}
extract_fun_s(all_filters, 'dataname == "dataname2"')
x = "dataname2"
extract_fun_s(all_filters, sprintf('dataname == "%s"', x))

# add elements to extras in all slices
add_extras <- function(tss, extras) {
  ans <- lapply(tss, function(ts) {
    ts$extras <- c(ts$extras, extras)
    ts
  })
  attributes(ans) <- attributes(tss)
  ans
}

# convert list to teal_slice
as.teal_slice <- function(x) {
  do.call(filter_var, x)
}

# convert nested list to teal_slices
# this function is not overly robust, it covers cases that are encountered in teal at this time
as.teal_slices <- function(x) {
  checkmate::assert_list(x, names = "named")

  is.bottom <- function(x) {
    isTRUE(is.list(x) && all(names(x) %in% c("selected", "keep_na", "keep_inf")))
  }
  make_args <- function() {
    list(
      dataname = NULL,
      varname = NULL,
      selected = NULL,
      keep_na = NULL,
      keep_inf = NULL,
      datalabel = NULL,
      target = NULL
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
        args$selected <- subitem$selected
        args$keep_na <- subitem$keep_na
        args$keep_inf <- subitem$keep_inf
        slices[[length(slices)+1]] <- as.teal_slice(Filter(Negate(is.null), args))
        args <- make_args()
      } else {
        for (iii in seq_along(subitem)) {
          subsubitem <- subitem[[iii]]
          if (is.bottom(subsubitem)) {
            args$dataname <- names(x)[i]
            args$varname <- names(subitem)[iii]
            args$selected <- subsubitem$selected
            args$keep_na <- subsubitem$keep_na
            args$keep_inf <- subsubitem$keep_inf
            args$datalabel <- names(item)[ii]
            if (args$datalabel == "subjects") args$target <- "y"
            slices[[length(slices)+1]] <- as.teal_slice(Filter(Negate(is.null), args))
            args <- make_args()
          } else {
            for (iiii in seq_along(subsubitem)) {
              subsubsubitem <- subsubitem[[iiii]]
              if (is.bottom(subsubsubitem)) {
                args$dataname <- names(x)[i]
                args$varname <- names(subsubitem)[iiii]
                args$selected <- subsubsubitem$selected
                args$keep_na <- subsubsubitem$keep_na
                args$keep_inf <- subsubsubitem$keep_inf
                args$datalabel <- names(item)[ii]
                args$target <- names(subitem)[iiii]
                slices[[length(slices)+1]] <- as.teal_slice(Filter(Negate(is.null), args))
                args <- make_args()
              }
            }
          }
        }
      }
    }
  }

  do.call(filter_settings, slices)
}

# name teal_slices according to value of respective field in slices
# possibly useful in set_filter_state_impl but needs safeguards
name_slices <- function(tss, field) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_string(field)
  checkmate::assert_choice(field, setdiff(names(formals(filter_var)), "..."))

  feats <- unlist(extract_feat(tss, field))
  if (anyDuplicated(feats)) stop("duplicated values")

  names(tss) <- feats
  tss
}
