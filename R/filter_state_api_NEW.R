
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
  str(x)
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
  attr(y, "exclude") <- attr(x, "exclude")[excludes]
  y
}

print.teal_slices <- function(x) {
  f <- attr(x, "exclude")
  ct <- attr(x, "count_type")
  x <- lapply(x, unclass)
  lapply(x, str)
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
