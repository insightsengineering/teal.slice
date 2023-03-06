
#' single variable filter
#'
#' Specifies a filter state.
#'
#' @param dataname `character(1)` name of data set
#' @param varname `character(1)` name of variable
#' @param choices vector specifying choices choices; type and size depends on variable type
#' @param selected vector specifying initial choices; type and size depends on variable type
#' @param varlabel `character(0-1)` optional variable label
#' @param keep_na `logical(1)` logical flag specifying the initial value of `private$keep_na` field
#' @param keep_inf `logical(0-1)` optional logical flag specifying the initial value of `private$keep_na` field
#' @param fixed `logical(1)` logical flag specifying whether to fix this filter state (i.e. forbid setting state)
#' @param ... optional additional information given as `name:value` pairs, to be stored in `private$extras`
#'
#' @return
#' Object of class `teal_slice`, which is a named list.
#'
#' @examples
#' filter_one <- filter_var("dataname1", "varname1", letters, "b", "characters", FALSE, FALSE, extra1 = "extraone1", extra2 = "extraone2")
#' filter_two <- filter_var("dataname1", "varname2", 1:10, 2, "integers", TRUE, FALSE, extra1 = "extratwo1", extra2 = "extratwo2")
#' filter_three <- filter_var("dataname1", "varname3", 1:10/10, 0.2, "doubles", TRUE, FALSE)
#'
#' @export
#' @rdname new_api
#'
filter_var <- function(
    dataname,
    varname,
    choices,
    selected,
    varlabel = NULL,
    keep_na = FALSE,
    keep_inf = NULL,
    fixed = FALSE,
    ...
) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(varname)
  # assertion on choices dependent on variable type
  # assertion on selected dependent on variable type
  checkmate::assert_string(varlabel, null.ok = TRUE)
  checkmate::assert_flag(keep_na)
  checkmate::assert_flag(keep_inf, null.ok = TRUE)
  checkmate::assert_flag(fixed)

  ans <- as.list(match.call())[-1]
  extras <- ans[!is.element(names(ans), formalArgs(filter_var))] # extras <- list(...)
  class(ans) <- "teal_slice"
  ans
}

filter_one <- filter_var("dataname1", "varname1", letters, "b", "characters", FALSE, FALSE, extra1 = "extraone1", extra2 = "extraone2")
filter_two <- filter_var("dataname1", "varname2", 1:10, 2, "integers", TRUE, FALSE, extra1 = "extratwo1", extra2 = "extratwo2")
filter_three <- filter_var("dataname1", "varname3", 1:10/10, 0.2, "doubles", TRUE, FALSE)


#' complete filter settings
#'
#' Collate single variable filter states into a complete filter specification.
#'
#' @param ... any number of `teal_slice` objects
#' @filterable `named list` of `character` vectors, specifying which variables
#'              (list elements) can be filtered in which data sets (list names)
#'
#' @return
#' Object of class `teal_slices`, which is an unnamed list of `teal_slice` objects.
#'
#' @examples
#' all_filters <- filter_settings(filter_one, filter_two, filter_three)
#'
#' @export
#' @rdname new_api
#'
filter_settings <- function(..., filterable = list()) {
  checkmate::assert_list(filterable, names = "named", types = "character")
  args <- list(...)
  checkmate::assert_list(args, types = "teal_slice", any.missing = FALSE)
  class(args) <- "teal_slices"
  args
}
all_filters <- filter_settings(filter_one, filter_two, filter_three)

print.teal_slice <- function(x) str(x)
print.teal_slices <- function(x) lapply(x, str)


# get get contents of extra fields in all slices
find_extras <- function(tss) {
  checkmate::assert_class(tss, "teal_slice")
  ans <- tss[!is.element(names(tss), formalArgs(filter_var))]
  class(ans) <- "teal_slice"
  ans
}
lapply(all_filters, find_extras)

# get slices that where freature is value
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
