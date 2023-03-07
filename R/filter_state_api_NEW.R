
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
#' @param varlabel `character(0-1)` optional variable label
#' @param keep_na `logical(0-1)` optional logical flag specifying whether to keep missing values
#' @param keep_inf `logical(0-1)` optional logical flag specifying whether to keep infinite values
#' @param fixed `logical(1)` logical flag specifying whether to fix this filter state (i.e. forbid setting state)
#'
#' @return
#' Object of class `teal_slice`, which is a named list.
#'
#' @examples
#' filter_one <- filter_var("dataname1", "varname1", letters, "b", "characters", FALSE)
#' filter_two <- filter_var("dataname1", "varname2", 1:10, 2, "integers", TRUE, FALSE)
#' filter_three <- filter_var("dataname1", "varname3", 1:10/10, 0.2, "doubles", TRUE, FALSE)
#'
#' @export
#' @rdname new_api
#'
filter_var <- function(
    dataname,
    varname,
    choices = NULL,
    selected = NULL,
    varlabel = NULL,
    keep_na = NULL,
    keep_inf = NULL,
    fixed = FALSE
) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(varname)
  checkmate::assert_atomic(choices)
  checkmate::assert_atomic(selected)
  checkmate::assert_subset(selected, choices)
  checkmate::assert_string(varlabel, null.ok = TRUE)
  checkmate::assert_flag(keep_na, null.ok = TRUE)
  checkmate::assert_flag(keep_inf, null.ok = TRUE)
  checkmate::assert_flag(fixed)

  ans <- as.list(match.call())[-1]
  ans <- lapply(ans, eval)
  class(ans) <- "teal_slice"
  ans
}

# filter_one <- filter_var("dataname1", "varname1", letters, "b", "characters", FALSE, FALSE)
# filter_two <- filter_var("dataname1", "varname2", 1:10, 2, "integers", TRUE, FALSE)
# filter_three <- filter_var("dataname1", "varname3", 1:10/10, 0.2, "doubles", TRUE, FALSE)


#' complete filter settings
#'
#' Collate single variable filter states into a complete filter specification.
#'
#' @param ... any number of `teal_slice` objects
#' @filterable `named list` of `character` vectors, specifying which variables
#'              (list elements) can be filtered in which data sets (list names)
#' @count_type `character(1)` string specifiying how observations are tallied by these filter states
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
# filter_settings <- function(
#     ...,
#     filterable = list(),
#     count_type = c("none", "all", "hierarchical")
# ) {
#   checkmate::assert_list(filterable, names = "named", types = "character")
#   args <- list(...)
#   checkmate::assert_list(args, types = "teal_slice", any.missing = FALSE)
#   class(args) <- "teal_slices"
#   count_type <- match_arg(count_type)
#   args <- append(
#     args,
#     list(
#       filterable = filterable,
#       count_type = count_type
#     )
#   )
#   args
# }

# all_filters <- filter_settings(filter_one, filter_two, filter_three)
#
# print.teal_slice <- function(x) str(x)
# print.teal_slices <- function(x) lapply(x, str)


# get get contents of extra fields in all slices
# find_extras <- function(tss) {
#   checkmate::assert_class(tss, "teal_slice")
#   ans <- tss[!is.element(names(tss), formalArgs(filter_var))]
#   class(ans) <- "teal_slice"
#   ans
# }
# lapply(all_filters, find_extras)
#
# # get slices that where freature is value
# extract_by_feat <- function(tss, feature, value) {
#   checkmate::assert_class(tss, "teal_slices")
#   Filter(function(x) x[[feature]] == value, tss)
# }
# extract_by_feat(all_filters, "dataname", "dataname1")
#
# # get feature from all slices
# extract_feat <- function(tss, feature) {
#   checkmate::assert_class(tss, "teal_slices")
#   lapply(tss, function(x) x[[feature]])
# }
# extract_feat(all_filters, "dataname")
