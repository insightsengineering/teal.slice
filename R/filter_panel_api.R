#' Managing `FilteredData` states
#'
#' @description `r lifecycle::badge("experimental")`
#' Set, get and remove filter states of `FilteredData` object
#'
#' @name filter_state_api
#'
#' @param datasets (`FilteredData`)\cr
#'   object to store filter state and filtered datasets, shared across modules\cr
#'   see [`FilteredData`] for details
#'
#' @param filter (`teal_slices`)\cr
#'   specify filters in place on app start-up
#'
#' @return
#' - `set_*`, `remove_*` and `clear_filter_state` return `NULL` invisibly
#' - `get_filter_state` returns a named `teal_slices` object
#'    containing a `teal_slice` for every existing `FilterState`
#'
#' @seealso [`teal_slice`]
#'
#' @examples
#' utils::data(miniACC, package = "MultiAssayExperiment")
#'
#' datasets <- init_filtered_data(
#'   x = list(
#'     iris = list(dataset = iris),
#'     mae = list(dataset = miniACC)
#'   )
#' )
#' fs <- filter_settings(
#'   filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor")),
#'   filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
#'   filter_var(
#'     dataname = "mae", varname = "years_to_birth", selected = c(30, 50),
#'     keep_na = TRUE, keep_inf = FALSE, datalabel = "subjects"
#'   ),
#'   filter_var(
#'     dataname = "mae", varname = "vital_status", selected = "1",
#'     keep_na = FALSE, datalabel = "subjects"
#'   ),
#'   filter_var(
#'     dataname = "mae", varname = "gender", selected = "female",
#'     keep_na = TRUE, datalabel = "subjects"
#'   ),
#'   filter_var(
#'     dataname = "mae", varname = "ARRAY_TYPE", selected = "",
#'     keep_na = TRUE, datalabel = "RPPAArray", arg = "subset"
#'   )
#' )
#'
#' # set initial filter state
#' set_filter_state(datasets, filter = fs)
#'
#' # get filter state
#' get_filter_state(datasets)
#'
#' # modify filter state
#' set_filter_state(
#'   datasets,
#'   filter_settings(
#'     filter_var(dataname = "iris", varname = "Species", selected = "setosa", keep_na = TRUE)
#'   )
#' )
#'
#' # remove specific filters
#' remove_filter_state(
#'   datasets,
#'   filter_settings(
#'     filter_var(dataname = "iris", varname = "Species"),
#'     filter_var(dataname = "mae", varname = "years_to_birth"),
#'     filter_var(dataname = "mae", varname = "vital_status")
#'   )
#' )
#'
#' # remove all states
#' clear_filter_states(datasets)
NULL

#' @rdname filter_state_api
#' @export
set_filter_state <- function(datasets, filter) {
  checkmate::assert_multi_class(datasets, c("FilteredData", "FilterPanelAPI"))
  checkmate::assert(
    checkmate::check_class(filter, "teal_slices"),
    checkmate::check_list(filter, min.len = 0, null.ok = TRUE)
  )
  if (!is.teal_slices(filter)) {
    filter <- as.teal_slices(filter)
  }

  datasets$set_filter_state(filter)
  invisible(NULL)
}

#' @rdname filter_state_api
#' @export
get_filter_state <- function(datasets) {
  checkmate::assert_multi_class(datasets, c("FilteredData", "FilterPanelAPI"))
  if (shiny::isRunning()) {
    datasets$get_filter_state()
  } else {
    shiny::isolate(datasets$get_filter_state())
  }
}

#' @rdname filter_state_api
#' @export
remove_filter_state <- function(datasets, filter) {
  checkmate::assert_multi_class(datasets, c("FilteredData", "FilterPanelAPI"))
  checkmate::assert(
    checkmate::check_class(filter, "teal_slices"),
    checkmate::check_list(filter, min.len = 0, null.ok = TRUE)
  )

  datasets$remove_filter_state(filter)
  invisible(NULL)
}

#' @rdname filter_state_api
#' @export
clear_filter_states <- function(datasets) {
  checkmate::assert_multi_class(datasets, c("FilteredData", "FilterPanelAPI"))
  datasets$clear_filter_states()
  invisible(NULL)
}
