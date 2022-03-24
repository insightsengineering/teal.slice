#' Create a new `FilteredData` object
#'
#' @param x an object that inherits from `TealData`
#'
#' @return a (`CDISCTealDataset`, `CDISCTealDatasetConnector`) object
#' @keywords internal
#'
#' @noRd
filtered_data_new <- function(x) {
  UseMethod("filtered_data_new")
}

#' @keywords internal
#' @export
filtered_data_new.TealData <- function(x) { # nolintr
  FilteredData$new()
}

#' @keywords internal
#' @export
filtered_data_new.CDISCTealData <- function(x) { # nolintr
  CDISCFilteredData$new()
}

#' Set `FilteredData` with data from `TealData`
#'
#' @param data an object that inherits from `TealData`
#' @param datasets an object that inherits from `FilteredData`
#'
#' @return modified `FilteredData` object
#' @keywords internal
#'
#' @noRd
filtered_data_set <- function(data, datasets) { # nolintr
  UseMethod("filtered_data_set", data)
}

#' @keywords internal
#' @export
filtered_data_set.TealData <- function(data, datasets) { # nolintr
  datasets$set_code(data$get_code_class())
  for (dataset in data$get_datasets()) {
    datasets$set_dataset(dataset)
  }
  datasets$set_check(data$get_check())
  return(invisible(NULL))
}

#' Managing `FilteredData` states
#'
#' @description `r lifecycle::badge("experimental")`
#' Set, get and remove filter states of `FilteredData` object
#'
#' @name filter_state_api
#'
#' @param datasets (`FilteredData`)\cr
#'   object to store filter state and filtered datasets, shared across modules. For more
#'   details see [`FilteredData`]
#'
#' @param filter (`list`)\cr
#'   You can define filters that show when the app starts. List names should be
#'   named according to datanames passed to the `data` argument.
#'   In case of  data.frame` the list should be composed as follows:
#'   ```
#'   list(<dataname1> = list(<varname1> = ..., <varname2> = ...),
#'        <dataname2> = list(...),
#'        ...)
#'
#'   ```
#'
#'   For example, filters for variable `Sepal.Length` in `iris` can be specified as
#'   follows:
#'   ```
#'   list(iris = list(Sepal.Length = list(selected = c(5.0, 7.0))))
#'   # or
#'   list(iris = list(Sepal.Length = c(5.0, 7.0)))
#'   ```
#'
#'   In case developer would like to include `NA` and `Inf` values in  the
#'   filtered dataset.
#'   ```
#'   list(Species = list(selected = c(5.0, 7.0), keep_na = TRUE, keep_inf = TRUE))
#'   list(Species = c(c(5.0, 7.0), NA, Inf))
#'   ```
#'
#'   To initialize with specific variable filter with all values on start, one
#'   can use
#'   ```
#'   list(Species = list())
#'   ```
#'   `filter` should be set with respect to the class of the column:
#'   * `numeric`: `selected` should be a two elements vector defining the range
#'   of the filter.
#'   * `Date`: `selected` should be a two elements vector defining the date-range
#'   of the filter
#'   * `POSIXct`: `selected` should be a two elements vector defining the
#'   `datetime` range of the filter
#'   * `character` and `factor`: `selected` should be a vector of any length
#'   defining initial values selected to filter.
#'   \cr
#'   `MultiAssayExperiment` `filter` should be specified in slightly different
#'   way. Since [MultiAssayExperiment::MultiAssayExperiment()] contains
#'   patient data ([SummarizedExperiment::colData()]) with list of experiments
#'   ([MultiAssayExperiment::ExperimentList()]), `filter` list should be named
#'   in the following name.
#'   \cr
#'
#'   ```
#'   list(
#'     <MAE dataname> = list(
#'       subjects = list(<column in colData> = ..., <column in colData> = ...),
#'       <experiment name> = list(
#'         subset = list(<column in rowData of experiment> = ...,
#'                       <column in rowData of experiment> = ...),
#'         select = list(<column in colData of experiment> = ...,
#'                       <column in colData of experiment> = ...)
#'       )
#'     )
#'   )
#'   ```
#'   `filter` is ignored if the app is restored from a bookmarked state.
#'
#' @return
#' - set, remove and clear returns `NULL`
#' - get returns named `list` of the same structure as described in `filter` argument.
#'
#' @examples
#' datasets <- teal.slice:::FilteredData$new()
#' datasets$set_dataset(teal.data::dataset("iris", iris))
#' utils::data(miniACC, package = "MultiAssayExperiment")
#' datasets$set_dataset(teal.data::dataset("mae", miniACC))
#' fs <- list(
#'   iris = list(
#'     Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
#'     Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
#'   ),
#'   mae = list(
#'     subjects = list(
#'       years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
#'       vital_status = list(selected = "1", keep_na = FALSE),
#'       gender = list(selected = "female", keep_na = TRUE)
#'     ),
#'     RPPAArray = list(
#'       subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
#'     )
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
#'   filter = list(iris = list(Species = list(selected = "setosa", keep_na = TRUE)))
#' )
#'
#' # remove specific filters
#' remove_filter_state(datasets,
#'   filter = list(
#'     iris = "Species",
#'     mae = list(
#'       subjects = c("years_to_birth", "vital_status")
#'     )
#'   )
#' )
#'
#' # remove all states
#' clear_filter_states(datasets)
NULL

#' @rdname filter_state_api
#' @export
set_filter_state <- function(datasets, filter) {
  checkmate::assert_class(datasets, "FilteredData")
  checkmate::assert_list(filter, min.len = 0, null.ok = TRUE)
  if (length(filter) > 0) {
    datasets$set_filter_state(filter)
  }
  invisible(NULL)
}

#' @rdname filter_state_api
#' @export
get_filter_state <- function(datasets) {
  checkmate::assert_class(datasets, "FilteredData")
  if (shiny::isRunning()) {
    datasets$get_filter_state()
  } else {
    isolate(datasets$get_filter_state())
  }
}

#' @rdname filter_state_api
#' @export
remove_filter_state <- function(datasets, filter) {
  checkmate::assert_class(datasets, "FilteredData")
  checkmate::assert_list(filter, min.len = 0, null.ok = TRUE)
  if (length(filter) > 0) {
    datasets$remove_filter_state(filter)
  }
  invisible(NULL)
}

#' @rdname filter_state_api
#' @export
clear_filter_states <- function(datasets) {
  checkmate::assert_class(datasets, "FilteredData")
  datasets$remove_all_filter_states()
  invisible(NULL)
}
