#' Initialize `FilteredData`
#'
#' Initialize `FilteredData`
#' @param x (named `list` or `TealData`) In case of `TealData` see [teal.data::teal_data()].
#'  If the list is provided, it should contain `list`(s) containing following fields:
#' - `dataset` data object object supported by [`FilteredDataset`].
#' - `metatada` (optional) additional metadata attached to the `dataset`.
#' - `keys` (optional) primary keys.
#' - `datalabel` (optional) label describing the `dataset`.
#' - `parent` (optional) which `dataset` is a parent of this one.
#' @param join_keys (`JoinKeys`) see [teal.data::join_keys()].
#' @param code (`CodeClass`) see [`teal.data::CodeClass`].
#' @param cdisc (`logical(1)`) whether data is of `cdisc` type (relational).
#' @param check (`logical(1)`) whether data has been check against reproducibility.
#' @examples
#' library(shiny)
#' datasets <- teal.slice::init_filtered_data(
#'   x = list(
#'     iris = list(dataset = iris),
#'     mtcars = list(dataset = mtcars, metadata = list(type = "training"))
#'   )
#' )
#' @export
init_filtered_data <- function(x, join_keys, code, cdisc, check) {
  UseMethod("init_filtered_data")
}

#' @keywords internal
#' @export
init_filtered_data.TealData <- function(x, # nolint
                                        join_keys = x$get_join_keys(),
                                        code = x$get_code_class(),
                                        cdisc = FALSE,
                                        check = x$get_check()) {
  data_objects <- lapply(x$get_datanames(), function(dataname) {
    dataset <- x$get_dataset(dataname)
    list(
      dataset = dataset$get_raw_data(),
      keys = dataset$get_keys(),
      metadata = dataset$get_metadata(),
      label = dataset$get_dataset_label()
    )
  })
  names(data_objects) <- x$get_datanames()

  init_filtered_data(
    x = data_objects,
    join_keys = join_keys,
    code = code,
    check = check
  )
}

#' @keywords internal
#' @export
init_filtered_data.CDISCTealData <- function(x, # nolint
                                             join_keys = x$get_join_keys(),
                                             code = x$get_code_class(),
                                             cdisc = TRUE,
                                             check = x$get_check()) {
  data_objects <- lapply(x$get_datanames(), function(dataname) {
    dataset <- x$get_dataset(dataname)

    # CDISCTealData can contain TealDataset and CDISCTealDataset objects
    # the former do not have a get_parent() call
    parent <- if (inherits(dataset, "CDISCTealDataset")) {
      dataset$get_parent()
    } else {
      NULL
    }

    list(
      dataset = dataset$get_raw_data(),
      keys = dataset$get_keys(),
      metadata = dataset$get_metadata(),
      label = dataset$get_dataset_label(),
      parent = parent
    )
  })
  names(data_objects) <- x$get_datanames()

  init_filtered_data(
    x = data_objects,
    join_keys = join_keys,
    code = code,
    check = check,
    cdisc = cdisc
  )
}

#' @keywords internal
#' @export
init_filtered_data.default <- function(x, join_keys = NULL, code = NULL, cdisc = FALSE, check = FALSE) { # nolint
  checkmate::assert_list(x, any.missing = FALSE, names = "unique")
  mapply(validate_dataset_args, x, names(x), MoreArgs = list(allowed_parent = cdisc))
  checkmate::assert_class(code, "CodeClass", null.ok = TRUE)
  checkmate::assert_class(join_keys, "JoinKeys", null.ok = TRUE)
  checkmate::check_flag(cdisc)
  checkmate::check_flag(check)

  datasets <- if (cdisc) {
    CDISCFilteredData$new(x, join_keys = join_keys, code = code, check = check)
  } else {
    FilteredData$new(x, join_keys = join_keys, code = code, check = check)
  }
}

#' Validate dataset arguments
#'
#' Validate dataset arguments
#' @param dataset_args (`list`)\cr
#'   containing the arguments except (`dataname`)
#'   needed by `init_filtered_dataset`
#' @param dataname (`character(1)`)\cr
#'   the name of the `dataset` to be added to this object
#' @param allowed_parent (`logical(1)`)\cr
#'   whether `FilteredDataset` can have a parent - i.e. if it's a part of `CDISCFilteredData`
#' @keywords internal
#' @return (`NULL` or throws an error)
validate_dataset_args <- function(dataset_args, dataname, allowed_parent = FALSE) {
  check_simple_name(dataname)
  checkmate::assert_flag(allowed_parent)
  checkmate::assert_list(dataset_args, names = "unique")

  allowed_names <- c("dataset", "keys", "label", "metadata")
  if (allowed_parent) {
    allowed_names <- c(allowed_names, "parent")
  }

  checkmate::assert_subset(names(dataset_args), choices = allowed_names)

  checkmate::assert_multi_class(dataset_args[["dataset"]], classes = c("data.frame", "MultiAssayExperiment"))
  checkmate::assert_character(dataset_args[["keys"]], null.ok = TRUE)
  teal.data::validate_metadata(dataset_args[["metadata"]])
  checkmate::assert_character(dataset_args[["label"]], null.ok = TRUE, min.len = 0, max.len = 1)
  checkmate::assert_character(dataset_args[["parent"]], null.ok = TRUE, min.len = 0, max.len = 1)
}
