#' @export
init_filtered_data <- function(x, ...) {
  UseMethod("init_filtered_data")
}

#' @keywords internal
#' @export
init_filtered_data.TealData <- function(x, ...) { # nolint
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
  FilteredData$new(
    data_objects = data_objects,
    join_keys = x$get_join_keys()$get(),
    code = x$get_code_class(),
    check = x$get_check()
  )
}

#' @keywords internal
#' @export
init_filtered_data.CDISCTealData <- function(x, ...) { # nolint
  data_objects <- lapply(x$get_datanames(), function(dataname) {
    dataset <- x$get_dataset(dataname)

    # CDISCTealData can contain TealDataset and CDISCTealDataset objects
    # the former to not have a get_parent() call
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

  CDISCFilteredData$new(
    data_objects = data_objects,
    join_keys = x$get_join_keys()$get(),
    code = x$get_code_class(),
    check = x$get_check()
  )
}

#' TODO documentation
#' @export
init_filtered_data.default <- function(x, join_keys = NULL, code = NULL, cdisc = FALSE) {
  checkmate::check_flag(cdisc)
  datasets <- if (cdisc) {
    CDISCFilteredData$new(x, join_keys = join_keys, code = code)
  } else {
    FilteredData$new(x, join_keys = join_keys, code = code)
  }
}


#' TODO
validate_dataset_args <- function(dataset_args, dataname, allowed_parent = FALSE) {
  return(TRUE)
}
