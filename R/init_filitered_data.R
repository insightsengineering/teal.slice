#' @export
init_filtered_data <- function(x, ...) {
  UseMethod("init_filtered_data")
}


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

  init_filtered_data(
    x = data_objects,
    join_keys = x$get_join_keys(),
    code = x$get_code_class(),
    check = x$get_check()
  )
}


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

  init_filtered_data(
    x = data_objects,
    join_keys = x$get_join_keys(),
    code = x$get_code_class(),
    check = x$get_check(),
    cdisc = TRUE
  )
}

#' TODO documentation
#' @export
init_filtered_data.default <- function(x, join_keys = NULL, code = NULL, cdisc = FALSE, check = FALSE) {
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


#' TODO
validate_dataset_args <- function(dataset_args, dataname, allowed_parent = FALSE) {
  return(TRUE)
}
