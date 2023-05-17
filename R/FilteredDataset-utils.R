#' Initializes `FilteredDataset`
#'
#' @keywords internal
#' @examples
#' # DefaultFilteredDataset example
#' iris_fd <- teal.slice:::init_filtered_dataset(
#'   iris,
#'   dataname = "iris",
#'   metadata = list(type = "teal")
#' )
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     iris_fd$ui_add(id = "add"),
#'     iris_fd$ui_active("dataset"),
#'     verbatimTextOutput("call"),
#'     verbatimTextOutput("metadata")
#'   ),
#'   server = function(input, output, session) {
#'     iris_fd$srv_add(id = "add")
#'     iris_fd$srv_active(id = "dataset")
#'
#'     output$metadata <- renderText({
#'       paste("Type =", iris_fd$get_metadata()$type)
#'     })
#'
#'     output$call <- renderText({
#'       paste(
#'         vapply(iris_fd$get_call(), deparse1, character(1), collapse = "\n"),
#'         collapse = "\n"
#'       )
#'     })
#'   }
#' )
#' }
#'
#' # MAEFilteredDataset example
#' library(MultiAssayExperiment)
#' data(miniACC)
#' MAE_fd <- teal.slice:::init_filtered_dataset(miniACC, "MAE", metadata = list(type = "MAE"))
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     MAE_fd$ui_add(id = "add"),
#'     MAE_fd$ui_active("dataset"),
#'     verbatimTextOutput("call"),
#'     verbatimTextOutput("metadata")
#'   ),
#'   server = function(input, output, session) {
#'     MAE_fd$srv_add(id = "add")
#'     MAE_fd$srv_active(id = "dataset")
#'     output$metadata <- renderText({
#'       paste("Type =", MAE_fd$get_metadata()$type)
#'     })
#'     output$call <- renderText({
#'       paste(
#'         vapply(MAE_fd$get_call(), deparse1, character(1), collapse = "\n"),
#'         collapse = "\n"
#'       )
#'     })
#'   }
#' )
#' }
#' @param dataset (`data.frame` or `MultiAssayExperiment`)\cr
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#' @param keys optional, (`character`)\cr
#'   Vector with primary keys
#' @param parent_name (`character(1)`)\cr
#'   Name of the parent dataset
#' @param parent (`reactive`)\cr
#'   object returned by this reactive is a filtered `data.frame` from other `FilteredDataset`
#'   named `parent_name`. Consequence of passing `parent` is a `reactive` link which causes
#'   causing re-filtering of this `dataset` based on the changes in `parent`.
#' @param join_keys (`character`)\cr
#'   Name of the columns in this dataset to join with `parent`
#'   dataset. If the column names are different if both datasets
#'   then the names of the vector define the `parent` columns.
#' @param label (`character`)\cr
#'   Label to describe the dataset
#' @param metadata (named `list` or `NULL`) \cr
#'   Field containing metadata about the dataset. Each element of the list
#'   should be atomic and length one.
#' @export
#' @note Although this function is exported for use in other packages, it may be changed or removed in a future release
#'   at which point any code which relies on this exported function will need to be changed.
init_filtered_dataset <- function(dataset, # nolint
                                  dataname,
                                  keys = character(0),
                                  parent_name = character(0),
                                  parent = reactive(dataset),
                                  join_keys = character(0),
                                  label = attr(dataset, "label"),
                                  metadata = NULL) {
  UseMethod("init_filtered_dataset")
}

#' @keywords internal
#' @export
init_filtered_dataset.data.frame <- function(dataset, # nolint
                                             dataname,
                                             keys = character(0),
                                             parent_name = character(0),
                                             parent = NULL,
                                             join_keys = character(0),
                                             label = attr(dataset, "label"),
                                             metadata = NULL) {
  DefaultFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    keys = keys,
    parent_name = parent_name,
    parent = parent,
    join_keys = join_keys,
    label = label,
    metadata = metadata
  )
}

#' @keywords internal
#' @export
init_filtered_dataset.MultiAssayExperiment <- function(dataset, # nolint
                                                       dataname,
                                                       keys = character(0),
                                                       parent_name, # ignored
                                                       parent, # ignored
                                                       join_keys, # ignored
                                                       label = attr(dataset, "label"),
                                                       metadata = NULL) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    keys = keys,
    label = label,
    metadata = metadata
  )
}

#' Gets supported filterable variable names
#'
#' Gets filterable variable names from a given object. The names match variables
#' of classes in an array `teal.slice:::.filterable_class`.
#' @param data (`object`)\cr
#'   the R object containing elements which class can be checked through `vapply` or `apply`.
#'
#' @examples
#' df <- data.frame(
#'   a = letters[1:3],
#'   b = 1:3,
#'   c = Sys.Date() + 1:3,
#'   d = Sys.time() + 1:3,
#'   z = complex(3)
#' )
#' teal.slice:::get_supported_filter_varnames(df)
#' @return `character` the array of the matched element names
#' @keywords internal
get_supported_filter_varnames <- function(data) {
  UseMethod("get_supported_filter_varnames")
}

#' @keywords internal
#' @export
get_supported_filter_varnames.default <- function(data) { # nolint
  is_expected_class <- vapply(
    X = data,
    FUN = function(x) any(class(x) %in% .filterable_class),
    FUN.VALUE = logical(1)
  )
  names(is_expected_class[is_expected_class])
}

#' @keywords internal
#' @export
get_supported_filter_varnames.matrix <- function(data) { # nolint
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(colnames(data))) {
    colnames(data)
  } else {
    character(0)
  }
}

#' @keywords internal
#' @export
get_supported_filter_varnames.MultiAssayExperiment <- function(data) { # nolint
  data <- SummarizedExperiment::colData(data)
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(names(data))) {
    names(data)
  } else {
    character(0)
  }
}

#' @title Returns a `choices_labeled` object
#'
#' @param data (`data.frame`, `DFrame`, `list`)\cr
#'   where labels can be taken from in case when `varlabels` is not specified.
#'   `data` must be specified if `varlabels` is not specified.
#' @param choices (`character`)\cr
#'  the array of chosen variables
#' @param varlabels (`character`)\cr
#'  the labels of variables in data
#' @param keys (`character`)\cr
#'  the names of the key columns in data
#' @return `character(0)` if choices are empty; a `choices_labeled` object otherwise
#' @keywords internal
data_choices_labeled <- function(data,
                                 choices,
                                 varlabels = formatters::var_labels(data, fill = TRUE),
                                 keys = character(0)) {
  if (length(choices) == 0) {
    return(character(0))
  }
  choice_types <- setNames(variable_types(data = data, columns = choices), choices)
  choice_types[keys] <- "primary_key"

  choices_labeled(
    choices = choices,
    labels = unname(varlabels[choices]),
    types = choice_types[choices]
  )
}

get_varlabels <- function(data) {
  if (!is.array(data)) {
    vapply(
      colnames(data),
      FUN = function(x) {
        label <- attr(data[[x]], "label")
        if (is.null(label)) {
          x
        } else {
          label
        }
      },
      FUN.VALUE = character(1)
    )
  } else {
    character(0)
  }
}
