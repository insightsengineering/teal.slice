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
#' app <- shinyApp(
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
#' if (interactive()) {
#'   runApp(app)
#' }
#'
#' # MAEFilteredDataset example
#' library(MultiAssayExperiment)
#' data(miniACC)
#' MAE_fd <- teal.slice:::init_filtered_dataset(miniACC, "MAE", metadata = list(type = "MAE"))
#' app <- shinyApp(
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
#' if (interactive()) {
#'   runApp(app)
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
