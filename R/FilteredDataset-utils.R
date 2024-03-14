#' Initialize `FilteredDataset`
#'
#' Initializes a `FilteredDataset` object corresponding to the class of the filtered dataset.
#'
#' @param dataset any object
#' @param dataname (`character(1)`)
#'   syntactically valid name given to the dataset.
#' @param keys (`character`) optional
#'   vector of primary key column names.
#' @param parent_name (`character(1)`)
#'   name of the parent dataset.
#' @param parent (`reactive`)
#'   that returns a filtered `data.frame` from other `FilteredDataset` named `parent_name`.
#'   Passing `parent` results in a `reactive` link that causes re-filtering of this `dataset`
#'   based on the changes in `parent`.
#' @param join_keys (`character`)
#'   vector of names of columns in this dataset to join with `parent` dataset.
#'   If column names in the parent do not match these, they should be given as the names of this vector.
#' @param label (`character(1)`)
#'   label to describe the dataset.
#'
#' @return Object of class `FilteredDataset`.
#'
#' @section Warning:
#' This function is exported to allow other packages to extend `teal.slice` but it is treated as internal.
#' Breaking changes may occur without warning.
#' We recommend consulting the package maintainer before using it.
#'
#' @examples
#' # DataframeFilteredDataset example
#' library(shiny)
#'
#' iris_fd <- init_filtered_dataset(iris, dataname = "iris")
#' ui <- fluidPage(
#'   iris_fd$ui_add(id = "add"),
#'   iris_fd$ui_active("dataset"),
#'   verbatimTextOutput("call")
#' )
#' server <- function(input, output, session) {
#'   iris_fd$srv_add(id = "add")
#'   iris_fd$srv_active(id = "dataset")
#'
#'   output$call <- renderText({
#'     paste(
#'       vapply(iris_fd$get_call(), deparse1, character(1), collapse = "\n"),
#'       collapse = "\n"
#'     )
#'   })
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
#'   # MAEFilteredDataset example
#'   library(shiny)
#'
#'   data(miniACC, package = "MultiAssayExperiment")
#'
#'   MAE_fd <- init_filtered_dataset(miniACC, "MAE")
#'   ui <- fluidPage(
#'     MAE_fd$ui_add(id = "add"),
#'     MAE_fd$ui_active("dataset"),
#'     verbatimTextOutput("call")
#'   )
#'   server <- function(input, output, session) {
#'     MAE_fd$srv_add(id = "add")
#'     MAE_fd$srv_active(id = "dataset")
#'     output$call <- renderText({
#'       paste(
#'         vapply(MAE_fd$get_call(), deparse1, character(1), collapse = "\n"),
#'         collapse = "\n"
#'       )
#'     })
#'   }
#'   if (interactive()) {
#'     shinyApp(ui, server)
#'   }
#' }
#' }
#' @keywords internal
#' @export
init_filtered_dataset <- function(dataset,
                                  dataname,
                                  keys = character(0),
                                  parent_name = character(0),
                                  parent = reactive(dataset),
                                  join_keys = character(0),
                                  label = attr(dataset, "label", exact = TRUE)) {
  UseMethod("init_filtered_dataset")
}

#' @keywords internal
#' @export
init_filtered_dataset.data.frame <- function(dataset,
                                             dataname,
                                             keys = character(0),
                                             parent_name = character(0),
                                             parent = NULL,
                                             join_keys = character(0),
                                             label = attr(dataset, "label", exact = TRUE)) {
  DataframeFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    keys = keys,
    parent_name = parent_name,
    parent = parent,
    join_keys = join_keys,
    label = label
  )
}

#' @keywords internal
#' @export
init_filtered_dataset.MultiAssayExperiment <- function(dataset,
                                                       dataname,
                                                       keys = character(0),
                                                       parent_name, # ignored
                                                       parent, # ignored
                                                       join_keys, # ignored
                                                       label = attr(dataset, "label", exact = TRUE)) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    keys = keys,
    label = label
  )
}

#' @keywords internal
#' @export
init_filtered_dataset.default <- function(dataset,
                                          dataname,
                                          keys, # ignored
                                          parent_name, # ignored
                                          parent, # ignored
                                          join_keys, # ignored
                                          label = attr(dataset, "label", exact = TRUE)) {
  DefaultFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    label = label
  )
}
