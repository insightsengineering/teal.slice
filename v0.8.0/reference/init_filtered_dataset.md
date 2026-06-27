# Initialize `FilteredDataset`

Initializes a `FilteredDataset` object corresponding to the class of the
filtered dataset.

## Usage

``` r
init_filtered_dataset(
  dataset,
  dataname,
  keys = character(0),
  parent_name = character(0),
  parent = reactive(dataset),
  join_keys = character(0),
  label = attr(dataset, "label", exact = TRUE)
)
```

## Arguments

- dataset:

  any object

- dataname:

  (`character(1)`) syntactically valid name given to the dataset.

- keys:

  (`character`) optional vector of primary key column names.

- parent_name:

  (`character(1)`) name of the parent dataset.

- parent:

  (`reactive`) that returns a filtered `data.frame` from other
  `FilteredDataset` named `parent_name`. Passing `parent` results in a
  `reactive` link that causes re-filtering of this `dataset` based on
  the changes in `parent`.

- join_keys:

  (`character`) vector of names of columns in this dataset to join with
  `parent` dataset. If column names in the parent do not match these,
  they should be given as the names of this vector.

- label:

  (`character(1)`) label to describe the dataset.

## Value

Object of class `FilteredDataset`.

## Warning

This function is exported to allow other packages to extend `teal.slice`
but it is treated as internal. Breaking changes may occur without
warning. We recommend consulting the package maintainer before using it.

## Examples

``` r
# DataframeFilteredDataset example
library(shiny)

iris_fd <- init_filtered_dataset(iris, dataname = "iris")
ui <- bslib::page_fluid(
  iris_fd$ui_add(id = "add"),
  iris_fd$ui_active("dataset"),
  verbatimTextOutput("call")
)
server <- function(input, output, session) {
  iris_fd$srv_add(id = "add")
  iris_fd$srv_active(id = "dataset")

  output$call <- renderText({
    paste(
      vapply(iris_fd$get_call(), deparse1, character(1), collapse = "\n"),
      collapse = "\n"
    )
  })
}
if (interactive()) {
  shinyApp(ui, server)
}

# \donttest{
if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
  # MAEFilteredDataset example
  library(shiny)

  data(miniACC, package = "MultiAssayExperiment")

  MAE_fd <- init_filtered_dataset(miniACC, "MAE")
  ui <- fluidPage(
    MAE_fd$ui_add(id = "add"),
    MAE_fd$ui_active("dataset"),
    verbatimTextOutput("call")
  )
  server <- function(input, output, session) {
    MAE_fd$srv_add(id = "add")
    MAE_fd$srv_active(id = "dataset")
    output$call <- renderText({
      paste(
        vapply(MAE_fd$get_call(), deparse1, character(1), collapse = "\n"),
        collapse = "\n"
      )
    })
  }
  if (interactive()) {
    shinyApp(ui, server)
  }
}
# }
```
