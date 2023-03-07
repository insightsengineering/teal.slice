testthat::test_that("The constructor accepts a data.frame object and dataname", {
  testthat::expect_no_error(FilteredDataset$new(dataset = head(iris), dataname = "iris"))
})

testthat::test_that("state_lists_empty does not throw after initializing FilteredDataset", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_no_error(filtered_dataset$clear_filter_states())
})

testthat::test_that("filter_states is empty when not initialized", {
  testfd <- R6::R6Class(
    "testfd",
    inherit = FilteredDataset,
    public = list(
      get_filter_states = function() private$filter_states
    )
  )
  filtered_dataset <- testfd$new(dataset = head(iris), dataname = "iris")
  testthat::expect_identical(filtered_dataset$get_filter_states(), list())
})

testthat::test_that("get_dataname returns the dataname passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_equal(filtered_dataset$get_dataname(), "iris")
})

testthat::test_that("get_dataset returns the dataset passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_equal(filtered_dataset$get_dataset(), head(iris))
})

testthat::test_that("get_dataset_label retruns the dataset label passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris", label = "dataset label")
  testthat::expect_equal(filtered_dataset$get_dataset_label(), "dataset label")
})

testthat::test_that("get_keys returns the keys passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris", keys = c("Petal.length"))
  testthat::expect_equal("Petal.length", filtered_dataset$get_keys())
})

testthat::test_that("ui_add is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_error(filtered_dataset$ui_add(), regex = "Pure virtual")
})

testthat::test_that("get_metadata returns the metadata of the data passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris", metadata = list(A = "A", B = TRUE, C = 5)
  )
  testthat::expect_equal(filtered_dataset$get_metadata(), list(A = "A", B = TRUE, C = 5))

  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_null(filtered_dataset$get_metadata())
})

# Format
testthat::test_that("$get_formatted_filter_state returns a string representation of filters", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  shiny::isolate(dataset$set_filter_state(state = fs))

  testthat::expect_equal(
    shiny::isolate(dataset$get_formatted_filter_state()),
    paste(
      c(
        "Filters for dataset: iris", "  Filtering on: Sepal.Length",
        "    Selected range: 5.100 - 6.400",
        "    Include missing values: TRUE",
        "  Filtering on: Species",
        "    Selected values: setosa, versicolor",
        "    Include missing values: FALSE"
      ),
      collapse = "\n"
    )
  )
})

testthat::test_that("$get_call returns the filter call of the dataset", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  shiny::isolate(dataset$set_filter_state(state = fs))
  filter_call <- shiny::isolate(dataset$get_call())$filter

  testthat::expect_equal(
    filter_call,
    quote(
      iris <- dplyr::filter(
        iris,
        (is.na(Sepal.Length) |
          (is.infinite(Sepal.Length) | Sepal.Length >= 5.1 & Sepal.Length <= 6.4)) &
          Species %in% c("setosa", "versicolor")
      )
    )
  )
})
