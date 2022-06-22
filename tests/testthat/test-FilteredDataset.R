testthat::test_that("The constructor accepts a data.frame object and dataname", {
  testthat::expect_error(FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  ), NA)
})

testthat::test_that("queues_empty does not throw after initializing FilteredDataset", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_error(filtered_dataset$queues_empty(), NA)
})

testthat::test_that("get_filter_states returns an empty list after initialization", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(filtered_dataset$get_filter_states(), list())
})

testthat::test_that("get_dataname returns the dataname passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(filtered_dataset$get_dataname(), "iris")
})

testthat::test_that("get_dataset returns the dataset passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(filtered_dataset$get_dataset(), head(iris))
})

testthat::test_that("get_dataset_label retruns the dataset label passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris", label = "dataset label"
  )
  testthat::expect_equal(filtered_dataset$get_dataset_label(), "dataset label")
})

testthat::test_that("get_hash returns the hash of the data.frame passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_equal(digest::digest(head(iris), algo = "md5"), filtered_dataset$get_hash())
})

testthat::test_that("get_keys returns the keys passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris", keys = c("Petal.length"))
  testthat::expect_equal("Petal.length", filtered_dataset$get_keys())
})

testthat::test_that("get_varlabels(NULL) returns a named array of NAs if data.frame has no varlabels", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(
    filtered_dataset$get_varlabels(),
    setNames(as.character(rep(NA, ncol(head(iris)))), nm = names(iris))
  )
})

testthat::test_that("get_varlabels returns labels for the part of the variables only", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(
    filtered_dataset$get_varlabels(variables = c("Petal.Length")),
    setNames(object = as.character(NA), nm = "Petal.Length")
  )
})

testthat::test_that("get_varnames returns the names of the variables in the data passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(filtered_dataset$get_varnames(), names(iris))
})

testthat::test_that("get_filtered_dataname returns <dataname>_FILTERED as default", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_equal(filtered_dataset$get_filtered_dataname(), "iris_FILTERED")
})

testthat::test_that("ui_add_filter_state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_error(filtered_dataset$ui_add_filter_state(), regex = "Pure virtual")
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
  dataset$set_filter_state(state = fs)
  states <- dataset$get_filter_states()[[1]]

  shiny::isolate(testthat::expect_equal(
    dataset$get_formatted_filter_state(),
    paste("Filters for dataset: iris", shiny::isolate(states$format(indent = 2)), sep = "\n")
  ))
})

testthat::test_that("$get_call returns the filter call of the dataset", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dataset$set_filter_state(state = fs)
  filter_call <- shiny::isolate(dataset$get_call())$filter

  testthat::expect_equal(
    deparse1(filter_call),
    paste0(
      "iris_FILTERED <- dplyr::filter(iris, (is.na(Sepal.Length) | (is.infinite(Sepal.Length) |",
      " Sepal.Length >= 5.1 & Sepal.Length <= 6.4)) & Species %in% c(\"setosa\", \"versicolor\"))"
    )
  )
})

