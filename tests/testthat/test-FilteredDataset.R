# initialize ----
testthat::test_that("constructor creates FilteredDataset with empty filter_states", {
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

# clear_filter_states ----
testthat::test_that("clear_filter_states does not raise errors after initializing FilteredDataset", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_no_error(filtered_dataset$clear_filter_states())
})

# get_dataset ----
testthat::test_that("get_dataset returns the dataset passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_identical(filtered_dataset$get_dataset(), head(iris))
})

# get_dataname ----
testthat::test_that("get_dataname returns the dataname passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_identical(filtered_dataset$get_dataname(), "iris")
})

# get_dataset_label ----
testthat::test_that("get_dataset_label retruns the dataset label passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris", label = "dataset label")
  testthat::expect_identical(filtered_dataset$get_dataset_label(), "dataset label")
})

# get_keys ----
testthat::test_that("get_keys returns the keys passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris", keys = c("Petal.length"))
  testthat::expect_identical("Petal.length", filtered_dataset$get_keys())
})

# get_metadata ----
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

# format ----
testthat::test_that("format returns a string representation of filters", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dataset$set_filter_state(state = fs)

  testthat::expect_equal(
    shiny::isolate(dataset$format()),
    shiny::isolate(format(dataset))
  )
  testthat::expect_equal(
    shiny::isolate(dataset$format(show_all = TRUE)),
    shiny::isolate(format(dataset, show_all = TRUE))
  )
})

# print ---
testthat::test_that("print returns a string representation of filters", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dataset$set_filter_state(state = fs)

  testthat::expect_equal(
    utils::capture.output(shiny::isolate(dataset$print())),
    utils::capture.output(shiny::isolate(print(dataset)))
  )
  testthat::expect_equal(
    utils::capture.output(shiny::isolate(dataset$print(show_all = TRUE))),
    utils::capture.output(shiny::isolate(print(dataset, show_all = TRUE)))
  )

})

# get_call ----
testthat::test_that("get_call returns the filter call of the dataset", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
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

# get_filter_overview ----
testthat::test_that("get_filter_overview returns a data frame", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dataset$set_filter_state(state = fs)
  overview <- shiny::isolate(dataset$get_filter_overview())

  testthat::expect_s3_class(overview, "data.frame")
  testthat::expect_equal(
    overview,
    data.frame(
      dataname = "iris",
      obs = 150,
      obs_filtered = 60
    )
  )
})

# ui_add ----
testthat::test_that("ui_add is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = head(iris), dataname = "iris"
  )
  testthat::expect_error(filtered_dataset$ui_add(), regex = "Pure virtual")
})
