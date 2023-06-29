# initialize ----
testthat::test_that("constructor accepts data.frame object with a dataname", {
  testthat::expect_no_error(DefaultFilteredDataset$new(dataset = head(iris), dataname = "iris"))
  testthat::expect_error(DefaultFilteredDataset$new(dataset = head(iris)), "argument .+ missing, with no default")
  testthat::expect_error(DefaultFilteredDataset$new(dataname = "iris"), "argument .+ missing, with no default")
  testthat::expect_error(DefaultFilteredDataset$new(dataset = as.list(iris)), "Assertion on 'dataset' failed")
  testthat::expect_error(DefaultFilteredDataset$new(dataset = iris, dataname = iris), "Assertion on 'name' failed")
})

testthat::test_that("filter_states list is initialized with single `FilterStates` element named filter", {
  testfd <- R6::R6Class(
    "testfd",
    inherit = DefaultFilteredDataset,
    public = list(
      get_filter_states = function() private$filter_states
    )
  )
  filtered_dataset <- testfd$new(dataset = iris, dataname = "iris")
  testthat::expect_identical(names(filtered_dataset$get_filter_states()), "filter")
})


# set_filter_state ----
testthat::test_that("set_filter_state accepts `teal_slices`", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")

  fs <- filter_settings(
    filter_conf(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_conf(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  testthat::expect_no_error(dataset$set_filter_state(state = fs))
  testthat::expect_error(dataset$set_filter_state(state = list(fs[[1]], fs[[2]])), "Assertion on 'state' failed")
})


# get_filter_state ----
testthat::test_that("set_filter_state sets `teal_slice`", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_conf(
      dataname = "iris", varname = "Sepal.Length", choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE, fixed = FALSE, locked = FALSE
    ),
    filter_conf(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), multiple = TRUE, selected = c("setosa", "versicolor"),
      keep_na = FALSE, keep_inf = FALSE, fixed = FALSE, locked = FALSE
    ),
    count_type = "all",
    include_varnames = list(iris = colnames(iris))
  )
  dataset$set_filter_state(fs)
  expect_identical_slices(dataset$get_filter_state(), fs)
})


# format ---
testthat::test_that("format returns a properly formatted string representation", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_conf(
      dataname = "iris", varname = "Sepal.Length", choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_inf = FALSE, fixed = FALSE, locked = FALSE
    ),
    filter_conf(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), selected = c("setosa", "versicolor"),
      keep_na = FALSE, fixed = FALSE, locked = FALSE
    ),
    count_type = "all",
    include_varnames = list(iris = colnames(iris))
  )
  dataset$set_filter_state(fs)
  testthat::expect_equal(
    shiny::isolate(dataset$format()),
    paste0(
      "DefaultFilteredDataset:\n",
      format(shiny::isolate(dataset$get_filter_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(dataset$format(show_all = TRUE)),
    paste0(
      "DefaultFilteredDataset:\n",
      format(shiny::isolate(dataset$get_filter_state()), show_all = TRUE)
    )
  )
})

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_conf(
      dataname = "iris", varname = "Sepal.Length", choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_inf = FALSE, fixed = FALSE, locked = FALSE
    ),
    filter_conf(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), selected = c("setosa", "versicolor"),
      keep_na = FALSE, fixed = FALSE, locked = FALSE
    ),
    count_type = "all",
    include_varnames = list(iris = colnames(iris))
  )
  dataset$set_filter_state(fs)

  testthat::expect_equal(
    utils::capture.output(cat(dataset$print())),
    c("DefaultFilteredDataset:", utils::capture.output(print(shiny::isolate(dataset$get_filter_state()))))
  )
  testthat::expect_equal(
    utils::capture.output(cat(dataset$print(show_all = TRUE))),
    c(
      "DefaultFilteredDataset:",
      utils::capture.output(print(shiny::isolate(dataset$get_filter_state()), show_all = TRUE))
    )
  )
})

# remove_filter_state ----
testthat::test_that("remove_filter_state removes desired filter", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_conf(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_conf(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dataset$set_filter_state(state = fs)
  dataset$remove_filter_state(filter_settings(filter_conf(dataname = "iris", varname = "Species")))
  testthat::expect_length(shiny::isolate(dataset$get_filter_state()), 1)
})

testthat::test_that("remove_filter_state can remove multiple filters", {
  dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
  fs <- filter_settings(
    filter_conf(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_conf(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dataset$set_filter_state(state = fs)
  dataset$remove_filter_state(
    filter_settings(
      filter_conf(dataname = "iris", varname = "Sepal.Length"),
      filter_conf(dataname = "iris", varname = "Species")
    )
  )

  testthat::expect_length(shiny::isolate(dataset$get_filter_state()), 0)
})


# get_filter_overview ----
testthat::test_that("get_filter_overview returns overview data.frame with obs filter counts", {
  dataset_iris <- DefaultFilteredDataset$new(dataset = head(iris), dataname = "iris")
  testthat::expect_equal(
    shiny::isolate(dataset_iris$get_filter_overview()),
    data.frame(dataname = "iris", obs = 6, obs_filtered = 6)
  )
})

testthat::test_that("get_filter_overview returns overview data.frame with obs filter counts", {
  dataset_iris <- DefaultFilteredDataset$new(dataset = head(iris), dataname = "iris")
  fs <- filter_settings(
    filter_conf(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 5.1), keep_na = FALSE, keep_inf = FALSE)
  )
  dataset_iris$set_filter_state(fs)
  testthat::expect_equal(
    shiny::isolate(dataset_iris$get_filter_overview()),
    data.frame(dataname = "iris", obs = 6, obs_filtered = 1)
  )
})

testthat::test_that("get_filter_overview returns overview data.frame with obs and subject filter counts  ", {
  dataset_iris <- DefaultFilteredDataset$new(dataset = head(iris), dataname = "iris", keys = "Species")
  fs <- filter_settings(
    filter_conf(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 5.1), keep_na = FALSE, keep_inf = FALSE)
  )
  dataset_iris$set_filter_state(fs)
  testthat::expect_equal(
    shiny::isolate(dataset_iris$get_filter_overview()),
    data.frame(dataname = "iris", obs = 6, obs_filtered = 1, subjects = 1, subjects_filtered = 1)
  )
})


# get_call ----
testthat::test_that("get_call returns a list of `<-` calls or NULL", {
  filtered_dataset <- DefaultFilteredDataset$new(
    dataset = iris, dataname = "iris"
  )
  testthat::expect_null(shiny::isolate(filtered_dataset$get_call()))
  fs <- filter_settings(
    filter_conf(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_conf(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  filtered_dataset$set_filter_state(state = fs)

  checkmate::expect_list(shiny::isolate(filtered_dataset$get_call()), types = "<-")
})
