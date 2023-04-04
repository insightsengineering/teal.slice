testthat::test_that("The constructor accepts a data.frame object with a dataname", {
  testthat::expect_no_error(DefaultFilteredDataset$new(dataset = head(iris), dataname = "iris"))
})

testthat::test_that("filter_states list is initialized with single FilterStates element named as filter", {
  testfd <- R6::R6Class(
    "testfd",
    inherit = DefaultFilteredDataset,
    public = list(
      get_filter_states = function() private$filter_states
    )
  )
  filtered_dataset <- testfd$new(dataset = iris, dataname = "mae")
  testthat::expect_identical(names(filtered_dataset$get_filter_states()), "filter")
})

testthat::test_that("get_call returns a list of calls or NULL", {
  filtered_dataset <- DefaultFilteredDataset$new(
    dataset = iris, dataname = "iris"
  )
  testthat::expect_null(shiny::isolate(filtered_dataset$get_call()))
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  filtered_dataset$set_filter_state(state = fs)

  checkmate::expect_list(shiny::isolate(filtered_dataset$get_call()), types = "<-")
})

testthat::test_that(
  "DefaultFilteredDataset$set_filter_state sets filters specified by list names",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")

    fs <- filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dataset$set_filter_state(state = fs)
    testthat::expect_equal(
      shiny::isolate(dataset$get_call()),
      list(
        filter = quote(
          iris <- dplyr::filter(
            iris,
            Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
              Species %in% c("setosa", "versicolor")
          )
        )
      )
    )
  }
)

testthat::test_that(
  "DefaultFilteredDataset$set_filter_state throws error when list is not named",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
    fs <- list(
      c(5.1, 6.4)
    )
    testthat::expect_error(dataset$set_filter_state(state = fs))
  }
)

testthat::test_that(
  "DefaultFilteredDataset$remove_filter_state removes desired filter",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
    fs <- filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dataset$set_filter_state(state = fs)
    dataset$remove_filter_state(filter_settings(filter_var(dataname = "iris", varname = "Species")))

    testthat::expect_equal(
      shiny::isolate(dataset$get_call()),
      list(
        filter = quote(
          iris <- dplyr::filter(
            iris,
            Sepal.Length >= 5.1 & Sepal.Length <= 6.4
          )
        )
      )
    )
  }
)

testthat::test_that(
  "DefaultFilteredDataset$get_filter_state returns list identical to input",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
    fs <- filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
      filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dataset$set_filter_state(state = fs)

    current_states <- unname(shiny::isolate(dataset$get_filter_state()))
    current_states <- lapply(current_states, function(x) {
      x <- unclass(x)
      x$choices = NULL
      x
    })
    current_states <- lapply(current_states, as.teal_slice)
    current_states <- do.call(filter_settings, current_states)

    testthat::expect_identical(current_states, fs)
  }
)

testthat::test_that(
  "DefaultFilteredDataset$remove_filter_state removes more than one filter",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset = iris, dataname = "iris")
    fs <- filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dataset$set_filter_state(state = fs)
    dataset$remove_filter_state(
      filter_settings(
        filter_var(dataname = "iris", varname = "Sepal.Length"),
        filter_var(dataname = "iris", varname = "Species")
      )
    )

    testthat::expect_null(
      shiny::isolate(dataset$get_call())
    )
  }
)

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
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 5.1), keep_na = FALSE, keep_inf = FALSE)
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
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 5.1), keep_na = FALSE, keep_inf = FALSE)
  )
  dataset_iris$set_filter_state(fs)
  testthat::expect_equal(
    shiny::isolate(dataset_iris$get_filter_overview()),
    data.frame(dataname = "iris", obs = 6, obs_filtered = 1, subjects = 1, subjects_filtered = 1)
  )
})
