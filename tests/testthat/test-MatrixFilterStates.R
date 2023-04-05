testthat::test_that("The constructor does not raise errors", {
  testthat::expect_no_error(
    MatrixFilterStates$new(data = matrix(0), dataname = "test")
  )
})

testthat::test_that("The constructor initializes one state_list", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = MatrixFilterStates,
    public = list(
      state_list_get = function(x) private$state_list_get(x)
    )
  )
  filter_states <- testfs$new(data = matrix(0), dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1)))
})

testthat::test_that("get_call returns a executable call filtering a matrix with numeric values", {
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3))
  )
  filter_states$set_filter_state(fs)
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call()),
    quote(test <- subset(test, subset = test[, "a"] >= 1 & test[, "a"] <= 3))
  )
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_equal(test, test[1:3, 1, drop = FALSE])
})

testthat::test_that("set_filter_state adds filters to state_list", {
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3), keep_na = FALSE, keep_inf = FALSE)
  )
  filter_states$set_filter_state(state = fs)

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
})

testthat::test_that("set_filter_state only accepts `teal_slices`", {
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- list(c(1, 2))

  testthat::expect_error(
    filter_states$set_filter_state(state = fs),
    "Assertion on 'state' failed",
  )
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3))
  )
  testthat::expect_no_error(filter_states$set_filter_state(fs))
})

testthat::test_that("remove_filter_state removes filters from state_list", {
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  test_compare <- test
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3))
  )
  filter_states$set_filter_state(state = fs)
  shiny::isolate(filter_states$remove_filter_state(
    filter_settings(
      filter_var(dataname = "test", varname = "a")
    )
  ))
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 0)
})

testthat::test_that("remove_filter_state throws warning when state_id is not in state_list", {
  teal.logger::suppress_logs()
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  test_compare <- test
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3))
  )
  filter_states$set_filter_state(state = fs)
  testthat::expect_warning(filter_states$remove_filter_state(
    filter_settings(
      filter_var(dataname = "test", varname = "B")
    )
  ))
})

testthat::test_that(
  "MatrixFilterStates$ui_add returns a message inside a div when data has no rows or no columns",
  code = {
    mfs <- MatrixFilterStates$new(data = as.matrix(data.frame()), dataname = "iris")
    testthat::expect_identical(
      mfs$ui_add("id"),
      div("no sample variables available")
    )
  }
)

testthat::test_that("get_filter_count returns the number of active filter states - MartixFilterStates", {
  data <- matrix(c(1, 2), ncol = 2, dimnames = list(NULL, c("a", "b")))
  filter_states <- MatrixFilterStates$new(data = data, dataname = "test")

  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "test", varname = "a"),
      filter_var(dataname = "test", varname = "b")
    )
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  filter_states$remove_filter_state(
    filter_settings(
      filter_var(dataname = "test", varname = "a")
    )
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
