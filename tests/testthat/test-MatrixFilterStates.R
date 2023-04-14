# initialize ----
testthat::test_that("constructor accepts a matrix", {
  testthat::expect_no_error(
    MatrixFilterStates$new(data = as.matrix(faithful), dataname = "test")
  )
  testthat::expect_error(
    MatrixFilterStates$new(data = faithful, dataname = "test"),
    "Assertion on 'data' failed"
  )
})

testthat::test_that("constructor initializes state_list of length 1", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = MatrixFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = matrix(0), dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1, NULL)))
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_get(2, NULL)),
    "Filter state list 2 has not been initialized"
  )
})

# get_fun ----
testthat::test_that("get_fun returns dplyr::filter", {
  filter_states <- MatrixFilterStates$new(data = matrix(0), dataname = "test")
  testthat::expect_equal(filter_states$get_fun(), "subset")
})

# set_filter_state ----
testthat::test_that("set_filter_state only accepts `teal_slices`", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3))
  )
  testthat::expect_error(filter_states$set_filter_state(fs[[1]]), "Assertion on 'state' failed")
  testthat::expect_no_error(filter_states$set_filter_state(fs))
})

testthat::test_that("set_filter_state adds states to state_list", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = MatrixFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- test_class$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3))
  )
  state_list <- shiny::isolate(filter_states$state_list_get(1, NULL))

  testthat::expect_length(state_list, 0)

  filter_states$set_filter_state(fs)
  state_list <- shiny::isolate(filter_states$state_list_get(1, NULL))

  testthat::expect_length(state_list, 1)
})

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` identical to that used to set state (choices excluded)", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(2, 8)),
    filter_var(dataname = "test", varname = "b", selected = c(14, 16))
  )
  filter_states$set_filter_state(fs)

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
})

# set_filter_state ctd. ----
testthat::test_that("set_filter_state updates existing filter states", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  filter_states$set_filter_state(
    state = filter_settings(
      filter_var(dataname = "test", varname = "a", selected = c(2, 8)),
      filter_var(dataname = "test", varname = "b", selected = c(14, 16))
    )
  )
  filter_states$set_filter_state(
    state = filter_settings(
      filter_var(dataname = "test", varname = "b", selected = c(16, 18)),
      filter_var(dataname = "test", varname = "c", selected = c(28, 30))
    )
  )

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    filter_settings(
      filter_var(dataname = "test", varname = "a", selected = c(2, 8)),
      filter_var(dataname = "test", varname = "b", selected = c(16, 18)),
      filter_var(dataname = "test", varname = "c", selected = c(28, 30))
    )
  )
})

# remove_filter_state ----
testthat::test_that("remove_filter_state removes filters from state_list", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
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

testthat::test_that("remove_filter_state raises warning when state_id is not in state_list", {
  teal.logger::suppress_logs()
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
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

# format ----
testthat::test_that("format is a method of MatrixFilterStates that accepts numeric indent argument", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  testthat::expect_no_error(shiny::isolate(filter_states$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_states$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format concatenates its FilterState elements using \\n without additional indent", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = MatrixFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- test_class$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(2, 3)),
    filter_var(dataname = "test", varname = "b", selected = c(4, 6))
  )
  filter_states$set_filter_state(state = fs)

  for (i in 1:3) {
    states_formatted <- shiny::isolate(
      lapply(filter_states$state_list_get(1, NULL), function(x) x$format(indent = i))
    )

    testthat::expect_identical(
      shiny::isolate(filter_states$format(indent = i)),
      paste(states_formatted, collapse = "\n")
    )
  }
})

# get_call ----
testthat::test_that("get_call returns executable call filtering a matrix with numeric values", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
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
  testthat::expect_true(all(test[1:3, "a"] >= 1 & test[1:3, "a"] <= 3))
})

# get_filter_count ----
testthat::test_that("get_filter_count returns the number of active filter states - MartixFilterStates", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")

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

# ui_add ----
testthat::test_that("ui_add returns a message inside a div when data has no rows or no columns", {
  test <- matrix(nrow = 0, ncol = 0)
  filter_state <- MatrixFilterStates$new(data = test, dataname = "test")
  testthat::expect_identical(
    filter_state$ui_add("id"),
    div("no sample variables available")
  )
})
