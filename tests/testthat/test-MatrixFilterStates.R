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

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` with default include_varnames", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    include_varnames = list(test = colnames(test)),
    count_type = "all"
  )

  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    fs
  )
})

# get_call ----
testthat::test_that("get_call returns executable call filtering a matrix with numeric values", {
  test <- matrix(1:100, ncol = 10, dimnames = list(NULL, letters[1:10]))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "a", selected = c(1, 3), keep_na = FALSE, keep_inf = FALSE)
  )
  filter_states$set_filter_state(fs)
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call()),
    quote(test <- subset(test, test[, "a"] >= 1 & test[, "a"] <= 3))
  )
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_true(all(test[1:3, "a"] >= 1 & test[1:3, "a"] <= 3))
})

