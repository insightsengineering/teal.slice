testthat::test_that("The constructor does not raise errors", {
  testthat::expect_no_error(
    MatrixFilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  )
})

testthat::test_that("The constructor initializes one state_list", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_null(isolate(filter_states$state_list_get(1)))
})

testthat::test_that("get_call returns a call filtering a matrix with numeric values", {
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))

  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  filter_state <- RangeFilterState$new(
    x = test,
    varname = "a",
    input_dataname = as.name("test"),
    extract_type = "matrix"
  )
  isolate(filter_state$set_selected(c(1, 3)))

  isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:3, 1, drop = FALSE])
})

testthat::test_that("set_filter_state adds filters to state_list", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(a = c(1, 2))
  isolate(filter_states$set_filter_state(state = fs, data = test))

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:2, 1, drop = FALSE])
})

testthat::test_that("set_filter_state throws error when list is unnamed", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(c(1, 2))

  testthat::expect_error(
    filter_states$set_filter_state(state = fs, data = test),
    "Assertion on 'checkmate::test_null(names(state))' failed: FALSE.",
    fixed = TRUE
  )
})

testthat::test_that("remove_filter_state removes filters from state_list", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(a = c(1, 2))
  isolate(filter_states$set_filter_state(state = fs, data = test))

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:2, 1, drop = FALSE])

  isolate(filter_states$remove_filter_state("a"))
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test)
})

testthat::test_that("remove_filter_state throws warning when state_id is not in state_list", {
  teal.logger::suppress_logs()
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(a = c(1, 2))
  isolate(filter_states$set_filter_state(state = fs, data = test))

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:2, 1, drop = FALSE])

  isolate(testthat::expect_warning(filter_states$remove_filter_state("B")))
})

testthat::test_that(
  "MatrixFilterStates$ui_add_filter_state returns a message inside a div when data has no rows or no columns",
  code = {
    mfs <- MatrixFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = "test"
    )
    testthat::expect_identical(
      mfs$ui_add_filter_state("id", as.matrix(data.frame())),
      div("no sample variables available")
    )
    testthat::expect_identical(
      mfs$ui_add_filter_state("id", as.matrix(data.frame(A = numeric(0)))),
      div("no samples available")
    )
  }
)
