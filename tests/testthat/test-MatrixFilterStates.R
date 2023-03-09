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
  filter_states$set_filter_state(list(a = c(1, 3)))
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
  fs <- list(a = list(selected = c(1, 2), keep_na = FALSE, keep_inf = FALSE))
  filter_states$set_filter_state(state = fs)

  testthat::expect_identical(shiny::isolate(filter_states$get_filter_state()), fs)
})

testthat::test_that("set_filter_state throws error when list is unnamed", {
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  fs <- list(c(1, 2))

  testthat::expect_error(
    filter_states$set_filter_state(state = fs),
    "Assertion on 'state' failed: Must have names.",
    fixed = TRUE
  )
})

testthat::test_that("remove_filter_state removes filters from state_list", {
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  test_compare <- test
  fs <- list(a = c(1, 2))
  filter_states$set_filter_state(state = fs)
  isolate(filter_states$remove_filter_state("a"))
  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    list(a = NULL)[0]
  )
})

testthat::test_that("remove_filter_state throws warning when state_id is not in state_list", {
  teal.logger::suppress_logs()
  test <- matrix(c(1, 2, 3, 4, 5), nrow = 5, ncol = 1, dimnames = list(c(), c("a")))
  filter_states <- MatrixFilterStates$new(data = test, dataname = "test")
  test_compare <- test
  fs <- list(a = c(1, 2))
  filter_states$set_filter_state(state = fs)
  testthat::expect_warning(filter_states$remove_filter_state("B"))
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
    list(a = list(), b = list())
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  filter_states$remove_filter_state(state_id = "a")
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
