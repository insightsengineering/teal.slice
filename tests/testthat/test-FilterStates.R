
# initialize ----
testthat::test_that("constructor accepts only a string as dataname", {
  testthat::expect_no_error(FilterStates$new(data = NULL, dataname = "string"))
  testthat::expect_error(FilterStates$new(data = NULL, dataname = quote(name)), "Assertion on 'dataname' failed")
  testthat::expect_error(FilterStates$new(data = NULL, dataname = call("call")), "Assertion on 'dataname' failed")
})

# get_fun ----
testthat::test_that("get_fun returns subset after initialization", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  testthat::expect_equal(filter_states$get_fun(), "subset")
})

# get_call ----
testthat::test_that("get_call returns NULL after initialization if no filter applied", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  testthat::expect_null(filter_states$get_call())
})
# call construction will be tested in child classes
# due to differences in subsetting functions and data-/variable name prefixing

# validate_state_list_exists ----
testthat::test_that("validate_state_list_exists raises errors if no filters were added", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = FilterStates,
    public = list(
      validate_state_list_exists = function(x) private$validate_state_list_exists(x)
    )
  )
  filter_states <- testfs$new(data = NULL, dataname = "test")

  testthat::expect_error(
    shiny::isolate(filter_states$validate_state_list_exists(1)),
    regexp = "Filter state list 1 has not been initialized in FilterStates object belonging to the dataset"
  )
})

# clear_filter_states ----
testthat::test_that("clearing empty FilterStates does not raise errors", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  testthat::expect_no_error(filter_states$clear_filter_states())
})

# data_choices_labeled ----
testthat::test_that("data_choices_labeled returns an empty character array if choices are an empty array", {
  testthat::expect_identical(data_choices_labeled(7, choices = c()), character(0))
})

testthat::test_that("data_choices_labeled returns a choices_labeled object if choices are not empty", {
  testthat::expect_s3_class(data_choices_labeled(data.frame(a = 1), choices = c("a")), "choices_labeled")
})

testthat::test_that("data_choices_labeled returns names of the elements matching the choices", {
  testthat::expect_identical(data_choices_labeled(data.frame(a = 1, b = 2), choices = c("a"))[1], c("a: a" = "a"))
})

testthat::test_that("data_choices_labeled returns labels of the elements matching the choices
  if the varlabels are provided for the elements", {
  result <- unname(data_choices_labeled(list(a = 1, b = 2), choices = c("a"), varlabels = c(a = "labelA"))[1])
  testthat::expect_equal(result, "a")
})
