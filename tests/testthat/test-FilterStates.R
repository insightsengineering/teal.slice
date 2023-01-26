# Notice, the particular design of these tests. We don't test the particulars of
# the calls, but only whether they evaluate to the expected value.
testthat::test_that("The constructor accepts a call, name or string as input_dataname", {
  testthat::expect_error(
    FilterStates$new(input_dataname = "string", output_dataname = "test", datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = quote(name), output_dataname = "test", datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = call("call"), output_dataname = "test", datalabel = "test"),
    NA
  )
})

testthat::test_that("The constructor accepts a call, name or string as output_dataname", {
  testthat::expect_error(
    FilterStates$new(input_dataname = "test", output_dataname = "string", datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = "test", output_dataname = quote(name), datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = "test", output_dataname = call("call"), datalabel = "test"),
    NA
  )
})

testthat::test_that("get_call returns NULL after initialization if input_dataname is the same as output_dataname", {
  filter_states <- FilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "label"
  )
  testthat::expect_null(filter_states$get_call())
})

testthat::test_that("get_call returns a call binding the object output_dataname to input_dataname", {
  test_dataset <- 7
  filter_states <- FilterStates$new(
    input_dataname = "test_dataset",
    output_dataname = "output",
    datalabel = "label"
  )
  eval(filter_states$get_call())
  testthat::expect_equal(output, test_dataset)
})

testthat::test_that("get_fun returns subset after initialization", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_equal(filter_states$get_fun(), "subset")
})

testthat::test_that("Emptying empty FilterStates does not throw", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$state_list_empty(), NA)
})

testthat::test_that("state_list_get throws on a freshly initialized FilterStates object", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$state_list_get(state_list_index = 1), "Filter state list 1 .* test")
})

testthat::test_that("state_list_initialize throws an error when passed an empty list", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(
    filter_states$state_list_initialize(list()),
    msg = "Assertion on 'x'"
  )
})

testthat::test_that("state_list_push throws before calling state_list_initialize", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_state <- FilterState$new("test", varname = "test")
  testthat::expect_error(filter_states$state_list_push(x = filter_state, state_list_index = 1L, state_id = "test"))
})

testthat::test_that("state_list_remove does not throw before initializing the state list", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  testthat::expect_error(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
})

testthat::test_that("data_choices_labeled returns an empty character array if choices are an empty array", {
  testthat::expect_equal(data_choices_labeled(7, choices = c()), character(0))
})

testthat::test_that("data_choices_labeled returns a choices_labeled object if choices are not empty", {
  testthat::expect_true(is(data_choices_labeled(data.frame(a = 1), choices = c("a")), "choices_labeled"))
})

testthat::test_that("data_choices_labeled returns names of the elements matching the choices", {
  testthat::expect_equal(data_choices_labeled(data.frame(a = 1, b = 2), choices = c("a"))[1], c("a: a" = "a"))
})

testthat::test_that("data_choices_labeled returns labels of the elements matching the choices
  if the varlabels are provided for the elements", {
  result <- unname(data_choices_labeled(list(a = 1, b = 2), choices = c("a"), varlabels = c(a = "labelA"))[1])
  testthat::expect_equal(result, "a")
})
