logs <- as.logical(c(1, 0, 0, 0, 1, 1, 0, 1, 0, 1, NA))

# initialize ----
testthat::test_that("constructor accepts logical values", {
  testthat::expect_no_error(
    LogicalFilterState$new(logs, slice = filter_var(dataname = "data", varname = "variable"))
  )
  testthat::expect_error(
    LogicalFilterState$new(0:1, slice = filter_var(dataname = "data", varname = "variable")),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor raises error when selection is not logical", {
  testthat::expect_error(
    LogicalFilterState$new(logs, slice = filter_var(dataname = "data", varname = "variable", selected = "TRUE")),
    "Must be of type 'logical'"
  )
})

testthat::test_that("constructor forces single selected when multiple is FALSE", {
  testthat::expect_no_error(
    state <- LogicalFilterState$new(
      x = logs,
      slice = filter_var(dataname = "data", varname = "var", selected = c(TRUE, FALSE), multiple = FALSE)
    )
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()$selected),
    TRUE
  )
})

# set_state ----
testthat::test_that("set_state: selected accepts a logical (or coercible) of length <=2", {
  filter_state <- LogicalFilterState$new(logs, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE))
  )
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "TRUE"))
  )
  testthat::expect_no_error(filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = 1)))
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(TRUE, TRUE))),
    "should be a logical vector of length <= 2"
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "a")),
    "The array of set values must contain values coercible to logical"
  )
})

testthat::test_that("set_state: multiple parameters accepting boolean and null values", {
  testthat::expect_no_warning(
    filter_state <- LogicalFilterState$new(
      x = logs,
      slice = filter_var(dataname = "data", varname = "variable", multiple = FALSE)
    )
  )

  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE))
  )
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = NULL))
  )
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(TRUE, TRUE)))
  )
})

# get_call ----
testthat::test_that("LogicalFilterState$get_call returns variable name when !multiple", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = filter_var(dataname = "data", varname = "variable", multiple = FALSE)
  )
  expect_identical(shiny::isolate(filter_state$get_call()), quote(variable))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = filter_var(dataname = "data", varname = "variable", choices = c(TRUE, FALSE), selected = FALSE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!variable)
  )
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = filter_var(dataname = "data", varname = "variable", choices = FALSE)
  )

  # todo: what should this really return?
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable %in% c(TRUE, FALSE))
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = filter_var(dataname = "data", varname = "variable", selected = FALSE),
    extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(!dataname$variable)
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = filter_var(dataname = "data", varname = "variable", selected = FALSE),
    extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(!dataname[, "variable"])
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = filter_var(dataname = "data", varname = "variable", selected = FALSE, keep_na = TRUE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | !variable)
  )
})

# format ----
testthat::test_that("format accepts logical show_all", {
  filter_state <- LogicalFilterState$new(logs, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = TRUE)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = FALSE)))
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = 1)),
    "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = 0)),
    "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = "TRUE")),
    "Assertion on 'show_all' failed"
  )
})

testthat::test_that("format returns a properly formatted string representation", {
  filter_state <- LogicalFilterState$new(logs, slice = filter_var(dataname = "data", varname = "variable"))
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "LogicalFilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "LogicalFilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  filter_state <- LogicalFilterState$new(logs, slice = filter_var(dataname = "data", varname = "variable"))
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print())),
    c(
      "LogicalFilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()))),
      " "
    )
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c(
      "LogicalFilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)),
      " "
    )
  )
})
