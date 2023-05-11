logs <- as.logical(c(1, 0, 0, 0, 1, 1, 0, 1, 0, 1, NA))

# initialize ----
testthat::test_that("constructor accepts logical values", {
  testthat::expect_no_error(
    LogicalFilterState$new(logs, dataname = "data", varname = "variable")
  )
  testthat::expect_error(
    LogicalFilterState$new(0:1, dataname = "data", varname = "variable"), "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor raises error when selection is not logical", {
  testthat::expect_error(
    LogicalFilterState$new(logs, dataname = "data", varname = "variable", selected = "TRUE"),
    "Assertion on 'selected' failed"
  )
})



# set_state ----
testthat::test_that("set_state: selected accepts a logical (or coercible) of length 1", {
  filter_state <- LogicalFilterState$new(logs, dataname = "data", varname = "variable")
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE))
  )
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "TRUE"))
  )
  testthat::expect_no_error(filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = 1)))
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(TRUE, TRUE))),
    "should be a logical scalar"
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "a")),
    "The array of set values must contain values coercible to logical"
  )
})

# get_call ----
testthat::test_that("get_call of default LogicalFilterState object returns variable name", {
  filter_state <- LogicalFilterState$new(logs[1:10], dataname = "data", varname = "variable")
  expect_identical(shiny::isolate(filter_state$get_call()), quote(variable))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    dataname = "data", varname = "variable",
    choices = c(TRUE, FALSE), selected = FALSE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!variable)
  )
})

testthat::test_that("get_call returns NULL if disabled", {
  filter_state <- LogicalFilterState$new(
    logs,
    dataname = "data", varname = "variable", selected = FALSE, disabled = TRUE
  )
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    dataname = "data", varname = "variable", choices = FALSE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable)
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    dataname = "data", varname = "variable", selected = FALSE, extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(!dataname$variable)
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    dataname = "data", varname = "variable", selected = FALSE, extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(!dataname[, "variable"])
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- LogicalFilterState$new(
    logs,
    dataname = "data", varname = "variable", selected = FALSE, keep_na = TRUE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | !variable)
  )
})

# format ----
testthat::test_that("format accepts numeric as indent", {
  filter_state <- LogicalFilterState$new(logs, dataname = "data", varname = "variable")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_state$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format returns a string representation the FilterState object", {
  filter_state <- LogicalFilterState$new(logs, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste(
      "  Filtering on: variable",
      "    Selected values: TRUE",
      "    Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("format prepends spaces to every line of the returned string", {
  filter_state <- LogicalFilterState$new(logs, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  for (i in 0:3) {
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = i)),
      paste(format("", width = i),
        c(
          "Filtering on: variable",
          sprintf("%sSelected values: TRUE", format("", width = i)),
          sprintf("%sInclude missing values: FALSE", format("", width = i))
        ),
        sep = "", collapse = "\n"
      )
    )
  }
})
