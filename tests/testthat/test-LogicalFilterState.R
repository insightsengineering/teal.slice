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
  testthat::expect_no_error(filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE)))
  testthat::expect_no_error(filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "TRUE")))
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
testthat::test_that("get_call returns FALSE values from data passed to selector", {
  variable <- logs
  filter_state <- LogicalFilterState$new(logs, dataname = "data", varname = "variable", selected = FALSE, keep_na = FALSE)

  expect_identical(
    eval(shiny::isolate(filter_state$get_call())),
    !logs
  )
})

testthat::test_that("get_call returns appropriate call depending on selection state", {
  filter_state <- LogicalFilterState$new(logs, dataname = "data", varname = "variable", selected = FALSE, keep_na = FALSE)

  expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!variable)
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE))
  expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable)
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | variable)
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


# is_any_filtered ----
testthat::test_that("is_any_filtered works properly when both TRUE and FALSE are present", {
  filter_state <- LogicalFilterState$new(x = logs, dataname = "data", varname = "variable")

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = FALSE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE, keep_na = TRUE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE, keep_na = FALSE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = FALSE, keep_na = TRUE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = FALSE, keep_na = FALSE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )


  filter_state <- LogicalFilterState$new(x = logs[!logs], dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = TRUE, keep_na = TRUE))
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = FALSE, keep_na = TRUE))
  testthat::expect_false(
    shiny::isolate(filter_state$is_any_filtered())
  )
})

testthat::test_that("is_any_filtered returns TRUE when enabled and FALSE when disabled", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = LogicalFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(logs, dataname = "data", varname = "variable", selected = TRUE, keep_na = TRUE)
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$disable())
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$enable())
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
})
