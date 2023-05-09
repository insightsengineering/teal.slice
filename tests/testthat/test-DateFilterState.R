dates <- as.Date("2000-01-01") + 0:9

# initialize ----
testthat::test_that("constructor accepts a Date object", {
  testthat::expect_no_error(
    DateFilterState$new(dates, dataname = "data", varname = "variable")
  )
  testthat::expect_error(
    DateFilterState$new(as.POSIXct(dates), dataname = "data", varname = "variable"),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor raises warning when selected out of range", {
  testthat::expect_warning(
    DateFilterState$new(
      dates,
      dataname = "data", varname = "variable", selected = range(dates) + c(-1, 1)
    ),
    regexp = "outside of the possible range"
  )
})

testthat::test_that("constructor raises warning when selected is not sorted", {
  testthat::expect_warning(
    DateFilterState$new(
      dates,
      dataname = "data", varname = "variable", selected = dates[c(10, 1)]
    ),
    regexp = "Start date 2000-01-10 is set after"
  )
})

testthat::test_that("constructor raises error when selection is not Date", {
  testthat::expect_error(
    DateFilterState$new(dates, dataname = "data", varname = "variable", selected = c("a", "b")),
    "The array of set values must contain values coercible to Date."
  )
})

testthat::test_that("constructor raises warning when chioces is not sorted", {
  testthat::expect_warning(
    DateFilterState$new(
      dates,
      dataname = "data", varname = "variable", choices = dates[c(10, 1)]
    ),
    regexp = "Invalid choices"
  )
})

testthat::test_that("constructor raises warning when chioces out of range", {
  testthat::expect_warning(
    DateFilterState$new(
      dates,
      dataname = "data", varname = "variable", choices = range(dates) + c(-1, 1)
    ),
    regexp = "Choices adjusted"
  )
})

testthat::test_that("constructor raises error when selection is not Date", {
  testthat::expect_error(
    DateFilterState$new(dates, dataname = "data", varname = "variable", choices = c("a", "b")),
    "Assertion on 'choices' failed"
  )
})

testthat::test_that("constructor sets default state", {
  fs <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_identical(
    shiny::isolate(fs$get_state()),
    filter_var(
      dataname = "data",
      varname = "variable",
      choices = dates[c(1, 10)],
      selected = dates[c(1, 10)]
    )
  )
})


# set_state ----
testthat::test_that("set_state: selected accepts vector of two coercible to Date elements", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_no_error(
    filter_state$set_state(filter_var(selected = dates[c(1, 10)], dataname = "data", varname = "variable"))
  )
    testthat::expect_no_error(
    filter_state$set_state(filter_var(selected = as.integer(dates[c(1, 10)]), dataname = "data", varname = "variable"))
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(selected = dates[1], dataname = "data", varname = "variable")),
    "The array of set values must have length two"
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(selected = c("a", "b"), dataname = "data", varname = "variable")),
    "The array of set values must contain values coercible to Date"
  )
})

testthat::test_that("set_state: selected raises warning when selection is not within the possible range", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")

  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dates, dataname = "data", varname = "variable", selected = c(dates[1] - 1, dates[10]))
    ),
    "outside of the possible range"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dates, dataname = "data", varname = "variable", selected = c(dates[1], dates[10] + 1))
    ),
    "outside of the possible range"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dates, dataname = "data", varname = "variable", selected = c(dates[1] - 1, dates[10] + 1))
    ),
    "outside of the possible range"
  )
})

testthat::test_that("set_state: selected range is limited to lower and upper bound of possible range", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  suppressWarnings(filter_state$set_state(
    filter_var(dataname = "data", varname = "variable", selected = c(dates[1] - 1, dates[10]))
  ))
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(dates[1], dates[10]))
  suppressWarnings(filter_state$set_state(
    filter_var(dataname = "data", varname = "variable", selected = c(dates[1], dates[10] + 1))
  ))
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(dates[1], dates[10]))
  suppressWarnings(filter_state$set_state(
    filter_var(dataname = "data", varname = "variable", selected = c(dates[1] - 1, dates[10] + 1))
  ))
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(dates[1], dates[10]))
})

testthat::test_that("set_state: selected raises error when selection is not a Date or coercible", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("a", "b"))),
    "The array of set values must contain values coercible to Date"
  )
})


# get_call ----
testthat::test_that("method get_call of default DateFilterState object returns NULL", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "dates")
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable", selected = dates[c(1, 3)])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.Date("2000-01-01") & variable <= as.Date("2000-01-03"))
  )
})

testthat::test_that("get_call returns NULL if disabled", {
  filter_state <- DateFilterState$new(
    dates, dataname = "data", varname = "variable", selected = dates[c(1, 3)], disabled = TRUE
  )
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- DateFilterState$new(
    dates, dataname = "data", varname = "variable",
    choices = dates[c(1, 3)], selected = dates[c(1, 3)]
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.Date("2000-01-01") & variable <= as.Date("2000-01-03"))
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- DateFilterState$new(
    dates, dataname = "data", varname = "variable", selected = dates[c(1, 3)], extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname$variable >= as.Date("2000-01-01") & dataname$variable <= as.Date("2000-01-03"))
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- DateFilterState$new(
    dates, dataname = "data", varname = "variable", selected = dates[c(1, 3)], extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname[, "variable"] >= as.Date("2000-01-01") & dataname[, "variable"] <= as.Date("2000-01-03"))
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- DateFilterState$new(
    c(dates, NA), dataname = "data", varname = "variable", selected = dates[c(1, 3)], keep_na = TRUE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | variable >= as.Date("2000-01-01") & variable <= as.Date("2000-01-03"))
  )
})

testthat::test_that("get_call returns call if all selected but NA exists", {
  filter_state <- DateFilterState$new(
    c(dates, NA), dataname = "data", varname = "variable", keep_na = FALSE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.Date("2000-01-01") & variable <= as.Date("2000-01-10"))
  )
})

# format ----
testthat::test_that("format accepts numeric as indent", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0L)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_state$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format returns a properly formatted string representation", {
  testthat::skip("temporary")
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_identical(
    shiny::isolate(filter_state$format()),
    paste(
      "  Filtering on: variable",
      "    Selected range: 2000-01-01 - 2000-01-10",
      "    Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("format prepends spaces to every line of the returned string", {
  testthat::skip("temporary")
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(selected = range(dates), dataname = "data", varname = "variable"))
  for (i in 0:3) {
    whitespace_indent <- paste0(rep(" ", i), collapse = "")
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = i)),
      paste(format("", width = i),
        c(
          "Filtering on: variable",
          sprintf("%sSelected range: 2000-01-01 - 2000-01-10", format("", width = i)),
          sprintf("%sInclude missing values: TRUE", format("", width = i))
        ),
        sep = "", collapse = "\n"
      )
    )
  }
})


# is_any_filtered ----
