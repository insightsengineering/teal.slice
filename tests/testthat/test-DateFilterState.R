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

testthat::test_that("constructor raises warning when selected is not sorted", {
  testthat::expect_warning(
    DateFilterState$new(
      dates, dataname = "data", varname = "variable", selected = dates[c(10, 1)]
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

# set_state ----
testthat::test_that("set_state: selected accepts an array of two Date objects", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_no_error(
    filter_state$set_state(filter_var(selected = dates[c(1, 10)], dataname = "data", varname = "variable"))
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(selected = dates[1], dataname = "data", varname = "variable")),
    "The array of set values must have length two."
  )
})

testthat::test_that("set_state: selected warns when selection is not within the possible range", {
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

testthat::test_that("set_state: selected limits the selected range to lower and upper bound of possible range", {
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


# get_call ----
testthat::test_that("get_call returns a condition true for the object passed in the constructor", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "dates")
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns call with limits imposed by constructor and selection", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_equal(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.Date("2000-01-01") & variable <= as.Date("2000-01-10"))
  )
  filter_state <- DateFilterState$new(
    dates, dataname = "data", varname = "variable", selected = dates[3:4]
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.Date("2000-01-03") & variable <= as.Date("2000-01-04"))
  )
})

testthat::test_that(
  "DateFilterState$is_any_filtered works properly when NA is present in data",
  code = {
    filter_state <- teal.slice:::DateFilterState$new(
      x = c(dates, NA),
      x_reactive = reactive(NULL),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )

    shiny::isolate(filter_state$set_state(filter_var(
      varname = "x", dataname = "data", keep_na = FALSE, selected = c(dates[1], dates[10]))
    ))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_state(filter_var(
      varname = "x", dataname = "data", keep_na = TRUE, selected = c(dates[1], dates[10]))
    ))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_state(filter_var(
      varname = "x", dataname = "data", keep_na = TRUE, selected = c(dates[2], dates[10]))
    ))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_state(filter_var(
      varname = "x", dataname = "data", keep_na = TRUE, selected = c(dates[1], dates[9]))
    ))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

# format ----
testthat::test_that("format returns a properly formatted string representation", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_identical(
    shiny::isolate(filter_state$format(indent = 0)),
    paste(
      "Filtering on: variable",
      "  Selected range: 2000-01-01 - 2000-01-10",
      "  Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("format accepts numeric as indent", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0L)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_state$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format asserts that indent is numeric", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_error(
    filter_state$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("format prepends spaces to every line of the returned string", {
  filter_state <- DateFilterState$new(dates, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(selected = range(dates), dataname = "data", varname = "variable"))
  for (i in 1:3) {
    whitespace_indent <- paste0(rep(" ", i), collapse = "")
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = !!(i))),
      sprintf(
        "%sFiltering on: variable\n%1$s  Selected range: 2000-01-01 - 2000-01-10\n%1$s  Include missing values: FALSE",
        format("", width = i)
      )
    )
  }
})

# is_any_filtered
testthat::test_that("is_any_filtered returns TRUE when enabled and FLASE when disabled", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = DateFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  date_seq <- seq(Sys.Date() - 2, Sys.Date(), 1)
  fs <- testfs$new(date_seq, dataname = "data", varname = "x")
  fs$set_state(filter_var(dataname = "data", varname = "x", selected = date_seq[1:2], keep_na = TRUE))
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$disable())
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$enable())
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
})

testthat::test_that("is_any_filtered reacts to choices", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = DateFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  date_seq <- seq(Sys.Date() - 4, Sys.Date(), 1)
  fs <- testfs$new(date_seq, dataname = "data", varname = "x", choices = date_seq[c(1, 2)])
  testthat::expect_true(isolate(fs$is_any_filtered()))
  fs <- testfs$new(date_seq, dataname = "data", varname = "x", choices = date_seq[c(1, 5)])
  testthat::expect_false(isolate(fs$is_any_filtered()))
})

