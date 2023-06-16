posixct <- as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:9
posixlt <- as.POSIXlt(posixct)

# initialize ----
testthat::test_that("constructor accepts a POSIXct or POSIXlt object", {
  testthat::expect_no_error(
    DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "variable"))
  )
  testthat::expect_no_error(
    DatetimeFilterState$new(posixlt, slice = filter_var(dataname = "data", varname = "variable"))
  )
  testthat::expect_error(
    DatetimeFilterState$new(as.Date(posixct), slice = filter_var(dataname = "data", varname = "variable")),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor raises warning when selected is out of range", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      x = posixct,
      slice = filter_var(dataname = "data", varname = "variable", selected = range(posixct) + c(-1, 1))
    ),
    "outside of the range"
  )
})

testthat::test_that("constructor raises warning when selected is not sorted", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      slice = filter_var(dataname = "data", varname = "variable", selected = posixct[c(10, 1)])
    ),
    "Start date '2000-01-01 12:00:09' is set after"
  )
})

testthat::test_that("constructor raises error when selection is not Datetime or coercible", {
  testthat::expect_error(
    DatetimeFilterState$new(
      posixct,
      slice = filter_var(dataname = "data", varname = "variable", selected = c("a", "b"))
    ),
    "The array of set values must contain values coercible to POSIX"
  )
})

testthat::test_that("constructor raises warning when choices is out of range", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      slice = filter_var(dataname = "data", varname = "variable", choices = range(posixct) + c(-1, 1))
    ),
    "outside of variable range"
  )
})

testthat::test_that("constructor raises warning when choices is not sorted", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      slice = filter_var(dataname = "data", varname = "variable", choices = posixct[c(10, 1)])
    ),
    "Invalid choices"
  )
})

testthat::test_that("constructor raises error when choices is not Date", {
  testthat::expect_error(
    DatetimeFilterState$new(
      posixct,
      slice = filter_var(dataname = "data", varname = "variable", choices = c("a", "b"))
    ),
    "Must inherit from class 'POSIXct'/'POSIXlt'"
  )
})


# set_state ----
testthat::test_that("set_state: selected accepts vector of two POSIXct objects or coercible", {
  filter_state <- DatetimeFilterState$new(
    posixct,
    slice = filter_var(dataname = "data", varname = "variable")
  )

  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[1:2]))
  )
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = as.integer(posixct[1:2])))
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[1])),
    "The array of set values must have length two"
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("a", "b"))),
    "The array of set values must contain values coercible to POSIX"
  )
})

testthat::test_that("set_state: selected raises warning when selection not fully included in range", {
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "posixct"))

  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "posixct", selected = c(posixct[1] - 1, posixct[10]))
    ),
    "outside of the range"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "posixct", selected = c(posixct[1], posixct[10] + 1))
    ),
    "outside of the range"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "posixct", selected = c(posixct[1] - 1, posixct[10] + 1))
    ),
    "outside of the range"
  )
})

testthat::test_that("set_state: selected range is limited to lower and upper bound of possible range", {
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "posixct"))

  suppressWarnings(filter_state$set_state(
    filter_var(dataname = "data", varname = "posixct", selected = c(posixct[1] - 1, posixct[10]))
  ))
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(posixct[1], posixct[10]))

  suppressWarnings(filter_state$set_state(
    filter_var(dataname = "data", varname = "posixct", selected = c(posixct[1], posixct[10] + 1))
  ))
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(posixct[1], posixct[10]))

  suppressWarnings(filter_state$set_state(
    filter_var(dataname = "data", varname = "posixct", selected = c(posixct[1] - 1, posixct[10] + 1))
  ))
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(posixct[1], posixct[10]))
})

testthat::test_that("set_state: selected raises error when selection is not a Date or coercible", {
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("a", "b"))),
    "The array of set values must contain values coercible to POSIX"
  )
})


# get_call ----
testthat::test_that("get_call method of default DatetimeFilterState object returns NULL", {
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- DatetimeFilterState$new(
    posixct,
    slice = filter_var(dataname = "data", varname = "variable", selected = posixct[c(1, 3)])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(
      variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") &
        variable < as.POSIXct("2000-01-01 12:00:03", tz = "GMT")
    )
  )
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- DatetimeFilterState$new(
    posixct,
    slice = filter_var(dataname = "data", varname = "variable", choices = posixct[c(1, 3)], selected = posixct[c(1, 3)])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(
      variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") &
        variable < as.POSIXct("2000-01-01 12:00:03", tz = "GMT")
    )
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- DatetimeFilterState$new(
    posixct,
    slice = filter_var(dataname = "data", varname = "variable", selected = posixct[c(1, 3)]),
    extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(
      dataname$variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") &
        dataname$variable < as.POSIXct("2000-01-01 12:00:03", tz = "GMT")
    )
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- DatetimeFilterState$new(
    posixct,
    slice = filter_var(dataname = "data", varname = "variable", selected = posixct[c(1, 3)]),
    extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(
      dataname[, "variable"] >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") &
        dataname[, "variable"] < as.POSIXct("2000-01-01 12:00:03", tz = "GMT")
    )
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- DatetimeFilterState$new(
    c(posixct, NA),
    slice = filter_var(dataname = "data", varname = "variable", selected = posixct[c(1, 3)], keep_na = TRUE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(
      is.na(variable) |
        variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") &
          variable < as.POSIXct("2000-01-01 12:00:03", tz = "GMT")
    )
  )
})

testthat::test_that("get_call returns call if all selected but NA exists", {
  posixct_na <- as.POSIXct(c(as.character(posixct), NA), tz = "GMT")
  filter_state <- DatetimeFilterState$new(
    posixct_na,
    slice = filter_var(dataname = "data", varname = "variable", keep_na = FALSE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(
      variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") &
        variable < as.POSIXct("2000-01-01 12:00:10", tz = "GMT")
    )
  )
})

# format ----
testthat::test_that("format accepts logical show_all", {
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "variable"))
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
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "DatetimeFilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "DatetimeFilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# print ---

testthat::test_that("print returns a properly formatted string representation", {
  filter_state <- DatetimeFilterState$new(posixct, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print())),
    c("DatetimeFilterState:", utils::capture.output(print(shiny::isolate(filter_state$get_state()))))
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c("DatetimeFilterState:", utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)))
  )
})
