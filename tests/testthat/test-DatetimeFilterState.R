posixct <- as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:9
posixlt <- as.POSIXlt(posixct)

# initialize ----
testthat::test_that("constructor accepts a POSIXct or POSIXlt object", {
  testthat::expect_no_error(
    DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  )
  testthat::expect_no_error(
    DatetimeFilterState$new(posixlt, dataname = "data", varname = "variable")
  )
  testthat::expect_error(
    DatetimeFilterState$new(as.Date(posixct), dataname = "data", varname = "variable"),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor raises warning when selected is out of range", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      dataname = "data", varname = "variable", selected = range(posixct) + c(-1, 1)
    ),
    "outside of the range"
  )
})

testthat::test_that("constructor raises warning when selected is not sorted", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      dataname = "data", varname = "variable", selected = posixct[c(10, 1)]
    ),
    "Start date '2000-01-01 12:00:09' is set after"
  )
})

testthat::test_that("constructor raises error when selection is not Datetime or coercible", {
  testthat::expect_error(
    DatetimeFilterState$new(posixct, dataname = "data", varname = "variable", selected = c("a", "b")),
    "The array of set values must contain values coercible to POSIX"
  )
})

testthat::test_that("constructor raises warning when choices is out of range", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      dataname = "data", varname = "variable", choices = range(posixct) + c(-1, 1)
    ),
    "outside of variable range"
  )
})

testthat::test_that("constructor raises warning when choices is not sorted", {
  testthat::expect_warning(
    DatetimeFilterState$new(
      posixct,
      dataname = "data", varname = "variable", choices = posixct[c(10, 1)]
    ),
    "Invalid choices"
  )
})

testthat::test_that("constructor raises error when choices is not Date", {
  testthat::expect_error(
    DatetimeFilterState$new(posixct, dataname = "data", varname = "variable", choices = c("a", "b")),
    "Assertion on 'choices' failed"
  )
})


# set_state ----
testthat::test_that("set_state: selected accepts vector of two POSIXct objects or coercible", {
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")

  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[1:2]))
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[1])),
    "The array of set values must have length two"
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = 1:2)),
    "The array of set values must contain values coercible to POSIX"
  )
})

testthat::test_that("set_state: selected raises warning when selection not fully included in range", {
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "posixct")

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
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "posixct")

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
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("a", "b"))),
    "The array of set values must contain values coercible to POSIX"
  )
})


# get_call ----
testthat::test_that("get_call returns call that encompasses all values passed to constructor", {
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") & variable <
            as.POSIXct("2000-01-01 12:00:10", tz = "GMT"))
  )
})

testthat::test_that("get_call returns a condition true for the object in the selected range", {
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[2:3]))
  variable <- posixct[1:4]
  testthat::expect_equal(
    eval(shiny::isolate(filter_state$get_call())),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$get_call()),
    quote(
      variable >= as.POSIXct("2000-01-01 12:00:01", tz = "GMT") & variable <
        as.POSIXct("2000-01-01 12:00:03", tz = "GMT")
    )
  )
})

testthat::test_that("get_call returns a condition evaluating to TRUE for NA values is keep_na is TRUE", {
  variable <- c(posixct, NA)
  filter_state <- DatetimeFilterState$new(
    variable,
    dataname = "data", varname = "variable"
  )
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call()))[11], TRUE)
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call()))[11], NA)
})


testthat::test_that("get_call preserves timezone of ISO object passed to constructor", {
  variable <- ISOdate(2021, 8, 25, tz = "Australia/Brisbane")
  filter_state <- DatetimeFilterState$new(
    variable,
    dataname = "data", varname = "variable"
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$get_call()),
    quote(
        variable >= as.POSIXct("2021-08-25 12:00:00", tz = "Australia/Brisbane") &
        variable < as.POSIXct("2021-08-25 12:00:01", tz = "Australia/Brisbane")
    )
  )
})


testthat::test_that("is_any_filtered works properly when NA is present in data",
  code = {
    filter_state <- teal.slice:::DatetimeFilterState$new(
      x = c(posixct, NA),
      x_reactive = reactive(NULL),
      varname = "variable",
      dataname = "data",
      extract_type = character(0)
    )

    shiny::isolate(filter_state$set_state(filter_var(keep_na = FALSE, varname = "variable", dataname = "data")))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_state(filter_var(keep_na = TRUE, varname = "variable", dataname = "data")))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

# format ----
testthat::test_that("format accepts numeric as indent", {
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0L)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_state$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format returns a properly formatted string representation", {
  skip("temporary")
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste(
      "  Filtering on: variable",
      "    Selected range: 2000-01-01 12:00:00 - 2000-01-01 12:00:09",
      "    Include missing values: TRUE",
      sep = "\n"
    )
  )
})

testthat::test_that("format prepends spaces to every line of the returned string", {
  skip("temporary")
  filter_state <- DatetimeFilterState$new(posixct, dataname = "data", varname = "variable")
  for (i in 0:3) {
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = i)),
      paste(format("", width = i),
        c(
          "Filtering on: variable",
          sprintf("%sSelected range: 2000-01-01 12:00:00 - 2000-01-01 12:00:09", format("", width = i)),
          sprintf("%sInclude missing values: TRUE", format("", width = i))
        ),
        sep = "", collapse = "\n"
      )
    )
  }
})

# is_any_filtered ----
testthat::test_that("is_any_filtered works properly when NA is present in data", {
  filter_state <- teal.slice:::DatetimeFilterState$new(c(posixct, NA), varname = "variable", dataname = "data")

  shiny::isolate(filter_state$set_state(filter_var(varname = "variable", dataname = "data", keep_na = FALSE)))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  shiny::isolate(filter_state$set_state(filter_var(varname = "variable", dataname = "data", keep_na = TRUE)))
  testthat::expect_false(shiny::isolate(filter_state$is_any_filtered()))

  shiny::isolate(filter_state$set_state(
    filter_var(varname = "variable", dataname = "data", selected = c(posixct[2], posixct[10]))
  ))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))
})

testthat::test_that("is_any_filtered returns TRUE when enabled and FALSE when disabled", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = DatetimeFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(posixct, dataname = "data", varname = "variable")
  fs$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[1:2], keep_na = TRUE))
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$disable())
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$enable())
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
})

testthat::test_that("is_any_filtered reacts to choices", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = DatetimeFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
  fs <- testfs$new(posixct, dataname = "data", varname = "variable", choices = posixct[c(1, 3)])
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
})
