posixct <- as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:9
posixlt <- as.POSIXlt(posixct)

testthat::test_that("constructor accepts a POSIXct or POSIXlt object", {
  testthat::expect_no_error(DatetimeFilterState$new(posixct, x_reactive = reactive(NULL), varname = "variable"))
  testthat::expect_no_error(DatetimeFilterState$new(posixlt, x_reactive = reactive(NULL), varname = "variable"))
})

testthat::test_that("get_call returns call that encompasses all values passed to constructor", {
  filter_state <- DatetimeFilterState$new(posixct, x_reactive = reactive(NULL), varname = "variable")
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= as.POSIXct("2000-01-01 12:00:00", tz = "GMT") & variable <
      as.POSIXct("2000-01-01 12:00:10", tz = "GMT"))
  )
})

testthat::test_that("set selected accepts an array of two POSIXct objects", {
  filter_state <- DatetimeFilterState$new(posixct, x_reactive = reactive(NULL), varname = "variable")
  testthat::expect_no_error(filter_state$set_selected(posixct[1:2]))
})

testthat::test_that("get_call returns a condition true for the object in the selected range", {
  filter_state <- DatetimeFilterState$new(posixct, x_reactive = reactive(NULL), varname = "variable")
  filter_state$set_selected(posixct[2:3])
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

testthat::test_that("get_call returns a condition evaluating to TRUE for NA values after set_keep_na(TRUE)", {
  variable <- c(posixct, NA)
  filter_state <- DatetimeFilterState$new(variable, x_reactive = reactive(NULL), varname = "variable")
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call()))[11], NA)
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call()))[11], TRUE)
})


testthat::test_that("DatetimeFilterState echoes the timezone of the ISO object passed to the constructor", {
  objects <- ISOdate(2021, 8, 25, tz = "Australia/Brisbane")
  filter_state <- DatetimeFilterState$new(objects, x_reactive = reactive(NULL), varname = "objects")
  testthat::expect_equal(
    shiny::isolate(filter_state$get_call()),
    quote(
      objects >= as.POSIXct("2021-08-25 12:00:00", tz = "Australia/Brisbane") &
        objects < as.POSIXct("2021-08-25 12:00:01", tz = "Australia/Brisbane")
    )
  )
})

testthat::test_that("set_selected warns when the selected range intersects the range but is not fully included in it", {
  objects <- as.POSIXct(c(2, 3), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_warning(filter_state$set_selected(c(objects[1] - 1, objects[1])), "outside of the range")
  testthat::expect_warning(filter_state$set_selected(c(objects[2], objects[2] + 1)), "outside of the range")
  testthat::expect_warning(
    filter_state$set_selected(c(objects[1] - 1, objects[2] + 1)),
    "outside of the range"
  )
})

testthat::test_that("set_selected throws when the selected range is completely outside of the possible range", {
  objects <- as.POSIXct(c(2, 3), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, x_reactive = reactive(NULL), varname = "objects")
  testthat::expect_warning(
    filter_state$set_selected(c(objects[2] + 1, objects[2] + 2)),
    "is outside of the range"
  )
})

testthat::test_that("set_selected limits the selected range to the lower and the upper bound of the possible range", {
  objects <- as.POSIXct(c(2, 3), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, x_reactive = reactive(NULL), varname = "objects")
  suppressWarnings(filter_state$set_selected(c(objects[1] - 1, objects[1])))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), c(objects[1], objects[1]))

  suppressWarnings(filter_state$set_selected(c(objects[2], objects[2] + 1)))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), c(objects[2], objects[2]))

  suppressWarnings(filter_state$set_selected(c(objects[1] - 1, objects[2] + 1)))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), c(objects[1], objects[2]))
})

testthat::test_that("set_selected throws when the value type cannot be interpreted as POSIX", {
  objects <- as.POSIXct(c(1, 2, 3), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, x_reactive = reactive(NULL), varname = "objects")
  testthat::expect_error(
    filter_state$set_selected(c("a", "b")),
    "The array of set values must contain values coercible to POSIX."
  )
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  objects <- as.POSIXct(c(1:4), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, x_reactive = reactive(NULL), varname = "test")
  testthat::expect_no_error(filter_state$set_state(list(selected = c(objects[2], objects[3]), keep_na = TRUE)))
  testthat::expect_error(
    filter_state$set_state(list(selected = c(objects[3], objects[4]), unknown = TRUE)),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  objects <- as.POSIXct(c(1:4), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  filter_state$set_state(list(selected = c(objects[2], objects[3]), keep_na = TRUE))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), c(objects[2], objects[3]))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01 00:00:00")
  filter_state <- DatetimeFilterState$new(objects, x_reactive = reactive(NULL), varname = "test")
  filter_state$set_state(list(selected = c(objects[2], objects[3]), keep_na = TRUE))
  testthat::expect_no_error(filter_state$set_state(list(selected = c(objects[3], objects[4]))))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), c(objects[3], objects[4]))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that(
  "DatetimeFilterState$is_any_filtered works properly when NA is present in data",
  code = {
    filter_state <- teal.slice:::DatetimeFilterState$new(
      x = c(posixct, NA),
      x_reactive = reactive(NULL),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )

    shiny::isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

# Format
testthat::test_that("$format() is a FilterStates's method that accepts indent", {
  object <- as.POSIXct(8, origin = "1900/01/01 00:00:00", tz = "GMT")
  filter_state <- DatetimeFilterState$new(object, x_reactive = reactive(NULL), varname = "test")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
})

testthat::test_that("$format() asserts that indent is numeric", {
  object <- as.POSIXct(8, origin = "1900/01/01 00:00:00", tz = "GMT")
  filter_state <- DatetimeFilterState$new(object, x_reactive = reactive(NULL), varname = "test")
  testthat::expect_error(
    filter_state$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() returns a string representation the FilterState object", {
  # This test is skipped on versions lower than 4.1.0 because on lower versions
  # the `c` function drops the time zone attribute silently from the POSIXct objects,
  # the state set in `filter_state` is incorrect and the test fails.
  testthat::skip_if(
    utils::compareVersion(sprintf("%s.%s", version["major"], version["minor"]), "4.1.0") < 0,
    message = "Skipped on R versions lower than 4.1.0"
  )
  object <- as.POSIXct(8, origin = "1900/01/01 00:00:00", tz = "GMT")
  filter_state <- DatetimeFilterState$new(object, x_reactive = reactive(NULL), varname = "test")
  filter_state$set_state(list(selected = c(object, object)))
  testthat::expect_equal(
    shiny::isolate(filter_state$format(indent = 0)),
    paste(
      "Filtering on: test",
      "  Selected range: 1900-01-01 00:00:08 - 1900-01-01 00:00:08",
      "  Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("$format() prepends spaces to every line of the returned string", {
  # This test is skipped on versions lower than 4.1.0 because on lower versions
  # the `c` function drops the time zone attribute silently from the POSIXct objects,
  # the state set in `filter_state` is incorrect and the test fails.
  testthat::skip_if(
    utils::compareVersion(sprintf("%s.%s", version["major"], version["minor"]), "4.1.0") < 0,
    message = "Skipped on R versions lower than 4.1.0"
  )
  object <- as.POSIXct(8, origin = "1900/01/01 00:00:00", tz = "GMT")
  filter_state <- DatetimeFilterState$new(object, x_reactive = reactive(NULL), varname = "test")
  filter_state$set_state(list(selected = c(object, object)))
  for (i in 1:3) {
    whitespace_indent <- paste0(rep(" ", i), collapse = "")
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = !!(i))),
      sprintf(
        paste(
          "%sFiltering on: test",
          "%1$s  Selected range: 1900-01-01 00:00:08 - 1900-01-01 00:00:08",
          "%1$s  Include missing values: FALSE",
          sep = "\n"
        ),
        format("", width = i)
      )
    )
  }
})
