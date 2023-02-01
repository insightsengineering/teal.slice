testthat::test_that("The constructor accepts a Date object", {
  testthat::expect_no_error(DateFilterState$new(as.Date("2013-07-13"), varname = "test"))
})

testthat::test_that("get_call returns a condition true for the object passed in the constructor", {
  test_date <- as.Date("2013-07-13")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("set_selected accepts an array of two Date objects", {
  test_date <- as.Date("2013-07-13")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_no_error(filter_state$set_selected(c(test_date, test_date)))
})

testthat::test_that("set_selected warns when selection is not within the possible range", {
  test_date <- as.Date("13/07/2013")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_warning(
    filter_state$set_selected(c(test_date - 1, test_date)),
    regexp = "outside of the possible range"
  )
  testthat::expect_warning(
    filter_state$set_selected(c(test_date, test_date + 1)),
    regexp = "outside of the possible range"
  )
  testthat::expect_warning(
    filter_state$set_selected(c(test_date - 1, test_date + 1)),
    regexp = "outside of the possible range"
  )
})

testthat::test_that("set_selected limits the selected range to the lower and the upper bound of the possible range", {
  test_date <- as.Date(c("13/07/2013", "14/07/2013"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  suppressWarnings(filter_state$set_selected(c(test_date[1] - 1, test_date[1])))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), c(test_date[1], test_date[1]))

  suppressWarnings(filter_state$set_selected(c(test_date[2], test_date[2] + 1)))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), c(test_date[2], test_date[2]))

  suppressWarnings(filter_state$set_selected(c(test_date[1] - 1, test_date[2] + 1)))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), c(test_date[1], test_date[2]))
})

testthat::test_that("set_selected throws when selection is completely outside of the possible range", {
  test_date <- as.Date("13/07/2013")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(
    suppressWarnings(filter_state$set_selected(c(test_date - 2, test_date - 1))),
    regexp = "the upper bound of the range lower than the lower bound"
  )
})

testthat::test_that("set_selected throws when selection is not Date", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(
    filter_state$set_selected(c("a", "b")),
    "The array of set values must contain values coercible to Date."
  )
})

testthat::test_that("get_call returns a condition true for the objects in the selected range", {
  test_date <- as.Date(c("2013-07-13", "2013-07-14", "2013-07-15"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  filter_state$set_selected(c(test_date[2], test_date[2]))
  testthat::expect_equal(eval(shiny::isolate(filter_state$get_call())), c(FALSE, TRUE, FALSE))
  testthat::expect_equal(
    shiny::isolate(filter_state$get_call()),
    quote(test_date >= as.Date("2013-07-14") & test_date <= as.Date("2013-07-14"))
  )
})

testthat::test_that("get_call returns a condition evaluating to NA for NA values", {
  test_date <- as.Date(c("2013-07-13", NA))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_equal(eval(shiny::isolate(filter_state$get_call()))[2], NA)
})

testthat::test_that("get_call reutrns a condition evaluating to TRUE for NA values after set_keep_na(TRUE)", {
  test_date <- as.Date(c("2013-07-13", NA))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  filter_state$set_keep_na(TRUE)
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call()))[2])
})

testthat::test_that("set_state accepts a named list with selected and keep_na elements", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_no_error(filter_state$set_state(list(selected = c(test_date[2], test_date[3]), keep_na = TRUE)))
  testthat::expect_error(
    filter_state$set_state(list(selected = c(test_date[2], test_date[3]), unknown = TRUE)),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  filter_state$set_state(list(selected = c(test_date[2], test_date[3]), keep_na = TRUE))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), c(test_date[2], test_date[3]))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})


testthat::test_that("set_state overwrites fields included in the input only", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  filter_state$set_state(list(selected = c(test_date[2], test_date[3]), keep_na = TRUE))
  testthat::expect_no_error(filter_state$set_state(list(selected = c(test_date[3], test_date[4]))))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), c(test_date[3], test_date[4]))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that(
  "DateFilterState$is_any_filtered works properly when NA is present in data",
  code = {
    dates <- Sys.Date() + seq(1:10)
    filter_state <- teal.slice:::DateFilterState$new(
      c(dates, NA),
      varname = "x",
      input_dataname = as.name("data"),
      extract_type = character(0)
    )

    shiny::isolate(filter_state$set_keep_na(FALSE))
    shiny::isolate(filter_state$set_selected(c(dates[1], dates[10])))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_selected(c(dates[1], dates[10])))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_selected(c(dates[2], dates[10])))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_selected(c(dates[1], dates[9])))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

# Format
testthat::test_that("$format() is a FilterStates's method that accepts indent", {
  test_date <- as.Date("13/07/2013", format = "%d/%m/%y")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
})

testthat::test_that("$format() asserts that indent is numeric", {
  test_date <- as.Date("13/07/2013", format = "%d/%m/%y")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(
    filter_state$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() returns a string representation the FilterState object", {
  test_date <- as.Date("13/07/2013", format = "%d/%m/%y")
  filter_state <- DateFilterState$new(test_date, varname = "test")
  filter_state$set_state(list(selected = c(test_date, test_date)))
  testthat::expect_equal(
    shiny::isolate(filter_state$format(indent = 0)),
    paste(
      "Filtering on: test",
      "  Selected range: 2020-07-13 - 2020-07-13",
      "  Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("$format() prepends spaces to every line of the returned string", {
  test_date <- as.Date("13/07/2013", format = "%d/%m/%y")
  filter_state <- DateFilterState$new(test_date, varname = "test")
  filter_state$set_state(list(selected = c(test_date, test_date)))
  for (i in 1:3) {
    whitespace_indent <- paste0(rep(" ", i), collapse = "")
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = !!(i))),
      sprintf(
        "%sFiltering on: test\n%1$s  Selected range: 2020-07-13 - 2020-07-13\n%1$s  Include missing values: FALSE",
        format("", width = i)
      )
    )
  }
})
