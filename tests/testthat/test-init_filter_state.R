# argument checks ----
testthat::test_that("init_filter_state checks arguments", {
  testthat::expect_error(init_filter_state(), "argument \"dataname\" is missing")
  testthat::expect_error(init_filter_state(dataname = "data"), "argument \"varname\" is missing")
  testthat::expect_error(init_filter_state(dataname = "data", varname = "variable"), "argument \"x\" is missing")

  testthat::expect_error(
    init_filter_state(7, dataname = quote(pi), varname = "variable"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    init_filter_state(7, dataname = call("data"), varname = "variable"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    init_filter_state(7, dataname = "data", varname = quote(pi)),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    init_filter_state(7, dataname = "data", varname = call("test")),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    init_filter_state(x = 7, dataname = "data", varname = "variable", x_reactive = NULL),
    "Assertion on 'x_reactive' failed"
  )
  testthat::expect_error(
    init_filter_state(x = 7, dataname = "data", varname = "variable", keep_na = "TRUE"),
    "Assertion on 'keep_na' failed"
  )
  testthat::expect_error(
    init_filter_state(x = 7, dataname = "data", varname = "variable", keep_inf = "TRUE"),
    "Assertion on 'keep_inf' failed"
  )
  testthat::expect_error(
    init_filter_state(x = 7, dataname = "data", varname = "variable", fixed = NULL),
    "Assertion on 'fixed' failed"
  )
  testthat::expect_error(
    init_filter_state(x = 7, dataname = "data", varname = "variable", extract_type = "other"),
    "Assertion on 'extract_type' failed"
  )
})

# return type ----
testthat::test_that("init_filter_state returns an EmptyFilterState if all values provided are NA", {
  testthat::expect_s3_class(init_filter_state(NA, dataname = "data", varname = "variable"), "EmptyFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a numeric of length 1", {
  numbers <- 1
  testthat::expect_s3_class(init_filter_state(numbers, dataname = "data", varname = "variable"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a RangeFilterState if passed a longer numeric", {
  numbers <- seq(1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
  testthat::expect_s3_class(init_filter_state(numbers, dataname = "data", varname = "variable"), "RangeFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState object if passed a Date object of length 1", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = 1)
  testthat::expect_s3_class(init_filter_state(dates, dataname = "data", varname = "variable"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a DateFilterState object if passed longer a Date object", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
  testthat::expect_s3_class(init_filter_state(dates, dataname = "data", varname = "variable"), "DateFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState object if passed
  a POSIXct or POSIXlt of length 1", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = 1)
  testthat::expect_s3_class(
    init_filter_state(as.POSIXct(dates), dataname = "data", varname = "variable"), "ChoicesFilterState"
  )
  testthat::expect_s3_class(
    init_filter_state(as.POSIXlt(dates), dataname = "data", varname = "variable"), "ChoicesFilterState"
  )
})

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a longer POSIXct or POSIXlt", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
  testthat::expect_s3_class(
    init_filter_state(as.POSIXct(dates), dataname = "data", varname = "variable"), "DatetimeFilterState"
  )
  testthat::expect_s3_class(
    init_filter_state(as.POSIXlt(dates), dataname = "data", varname = "variable"), "DatetimeFilterState"
  )
})

testthat::test_that("init_filter_state returns a RangeFilterState if passed a numeric vector containing Inf", {
  testthat::expect_no_error(fs <- init_filter_state(c(1, 2, 3, 4, Inf), dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "RangeFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed fewer than five non-NA elements", {
  testthat::expect_no_error(fs <- init_filter_state(c(1, 2, 3, 4, NA), dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState, if passed a character vector of any length", {
  testthat::expect_no_error(fs <- init_filter_state("a", dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "ChoicesFilterState")

  chars <- replicate(
    n = getOption("teal.threshold_slider_vs_checkboxgroup") + 1,
    paste(sample(letters, size = 10, replace = TRUE), collapse = "")
  )
  testthat::expect_no_error(fs <- init_filter_state("a", dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState, if passed a factor of any length", {
  testthat::expect_no_error(fs <- init_filter_state(factor("a"), dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "ChoicesFilterState")

  chars <- factor(
    replicate(
      n = getOption("teal.threshold_slider_vs_checkboxgroup") + 1,
      paste(sample(letters, size = 10, replace = TRUE), collapse = "")
    )
  )
  testthat::expect_no_error(fs <- init_filter_state("a", dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "ChoicesFilterState")
})

testthat::test_that("init_filter_state return a LogicalFilterState, if passed a logical vector", {
  testthat::expect_no_error(fs <- init_filter_state(c(TRUE), dataname = "data", varname = "variable"))
  testthat::expect_s3_class(fs, "LogicalFilterState")
})

testthat::test_that("init_filter_state default accepts a list", {
  fs <- init_filter_state(list(1, 2, 3), dataname = "data", varname = "variable")
  testthat::expect_s3_class(fs, "FilterState")
})
