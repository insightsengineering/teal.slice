testthat::test_that("init_filter_state accepts a string, not name as varname", {
  testthat::expect_no_error(init_filter_state(7, varname = "test", dataname = "data"))
  testthat::expect_error(init_filter_state(7, varname = quote(test)))
})

testthat::test_that("init_filter_state accepts a character vector of length 0 or 1 as varlabel", {
  testthat::expect_no_error(init_filter_state(7, varname = "test", varlabel = "test", dataname = "data"))
  testthat::expect_no_error(init_filter_state(7, varname = "test", varlabel = character(0), dataname = "data"))
})

testthat::test_that("init_filter_state accepts character as extract_type", {
  testthat::expect_no_error(
    init_filter_state(7, varname = "test", extract_type = character(0), dataname = "test")
  )

  testthat::expect_no_error(
    init_filter_state(7, varname = "test", dataname = "test", extract_type = "list")
  )
  testthat::expect_no_error(
    init_filter_state(7, varname = "test", dataname = "test", extract_type = "matrix")
  )
})

testthat::test_that("init_filter_state returns an EmptyFilterState if all values provided are NA", {
  testthat::expect_true(is(init_filter_state(NA, varname = "test", dataname = "data"), "EmptyFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a longer numeric", {
  numbers <- 1
  testthat::expect_s3_class(init_filter_state(numbers, varname = "test", dataname = "data"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a longer numeric", {
  numbers <- 1:getOption("teal.threshold_slider_vs_checkboxgroup") + 1
  testthat::expect_s3_class(init_filter_state(numbers, varname = "test", dataname = "data"), "RangeFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState object if passed a Date object of length 1", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = 1)
  testthat::expect_s3_class(init_filter_state(dates, varname = "test", dataname = "data"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a DateFilterState object if passed longer a Date object", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
  testthat::expect_s3_class(init_filter_state(dates, varname = "test", dataname = "data"), "DateFilterState")
})

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a POSIXct or POSIXlt of length 1", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = 1)
  testthat::expect_s3_class(
    init_filter_state(as.POSIXct(dates), varname = "test", dataname = "data"), "ChoicesFilterState")
  testthat::expect_s3_class(
    init_filter_state(as.POSIXlt(dates), varname = "test", dataname = "data"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a longer POSIXct or POSIXlt", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
  testthat::expect_s3_class(
    init_filter_state(as.POSIXct(dates), varname = "test", dataname = "data"), "DatetimeFilterState")
  testthat::expect_s3_class(
    init_filter_state(as.POSIXlt(dates), varname = "test", dataname = "data"), "DatetimeFilterState")
})

testthat::test_that("init_filter_state returns a RangeFilterState if passed a numeric array containing Inf", {
  testthat::expect_no_error(fs <- init_filter_state(c(1, 2, 3, 4, Inf), varname = "test", dataname = "data"))
  testthat::expect_true(is(fs, "RangeFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed fewer than five non-NA elements", {
  testthat::expect_no_error(fs <- init_filter_state(c(1, 2, 3, 4, NA), varname = "test", dataname = "data"))
  testthat::expect_true(is(fs, "ChoicesFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState, if passed a character array", {
  testthat::expect_no_error(fs <- init_filter_state(c("a"), varname = "test", dataname = "data"))
  testthat::expect_true(is(fs, "ChoicesFilterState"))
})

testthat::test_that("init_filter_state return a LogicalFilterState, if passed a logical array", {
  testthat::expect_no_error(fs <- init_filter_state(c(TRUE), varname = "test", dataname = "data"))
  testthat::expect_true(is(fs, "LogicalFilterState"))
})

testthat::test_that("init_filter_state default accepts a list", {
  fs <- init_filter_state(list(1, 2, 3), varname = "test", dataname = "data")
  testthat::expect_true(inherits(fs, "FilterState"))
})
