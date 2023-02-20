testthat::test_that("init_filter_state accepts a string, not name as varname", {
  testthat::expect_no_error(init_filter_state(7, varname = "test"))
  testthat::expect_error(init_filter_state(7, varname = quote(test)))
})

testthat::test_that("init_filter_state accepts a character vector of length 0 or 1 as varlabel", {
  testthat::expect_no_error(init_filter_state(7, varname = "test", varlabel = "test"))
  testthat::expect_no_error(init_filter_state(7, varname = "test", varlabel = character(0)))
})

test_that("dataname must be specified if extract_type is specified", {
  adsl <- scda::synthetic_cdisc_data("latest")$adsl
  testthat::expect_error(
    init_filter_state(
      adsl$SEX,
      varname = "SEX",
      dataname = NULL,
      extract_type = "matrix"
    ),
    regexp = "if extract_type is specified, dataname must also be specified"
  )
})

testthat::test_that("init_filter_state accepts character as extract_type", {
  testthat::expect_no_error(
    init_filter_state(7, varname = "test", extract_type = character(0))
  )

  testthat::expect_no_error(
    init_filter_state(7, varname = "test", dataname = "test", extract_type = "list")
  )
  testthat::expect_no_error(
    init_filter_state(7, varname = "test", dataname = "test", extract_type = "matrix")
  )
})

testthat::test_that("init_filter_state provides default values for varlabel and dataname", {
  filter_state <- init_filter_state(7, varname = "test")
  testthat::expect_equal(filter_state$get_varlabel(), character(0))
  testthat::expect_equal(filter_state$get_dataname(), character(1))
})

testthat::test_that("init_filter_state returns an EmptyFilterState if all values provided are NA", {
  testthat::expect_true(is(init_filter_state(NA, varname = "test"), "EmptyFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a longer numeric", {
  numbers <- 1
  testthat::expect_s3_class(init_filter_state(numbers, varname = "test"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a longer numeric", {
  numbers <- 1: getOption("teal.threshold_slider_vs_checkboxgroup") + 1
  testthat::expect_s3_class(init_filter_state(numbers, varname = "test"), "RangeFilterState")
})

testthat::test_that("init_filter_state returns a ChoicesFilterState object if passed a Date object of length 1", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = 1)
  testthat::expect_s3_class(init_filter_state(dates, varname = "test"), "ChoicesFilterState")
})

testthat::test_that("init_filter_state returns a DateFilterState object if passed longer a Date object", {
  dates <- seq(as.Date("1990/01/01"), by = 1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
  testthat::expect_s3_class(init_filter_state(dates, varname = "test"), "DateFilterState")
})

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a POSIXct or POSIXlt of length 1", {
    dates <- seq(as.Date("1990/01/01"), by = 1, length.out = 1)
    testthat::expect_s3_class(init_filter_state(as.POSIXct(dates), varname = "test"), "ChoicesFilterState")
    testthat::expect_s3_class(init_filter_state(as.POSIXlt(dates), varname = "test"), "ChoicesFilterState")
  })

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a longer POSIXct or POSIXlt", {
    dates <- seq(as.Date("1990/01/01"), by = 1, length.out = getOption("teal.threshold_slider_vs_checkboxgroup") + 1)
    testthat::expect_s3_class(init_filter_state(as.POSIXct(dates), varname = "test"), "DatetimeFilterState")
    testthat::expect_s3_class(init_filter_state(as.POSIXlt(dates), varname = "test"), "DatetimeFilterState")
  })

testthat::test_that("init_filter_state returns a RangeFilterState if passed a numeric array containing Inf", {
  testthat::expect_no_error(fs <- init_filter_state(c(1, 2, 3, 4, Inf), varname = "test"))
  testthat::expect_true(is(fs, "RangeFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed fewer than five non-NA elements", {
  testthat::expect_no_error(fs <- init_filter_state(c(1, 2, 3, 4, NA), varname = "test"))
  testthat::expect_true(is(fs, "ChoicesFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState, if passed a character array", {
  testthat::expect_no_error(fs <- init_filter_state(c("a"), varname = "test"))
  testthat::expect_true(is(fs, "ChoicesFilterState"))
})

testthat::test_that("init_filter_state return a LogicalFilterState, if passed a logical array", {
  testthat::expect_no_error(fs <- init_filter_state(c(TRUE), varname = "test"))
  testthat::expect_true(is(fs, "LogicalFilterState"))
})

testthat::test_that("init_filter_state default accepts a list", {
  fs <- init_filter_state(list(1, 2, 3), varname = "test")
  testthat::expect_true(inherits(fs, "FilterState"))
})
