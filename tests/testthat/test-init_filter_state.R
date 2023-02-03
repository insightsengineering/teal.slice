testthat::test_that("init_filter_state accepts a string or name as varname", {
  testthat::expect_no_error(init_filter_state(7, varname = "test"))
  testthat::expect_no_error(init_filter_state(7, varname = quote(test)))
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

testthat::test_that("init_filter_state provides default values for varlabel, dataname, use_datname", {
  filter_state <- init_filter_state(7, varname = "test")
  testthat::expect_equal(filter_state$get_varlabel(), character(0))
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "NULL")
})

testthat::test_that("init_filter_state returns an EmptyFilterState if all values provided are NA", {
  testthat::expect_true(is(init_filter_state(NA, varname = "test"), "EmptyFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a numeric array of length < 5", {
  testthat::expect_true(is(init_filter_state(c(1, 2, 3, 4), varname = "test"), "ChoicesFilterState"))
  testthat::expect_true(is(init_filter_state(c(1, 2, 3, 4, 5), varname = "test"), "RangeFilterState"))
})

testthat::test_that("init_filter_state returns a DateFilterState object if passed a Date object", {
  testthat::expect_true(is(init_filter_state(as.Date("1990/01/01"), varname = "test"), "DateFilterState"))
})

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a POSIXct or POSIXlt object", {
    testthat::expect_true(is(init_filter_state(as.POSIXct("1900/01/01"), varname = "test"), "DatetimeFilterState"))
    testthat::expect_true(is(init_filter_state(as.POSIXlt("1900/01/01"), varname = "test"), "DatetimeFilterState"))
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
