# check_simple_name ----
test_that("check_simple_name behaves as expected", {
  testthat::expect_silent(check_simple_name("aas2df"))
  testthat::expect_silent(check_simple_name("ADSL"))
  testthat::expect_silent(check_simple_name("ADSLmodified"))
  testthat::expect_silent(check_simple_name("a1"))
  testthat::expect_silent(check_simple_name("ADSL_modified"))
  testthat::expect_silent(check_simple_name("ADSL_filtered"))
  testthat::expect_silent(check_simple_name("FILTERED_ADSL"))
  testthat::expect_silent(check_simple_name("FILTERED"))
  testthat::expect_silent(check_simple_name("ADSLFILTERED"))
  testthat::expect_silent(check_simple_name("a_1_2_b_"))

  testthat::expect_error(check_simple_name("1a"), "name '.+' must only contain alphanumeric characters")
  testthat::expect_error(check_simple_name("ADSL.modified"), "name '.+' must only contain alphanumeric characters")
  testthat::expect_error(check_simple_name("a1..."), "name '.+' must only contain alphanumeric characters")
  testthat::expect_error(check_simple_name("a a"), "name '.+' must only contain alphanumeric characters")
  testthat::expect_error(check_simple_name("_A_b"), "name '.+' must only contain alphanumeric characters")
})

# get_teal_bs_theme ----
testthat::test_that("get_teal_bs_theme", {
  testthat::expect_null(get_teal_bs_theme())
  withr::with_options(list("teal.bs_theme" = bslib::bs_theme(version = "5")), {
    testthat::expect_s3_class(get_teal_bs_theme(), "bs_theme")
  })
})

# make_c_call ----
testthat::test_that("make_c_call", {
  testthat::expect_identical(teal.slice:::make_c_call(1:3), quote(c(1L, 2L, 3L)))
  testthat::expect_identical(teal.slice:::make_c_call(1), 1)
})
