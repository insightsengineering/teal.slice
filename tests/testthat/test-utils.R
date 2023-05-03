# check_ellipsis ----
method <- function(a, b, ..., stop = TRUE, allowed_args = character()) {
  check_ellipsis(..., stop = stop, allowed_args = allowed_args)
}

testthat::test_that("check_ellipsis with no unused", {
  testthat::expect_silent(method(a = 1, b = 2))
})

testthat::test_that("check_ellipsis with extra unamed arguments", {
  testthat::expect_error(method(a = 1, b = 2, 5, 6), "2 total unused argument\\(s\\)\\.")
  testthat::expect_warning(method(a = 1, b = 2, 5, 6, stop = FALSE), "2 total unused argument\\(s\\)\\.")
})

testthat::test_that("check_ellipsis with extra named arguments", {
  testthat::expect_error(
    method(a = 1, b = 2, c = 5, d = 6),
    "2 total unused argument\\(s\\)\\. 2 with name\\(s\\): c, d\\."
  )
  testthat::expect_warning(
    method(a = 1, b = 2, c = 5, d = 6, stop = FALSE),
    "2 total unused argument\\(s\\)\\. 2 with name\\(s\\): c, d\\."
  )
})

testthat::test_that("check_ellipsis with extra named and unamed arguments", {
  testthat::expect_error(method(a = 1, b = 2, c = 5, 6), "2 total unused argument\\(s\\). 1 with name\\(s\\): c\\.")
  testthat::expect_warning(
    method(a = 1, b = 2, c = 5, 6, stop = FALSE),
    "2 total unused argument\\(s\\). 1 with name\\(s\\): c\\."
  )
})

testthat::test_that("check_ellipsis with extra named and unamed arguments in wrong order", {
  testthat::expect_error(method(c = 5, 6, a = 1, b = 2), "2 total unused argument\\(s\\)\\. 1 with name\\(s\\): c\\.")
  testthat::expect_warning(
    method(a = 1, c = 5, b = 2, 6, stop = FALSE),
    "2 total unused argument\\(s\\)\\. 1 with name\\(s\\): c\\."
  )
})

testthat::test_that("check_ellipsis with allowed args", {
  testthat::expect_silent(method(a = 1, b = 2, z = 3, allowed_args = c("z")))
  testthat::expect_error(
    method(a = 1, b = 2, y = 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): y\\."
  )
  testthat::expect_error(
    method(a = 1, b = 2, y = 3, z = 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): y\\."
  )
  testthat::expect_error(
    method(a = 1, b = 2, 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\."
  )
  testthat::expect_silent(method(a = 1, b = 2, 3, allowed_args = c("")))
  testthat::expect_error(
    method(a = 1, b = 2, 3, z = 9, allowed_args = c("")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): z\\."
  )
  testthat::expect_silent(method(a = 1, b = 2, 3, z = 5, allowed_args = c("", "z", "y")))
  testthat::expect_silent(method(a = 1, b = 2, 3, z = 5, y = 4, allowed_args = c("", "z", "y")))
  testthat::expect_silent(method(a = 1, b = 2, allowed_args = c("", "z", "y")))
})



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
  testthat::expect_true(is.null(get_teal_bs_theme()))
  withr::with_options(list("teal.bs_theme" = bslib::bs_theme(version = "5")), {
    testthat::expect_s3_class(get_teal_bs_theme(), "bs_theme")
  })
})


# adjust_states ----
testthat::test_that("adjust_states accepts `teal_slices`", {
  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), selected = "setosa", keep_na = TRUE
    ),
    filter_var(
      dataname = "iris", varname = "Sepal.Length",
      choices = c(4.3, 7.9), selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE
    )
  )
  testthat::expect_no_error(adjust_states(fs))
  testthat::expect_error(adjust_states(unclass(fs)), "Assertion on 'tss' failed")
})

testthat::test_that("adjust_states returns `teal_slices` with choices set to  NULL and otherwise identical", {
  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), selected = "setosa", keep_na = TRUE
    ),
    filter_var(
      dataname = "iris", varname = "Sepal.Length",
      choices = c(4.3, 7.9), selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE
    )
  )
  fss <- expect_no_error(adjust_states(fs))

  testthat::expect_null(slices_field(fss, "choices"))

  fs[[1]]["choices"] <- NULL
  fs[[2]]["choices"] <- NULL
  fss[[1]]["choices"] <- NULL
  fss[[2]]["choices"] <- NULL
  testthat::expect_identical(fs, fss)
})
