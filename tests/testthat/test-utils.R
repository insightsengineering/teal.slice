
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
    filter_var(dataname = "iris", varname = "Species",
               choices = c("setosa", "versicolor", "virginica"), selected = "setosa", keep_na = TRUE),
    filter_var(dataname = "iris", varname = "Sepal.Length",
               choices = c(4.3, 7.9), selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE)
  )
  testthat::expect_no_error(adjust_states(fs))
  testthat::expect_error(adjust_states(unclass(fs)), "Assertion on 'tss' failed")
})

testthat::test_that("adjust_states returns `teal_slices` with choices set to  NULL and otherwise identical", {
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Species",
               choices = c("setosa", "versicolor", "virginica"), selected = "setosa", keep_na = TRUE),
    filter_var(dataname = "iris", varname = "Sepal.Length",
               choices = c(4.3, 7.9), selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE)
  )
  fss <- expect_no_error(adjust_states(fs))

  testthat::expect_null(slices_field(fss, "choices"))

  fs[[1]]["choices"] <- NULL
  fs[[2]]["choices"] <- NULL
  fss[[1]]["choices"] <- NULL
  fss[[2]]["choices"] <- NULL
  testthat::expect_identical(fs, fss)
})

