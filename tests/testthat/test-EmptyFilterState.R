# get_call ----
testthat::test_that("get_call of default EmptyFilterState returns NULL by default", {
  filter_state <- EmptyFilterState$new(NA, dataname = "data", varname = "variable")
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call of default EmptyFilterState returns !is.na() call if keep_na is FALSE", {
  filter_state <- EmptyFilterState$new(NA, dataname = "data", varname = "variable", keep_na = FALSE)
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!is.na(variable))
  )
})

testthat::test_that("get_call of default EmptyFilterState returns null if disabled", {
  filter_state <- EmptyFilterState$new(NA, dataname = "data", varname = "variable", keep_na = FALSE, disabled = TRUE)
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})
