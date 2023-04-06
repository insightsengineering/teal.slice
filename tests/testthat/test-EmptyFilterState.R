

# set_state ----
testthat::test_that("set_state returns a list identical to set_state input", {
  filter_state <- EmptyFilterState$new(character(0), dataname = "data", varname = "variable")
  state <- filter_var(dataname = "data", varname = "variable", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), unclass(state))
})

# get_state ----
testthat::test_that("get_state returns a list identical to set_state input", {
  filter_state <- EmptyFilterState$new(character(0), dataname = "data", varname = "variable")
  state <- filter_var(dataname = "data", varname = "variable", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), unclass(state))
})

# get_call ----
testthat::test_that("get_call returns NULL after set_keep_na(FALSE)", {
  # filter_state <- EmptyFilterState$new("7", dataname = "data", varname = "variable", keep_na = FALSE)
  # testthat::expect_null(shiny::isolate(filter_state$get_call()), NULL)
})

testthat::test_that("get_call returns a call if keep_na is TRUE", {
  filter_state <- EmptyFilterState$new(character(0), dataname = "data", varname = "variable", keep_na = TRUE)
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(variable)))
})


# is_any_filtered ----
testthat::test_that("is_any_filtered returns FALSE when keep_na is TRUE and returns TRUE when keep_na is FALSE", {
  filter_state <- teal.slice:::EmptyFilterState$new(
    rep(NA, 10),
    dataname = "data",
    varname = "variable",
    keep_na = TRUE
  )
  testthat::expect_false(
    shiny::isolate(filter_state$is_any_filtered())
  )

  filter_state <- teal.slice:::EmptyFilterState$new(
    rep(NA, 10),
    dataname = "data",
    varname = "variable",
    keep_na = FALSE
  )
  testthat::expect_true(
    shiny::isolate(filter_state$is_any_filtered())
  )
})
