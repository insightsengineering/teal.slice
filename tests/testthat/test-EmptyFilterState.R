testthat::test_that("get_call returns NULL after set_keep_na(FALSE)", {
  filter_state <- EmptyFilterState$new(7, varname = "7", dataname = "data")
  filter_state$set_keep_na(FALSE)
  testthat::expect_identical(shiny::isolate(filter_state$get_call()), quote(!is.na(7)))
})

testthat::test_that("get_call returns a call after set_keep_na(TRUE)", {
  filter_state <- EmptyFilterState$new(7, varname = "test", dataname = "data")
  filter_state$set_keep_na(TRUE)
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(test)))
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- EmptyFilterState$new(7, varname = "test", dataname = "data")
  filter_state$set_state(list(keep_na = TRUE))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
  testthat::expect_error(
    filter_state$set_state(list(selected = 1)),
    "All values in variable 'test' are `NA`"
  )
  testthat::expect_error(
    filter_state$set_state(list(keep_na = FALSE, unknown = TRUE)),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("get_state returns a list identical to set_state input", {
  filter_state <- EmptyFilterState$new(NA_character_, varname = "test", dataname = "data")
  state <- list(keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

testthat::test_that(
  "EmptyFilterState$is_any_filtered returns FALSE when keep_na is TRUE and returns TRUE when keep_na is FALSE",
  code = {
    filter_state <- teal.slice:::EmptyFilterState$new(
      rep(NA, 10),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )
    shiny::isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)
