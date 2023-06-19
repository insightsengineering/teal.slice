# get_call ----
testthat::test_that("get_call of default EmptyFilterState returns NULL by default", {
  filter_state <- EmptyFilterState$new(NA, slice = filter_var(dataname = "data", varname = "variable"))
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call of default EmptyFilterState returns !is.na() call if keep_na is FALSE", {
  filter_state <- EmptyFilterState$new(NA, slice = filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!is.na(variable))
  )
})


# format ---
testthat::test_that("format accepts logical show_all", {
  filter_state <- EmptyFilterState$new(NA, slice = filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = TRUE)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = FALSE)))
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = 1)),
    "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = 0)),
    "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = "TRUE")),
    "Assertion on 'show_all' failed"
  )
})

testthat::test_that("format returns a properly formatted string representation", {
  filter_state <- EmptyFilterState$new(NA, slice = filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "EmptyFilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "EmptyFilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# print ---

testthat::test_that("print returns a properly formatted string representation", {
  filter_state <- EmptyFilterState$new(NA, slice = filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print())),
    c(
      "EmptyFilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()))),
      " "
    )
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c(
      "EmptyFilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)),
      " "
    )
  )
})
