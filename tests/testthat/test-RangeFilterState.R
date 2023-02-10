# initialize ----
testthat::test_that("The constructor accepts numerical values", {
  testthat::expect_no_error(RangeFilterState$new(c(1), varname = "test"))
})

testthat::test_that("The constructor accepts infinite values but not infinite only", {
  testthat::expect_no_error(RangeFilterState$new(c(1, Inf, -Inf), varname = "test"))
  testthat::expect_error(RangeFilterState$new(Inf, varname = "test"), "\"x\" contains no finite values")
  testthat::expect_error(RangeFilterState$new(c(Inf, NA), varname = "test"), "\"x\" contains no finite values")
})

# get_selected ----
testthat::test_that("get_selected returns range computed on a vector containing c(1, Inf, -Inf, NA)", {
  test <- c(1, Inf, -Inf, NA)
  filter_state <- RangeFilterState$new(test, varname = "test")
  expect_identical(
    shiny::isolate(filter_state$get_selected()),
    c(1, 1)
  )
})

# set_selected ----
# testthat::test_that("set_selected warns when the selected range intersects the possible range
#   but is not fully included in it", {
#   filter_state <- RangeFilterState$new(7, varname = "test")
#   testthat::expect_warning(filter_state$set_selected(c(1, 7)), "outside of the possible range")
#   testthat::expect_warning(filter_state$set_selected(c(7, 13)), "outside of the possible range")
#   testthat::expect_warning(filter_state$set_selected(c(1, 13)), "outside of the possible range")
# })

testthat::test_that("set_selected raises error when the passed values are not coercible to numeric", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_error(
    filter_state$set_selected(c(print)),
    "Values to set must be an atomic vector."
  )
})

testthat::test_that("set_selected accepts an array with two numerical elements", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_no_error(filter_state$set_selected(c(7, 7)))
})

# get_call ----
testthat::test_that("get_call returns a condition TRUE for all values passed to the constructor", {
  filter_state <- RangeFilterState$new(c(1, 2, 3), varname = "test")
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(test >= 1 & test <= 3))
  test <- c(1, 2, 3)
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns a condition TRUE for all values passed to the constructor", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(test >= 7 & test <= 7))
  test <- 7
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns a valid call after an unsuccessfull set_selected", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_error(suppressWarnings(
    filter_state$set_selected(c(1, 3)),
    regexp = "the upper bound of the range lower than the lower bound"
  ))
  test <- 7
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns the call with values passed in set_selected", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_selected(c(3, 4))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(test >= 3 & test <= 4))
})

testthat::test_that("get_call returns a condition true for the values from the range passed to set_selected", {
  filter_state <- RangeFilterState$new(c(3, 5), varname = "test")
  filter_state$set_selected(c(3, 5))
  test <- c(2:6)
  eval(shiny::isolate(filter_state$get_call()))
  testthat::expect_equal(eval(shiny::isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

testthat::test_that("get_call returns the call with a condition false for infinite values", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  test <- Inf
  testthat::expect_false(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition true for infinite values after set_keep_inf(TRUE)", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_keep_inf(TRUE)
  test <- Inf
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition returning NA for NA values", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  test <- NA
  testthat::expect_equal(eval(shiny::isolate(filter_state$get_call())), NA)
})

testthat::test_that("get_call returns a condition true for NAs after set_keep_na(TRUE)", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_keep_na(TRUE)
  test <- NA
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition true for NAs and Inf values after setting NA and Inf flag", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_keep_na(TRUE)
  filter_state$set_keep_inf(TRUE)
  test <- c(NA, Inf)
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})

# get_state ----
testthat::test_that("get_state returns a list identical to set_state input", {
  filter_state <- RangeFilterState$new(c(1.0, 8.0, NA_real_, Inf), varname = "test")
  state <- list(selected = c(2.0, 7.0), keep_na = TRUE, keep_inf = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

# set_state ----
testthat::test_that("set_state needs a named list with selected, keep_na and keep_inf elements", {
  filter_state <- RangeFilterState$new(c(1, 8, NA_real_, Inf), varname = "test")
  testthat::expect_no_error(filter_state$set_state(list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE)))
  testthat::expect_error(filter_state$set_state(list(selected = c(1, 2), unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  filter_state <- RangeFilterState$new(c(1, 8, NA_real_, Inf), varname = "test")
  filter_state$set_state(list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), c(1, 2))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_inf()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- RangeFilterState$new(c(1, 8, NA_real_, Inf), varname = "test")
  filter_state$set_state(list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE))
  testthat::expect_no_error(filter_state$set_state(list(selected = c(5, 6), keep_na = TRUE, keep_inf = TRUE)))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), c(5, 6))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
  testthat::expect_true(shiny::isolate(filter_state$get_keep_inf()))
})

# is_any_filtered ----
testthat::test_that(
  "RangeFilterState$is_any_filtered works properly when NA and Inf are present in data",
  code = {
    filter_state <- teal.slice:::RangeFilterState$new(
      c(NA, Inf, seq(1:10)),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )

    shiny::isolate(filter_state$set_keep_na(FALSE))
    shiny::isolate(filter_state$set_keep_inf(TRUE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(FALSE))
    shiny::isolate(filter_state$set_keep_inf(FALSE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_keep_inf(TRUE))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_keep_inf(FALSE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_selected(c(2, 10)))
    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_keep_inf(TRUE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_selected(c(1, 9)))
    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_keep_inf(TRUE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_selected(c(1, 10)))
    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_keep_inf(TRUE))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)


# format ----
testthat::test_that("$format() is a FilterStates's method that accepts indent", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
})

testthat::test_that("$format() asserts that indent is numeric", {
  testthat::expect_error(
    RangeFilterState$new(c(7), varname = "test")$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() returns a string representation the FilterState object", {
  filter_state <- RangeFilterState$new(c(7), varname = "test")
  filter_state$set_state(list(selected = c(7, 7)))
  testthat::expect_equal(
    shiny::isolate(filter_state$format(indent = 0)),
    paste(
      "Filtering on: test",
      "  Selected range: 7.000 - 7.000",
      "  Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("$format() prepends spaces to every line of the returned string", {
  filter_state <- RangeFilterState$new(c(7), varname = "test")
  filter_state$set_state(list(selected = c(7, 7)))
  for (i in 1:3) {
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = !!(i))),
      sprintf(
        "%sFiltering on: test\n%1$s  Selected range: 7.000 - 7.000\n%1$s  Include missing values: FALSE",
        format("", width = i)
      )
    )
  }
})


# get_pretty_range_step ----
testthat::test_that("private$get_pretty_range_step returns pretty step size", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = RangeFilterState,
    public = list(
      test_get_pretty_range_step = function(pretty_range) {
        private$get_pretty_range_step(pretty_range)
      }
    )
  )

  pretty_sepal_length <- pretty(iris$Sepal.Length, n = 100)
  filter_state <- test_class$new(pretty_sepal_length, varname = "test")
  step <- filter_state$test_get_pretty_range_step(pretty_sepal_length)
  testthat::expect_identical(step, 0.05)

  pretty_mpg <- pretty(mtcars$mpg, n = 100)

  filter_state <- test_class$new(pretty_mpg, varname = "test")
  step <- filter_state$test_get_pretty_range_step(pretty_mpg)
  testthat::expect_identical(step, 0.2)
})
