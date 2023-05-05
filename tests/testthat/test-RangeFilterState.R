nums <- 1:10

# initialize ----
testthat::test_that("constructor accepts numerical values", {
  testthat::expect_no_error(RangeFilterState$new(nums, dataname = "data", varname = "variable"))
  testthat::expect_error(
    RangeFilterState$new(as.character(nums), dataname = "data", varname = "variable"), "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor accepts infinite values but not infinite only", {
  testthat::expect_no_error(
    RangeFilterState$new(c(nums, Inf, -Inf), dataname = "data", varname = "variable")
  )
  testthat::expect_error(
    RangeFilterState$new(Inf, dataname = "data", varname = "variable"),
    "\"x\" contains no finite values"
  )
  testthat::expect_error(
    RangeFilterState$new(c(Inf, NA), dataname = "data", varname = "variable"),
    "\"x\" contains no finite values"
  )
})

testthat::test_that("constructor initializes keep_inf = TRUE by default if x contains Infs", {
  filter_state <- RangeFilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_state())$keep_inf)
  filter_state <- RangeFilterState$new(c(7, Inf), dataname = "data", varname = "7")
  testthat::expect_true(shiny::isolate(filter_state$get_state())$keep_inf)
})

testthat::test_that("constructor raises error when selected is not sorted", {
  testthat::expect_error(
    RangeFilterState$new(
      nums,
      dataname = "data", varname = "variable", selected = nums[c(10, 1)]
    ),
    "Assertion on 'x' failed: Must be sorted"
  )
})

testthat::test_that("constructor raises error when selection is not numeric or coercible", {
  testthat::expect_error(
    suppressWarnings(
      RangeFilterState$new(nums, dataname = "data", varname = "variable", selected = c("a", "b"))
    ),
    "The array of set values must contain values coercible to numeric"
  )
})

testthat::test_that("constructor raises error when choices is out of range", {
  testthat::expect_warning(
    RangeFilterState$new(
      nums,
      dataname = "data", varname = "variable", choices = range(nums) + c(-1, 1)
    ),
    "Choices adjusted"
  )
})

testthat::test_that("constructor raises warning when choices is not sorted", {
  testthat::expect_warning(
    RangeFilterState$new(
      nums,
      dataname = "data", varname = "variable", choices = nums[c(10, 1)]
    ),
    "Invalid choices"
  )
})

testthat::test_that("constructor raises error when choices is not numeric or coercible", {
  testthat::expect_error(
    suppressWarnings(
      RangeFilterState$new(nums, dataname = "data", varname = "variable", choices = c("a", "b"))
    ),
    "Assertion on 'choices' failed"
  )
})


# set_state ----
testthat::test_that("set_state: selected accepts vector of two numbers or coercible", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")

  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = nums[1:2]))
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = nums[1])),
    "The array of set values must have length two"
  )
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = as.character(1:2)))
  )
  testthat::expect_error(
    suppressWarnings(
      filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = as.character("a", "b")))
    ),
    "The array of set values must contain values coercible to numeric"
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(print))),
    "Assertion on 'selected' failed"
  )
})


# get_call ----
testthat::test_that("get_call returns call encompassing all values passed to constructor", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable",
                                       keep_na = FALSE, keep_inf = FALSE)
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(variable >= 1 & variable <= 10))
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable", select = c(4, 6),
                                       keep_na = FALSE, keep_inf = FALSE)
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(variable >= 4 & variable <= 6))
})

testthat::test_that("get_call returns call encompassing all values passed in set_selected", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable", selected = c(3, 4),
                                       keep_na = FALSE, keep_inf = FALSE)
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(variable >= 3 & variable <= 4))
})

testthat::test_that("NA and Inf can both be included by call returned by get_call", {
  filter_state <- RangeFilterState$new(c(1, 8), dataname = "data", varname = "variable")
  variable <- c(NA, Inf)
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call())), c(NA, FALSE))
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_inf = TRUE, keep_na = TRUE))
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call())), c(TRUE, TRUE))
})

testthat::test_that("get_call returns valid call after unsuccessfull setting of selected", {
  filter_state <- RangeFilterState$new(7, dataname = "data", varname = "variable", keep_na = FALSE, keep_inf = FALSE)
  testthat::expect_no_error(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(1, 3)))
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(variable >= 7 & variable <= 7))
})


# format ----
testthat::test_that("format accepts numeric as indent", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_state$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format returns a string representation the FilterState object", {
  testthat::skip("temporary")
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable"))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste(
      "  Filtering on: variable",
      "    Selected range: 1.000 - 10.000",
      "    Include missing values: TRUE",
      sep = "\n"
    )
  )
})

testthat::test_that("format prepends spaces to every line of the returned string", {
  testthat::skip("temporary")
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable"))
  for (i in 0:3) {
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = i)),
      paste(format("", width = i),
        c(
          "Filtering on: variable",
          sprintf("%sSelected range: 1.000 - 10.000", format("", width = i)),
          sprintf("%sInclude missing values: TRUE", format("", width = i))
        ),
        sep = "", collapse = "\n"
      )
    )
  }
})


# is_any_filtered ----
testthat::test_that("is_any_filtered works properly when NA and Inf are present in data", {
  filter_state <- teal.slice:::RangeFilterState$new(x = c(nums, NA, Inf), varname = "variable", dataname = "data")

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE, keep_inf = FALSE))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE, keep_inf = FALSE))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE, keep_inf = TRUE))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  filter_state$set_state(filter_var(keep_na = TRUE, keep_inf = TRUE, dataname = "data", varname = "variable"))
  testthat::expect_false(shiny::isolate(filter_state$is_any_filtered()))

  filter_state$set_state(
    filter_var(dataname = "data", varname = "variable", selected = c(2, 10), keep_na = TRUE, keep_inf = TRUE)
  )
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))
})

testthat::test_that("is_any_filtered returns TRUE when enabled and FALSE when disabled", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = RangeFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(c(1:10, Inf), dataname = "data", varname = "variable")
  fs$set_state(filter_var(dataname = "data", varname = "variable", selected = c(4, 5), keep_inf = TRUE))
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$disable())
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$enable())
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
})

testthat::test_that("is_any_filtered reacts to choices", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = RangeFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(c(nums, Inf), dataname = "data", varname = "variable", keep_inf = TRUE)
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
  fs <- testfs$new(c(nums, Inf), dataname = "data", varname = "variable", choices = c(1, 9))
  testthat::expect_true(fs$is_any_filtered())
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
  filter_state <- test_class$new(pretty_sepal_length, dataname = "data", varname = "variable")
  step <- filter_state$test_get_pretty_range_step(pretty_sepal_length)
  testthat::expect_identical(step, 0.05)

  pretty_mpg <- pretty(mtcars$mpg, n = 100)

  filter_state <- test_class$new(pretty_mpg, dataname = "data", varname = "variable")
  step <- filter_state$test_get_pretty_range_step(pretty_mpg)
  testthat::expect_identical(step, 0.2)
})
