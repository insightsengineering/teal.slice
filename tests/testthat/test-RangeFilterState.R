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
  skip("no longer necessary(?)")
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


# set_state ----
testthat::test_that("set_state: selected accepts numeric vector of length 2", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  testthat::expect_no_error(
    filter_state$set_state(filter_var(selected = nums[c(1, 10)], dataname = "data", varname = "variable"))
  )
  testthat::expect_error(
    filter_state$set_state(filter_var(selected = nums[1], dataname = "data", varname = "variable")),
    "The array of set values must have length two"
  )

  testthat::expect_error(
    suppressWarnings(
      filter_state$set_state(filter_var(selected = c("a", "b"), dataname = "data", varname = "variable"))
    ),
    "The array of set values must contain values coercible to numeric"
  )
})

testthat::test_that("set_state: selected raises `logger` warning when selection is not within the possible range", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")

  skip("no longer guarded")
  testthat::expect_output(
    filter_state$set_state(
      filter_var(nums, dataname = "data", varname = "variable", selected = c(nums[1] - 1, nums[10]))
    ),
    "Programmatic range specification"
  )
  testthat::expect_output(
    filter_state$set_state(
      filter_var(nums, dataname = "data", varname = "variable", selected = c(nums[1], nums[10] + 1))
    ),
    "Programmatic range"
  )
  testthat::expect_output(
    filter_state$set_state(
      filter_var(nums, dataname = "data", varname = "variable", selected = c(nums[1] - 1, nums[10] + 1))
    ),
    "Programmatic range"
  )
})

testthat::test_that("set_state: selected range is limited to lower and upper bound of possible range", {
  skip("hmm")
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  testthat::expect_output(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c(nums[1] - 1, nums[10]))
    ),
    "Programmatic range"
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(nums[1], nums[10]))
  testthat::expect_output(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c(nums[1], nums[10] + 1))
    ),
    "Programmatic range"
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(nums[1], nums[10]))
  testthat::expect_output(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c(nums[1] - 1, nums[10] + 1))
    ),
    "Programmatic range"
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$selected), c(nums[1], nums[10]))
})

testthat::test_that("set_state: selected raises error when selection is not coercible to numeric", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  testthat::expect_error(
    suppressWarnings(
      filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("a", "b")))
    ),
    "The array of set values must contain values coercible to numeric"
  )
})


# get_call ----
testthat::test_that("method get_call of default RangeFilterState object returns NULL", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "nums")
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable", selected = nums[c(1, 3)])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= 1 & variable <= 3)
  )
})

testthat::test_that("get_call returns NULL if disabled", {
  filter_state <- RangeFilterState$new(
    nums,
    dataname = "data", varname = "variable", selected = nums[c(1, 3)], disabled = TRUE
  )
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- RangeFilterState$new(
    nums,
    dataname = "data", varname = "variable",
    choices = nums[c(1, 3)], selected = nums[c(1, 3)]
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= 1 & variable <= 3)
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- RangeFilterState$new(
    nums,
    dataname = "data", varname = "variable", selected = nums[c(1, 3)], extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname$variable >= 1 & dataname$variable <= 3)
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- RangeFilterState$new(
    nums,
    dataname = "data", varname = "variable", selected = nums[c(1, 3)], extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname[, "variable"] >= 1 & dataname[, "variable"] <= 3)
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- RangeFilterState$new(
    c(nums, NA),
    dataname = "data", varname = "variable", selected = nums[c(1, 3)], keep_na = TRUE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | variable >= 1 & variable <= 3)
  )
})

testthat::test_that("get_call returns call if all selected but NA exists", {
  filter_state <- RangeFilterState$new(
    c(nums, NA),
    dataname = "data", varname = "variable", keep_na = FALSE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable >= 1 & variable <= 10)
  )
})

# format ----
testthat::test_that("format accepts logical show_all", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
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
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable"))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "RangeFilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "RangeFilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  filter_state <- RangeFilterState$new(nums, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable"))
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print())),
    c(
      "RangeFilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()))),
      " "
    )
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c(
      "RangeFilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)),
      " "
    )
  )
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
