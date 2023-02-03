testthat::test_that("The constructor accepts character, name or call as varname", {
  testthat::expect_no_error(FilterState$new(c(7), varname = "test"))
  testthat::expect_no_error(FilterState$new(c(7), varname = quote(pi)))
  testthat::expect_no_error(FilterState$new(c(7), varname = call("test")))
})

testthat::test_that("The constructor requires a varname", {
  testthat::expect_error(FilterState$new(c(7)), regexp = "argument \"varname\" is missing")
})

testthat::test_that("The constructor accepts a string as varlabel", {
  testthat::expect_no_error(FilterState$new(c(7), varname = "test", varlabel = "test"))
})

testthat::test_that("get_call returns NULL", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_null(filter_state$get_call())
})

test_that("dataname must be specified if extract_type is specified", {
  testthat::expect_error(
    FilterState$new(
      c("F", "M"),
      varname = "SEX",
      dataname = NULL,
      extract_type = "matrix"
    ),
    regexp = "if extract_type is specified, dataname must also be specified"
  )
})

testthat::test_that("get_dataname returns a string when dataname is NULL", {
  filter_state <- FilterState$new(7, varname = "7", dataname = NULL)
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "NULL")
})

testthat::test_that("get_varlabel returns a string passed to the constructor", {
  filter_state <- FilterState$new(7, varname = "7", varlabel = "test")
  testthat::expect_equal(filter_state$get_varlabel(), "test")
})

testthat::test_that("get_varname(deparse = FALSE) returns a name if varname passed to the constructor is a string", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_equal(filter_state$get_varname(deparse = FALSE), quote(`7`))
})

testthat::test_that("get_varname(deparse = TRUE) returns a string if varname passed to the constructor is a string", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_equal(filter_state$get_varname(deparse = TRUE), "7")
})

testthat::test_that("get_varname returns a call if call is passed to the constructor", {
  filter_state <- FilterState$new(7, varname = call("test"))
  testthat::expect_equal(filter_state$get_varname(deparse = FALSE), call("test"))
  testthat::expect_equal(filter_state$get_varname(deparse = TRUE), "test()")
})

testthat::test_that("get_selected returns NULL after initialization", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_selected()))
})

testthat::test_that("set_selected sets value, get_selected returns the same", {
  filter_state <- FilterState$new(7L, varname = "7")
  filter_state$set_selected(7L)
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), 7L)
})

testthat::test_that("get_keep_na returns FALSE after initialization", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_false(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state sets selected and keep_na", {
  filter_state <- FilterState$new(c("a", NA_character_), varname = "var")
  state <- list(selected = "a", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(
    state,
      list(
        selected = shiny::isolate(filter_state$get_selected()),
        keep_na = shiny::isolate(filter_state$get_keep_na())
      )
  )
})

testthat::test_that("get_state returns a list identical to set_state input", {
  filter_state <- FilterState$new(c("a", NA_character_), varname = "var")
  state <- list(selected = "a", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

testthat::test_that(
  "add_keep_na_call does not add anything by default",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1, NA), varname = "test")
    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  }
)

testthat::test_that(
  "add_keep_na_call adds `is.na` when `keep_na` is set",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1, NA), varname = "test")
    shiny::isolate(filter_state$set_keep_na(TRUE))

    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(is.na(test) | TRUE)
    )
  }
)

testthat::test_that(
  "Setting private$na_rm to TRUE adds `!is.na` before condition via add_keep_na_call",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1, NA), varname = "test")
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(!is.na(test) & TRUE)
    )
  }
)

testthat::test_that(
  "Setting private$na_rm to TRUE doesn't add `!is.na` before condition via add_keep_na_call
  when variable has no NAs",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1), varname = "test")
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  }
)

# Format
testthat::test_that("$format() is a FilterStates's method that accepts indent", {
  testthat::expect_no_error(shiny::isolate(FilterState$new(c(7), varname = "test")$format(indent = 0)))
})

testthat::test_that("$format() asserts that indent is numeric", {
  testthat::expect_error(
    FilterState$new(c(7), varname = "test")$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() returns a string representation the FilterState object", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- FilterState$new(values, varname = "test")
  filter_state$set_state(list(selected = values))
  testthat::expect_equal(
    shiny::isolate(filter_state$format(indent = 0)),
    paste(
      "Filtering on: test",
      "  Selected values: value_1, value_2, value_3",
      "  Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("$format() prepends spaces to every line of the returned string", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- FilterState$new(values, varname = "test")
  filter_state$set_state(list(selected = values))
  for (i in 1:3) {
    whitespace_indent <- paste0(rep(" ", i), collapse = "")
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = i)),
      paste(format("", width = i),
        c(
          "Filtering on: test",
          "  Selected values: value_1, value_2, value_3",
          "  Include missing values: FALSE"
        ),
        sep = "", collapse = "\n"
      )
    )
  }
})

testthat::test_that("$format() returns a properly wrapped string", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- FilterState$new(values, varname = "test")
  filter_state$set_state(list(selected = values))
  line_width <- 76L # arbitrary value given in method body
  manual <- 4L # manual third order indent given in method body
  for (i in 1:10) {
    output <- shiny::isolate(filter_state$format(indent = i))
    captured <- utils::capture.output(cat(output))
    line_lengths <- vapply(captured, nchar, integer(1L))
    testthat::expect_lte(max(line_lengths), line_width + i + manual)
  }
})

testthat::test_that("$format() line wrapping breaks if strings are too long", {
  values <- c("exceedinglylongvaluenameexample", "exceedingly long value name example with spaces")
  filter_state <- FilterState$new(values, varname = "test")
  filter_state$set_state(list(selected = values))
  manual <- 4L # manual third order indent given in method body
  linewidth <- 30L
  output <- shiny::isolate(filter_state$format(indent = 2, wrap_width = linewidth))
  captured <- utils::capture.output(cat(output))
  line_lengths <- vapply(captured, nchar, integer(1L))
  testthat::expect_failure(
    testthat::expect_lte(max(line_lengths), 2 + manual + linewidth)
  )
  expect_error(
    shiny::isolate(filter_state$format(indent = 2, wrap_width = 10)),
    "[Aa]ssertion.+failed"
  )
})

# bug fix #41
testthat::test_that("private$get_pretty_range_step returns pretty step size", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = RangeFilterState,
    public = list(
      test_get_pretty_range_step = function(min, max, pretty_range) {
        private$get_pretty_range_step(min, max, pretty_range)
      }
    )
  )

  pretty_sepal_length <- pretty(iris$Sepal.Length, n = 100)

  filter_state <- test_class$new(pretty_sepal_length, varname = "test")
  step <- filter_state$test_get_pretty_range_step(
    min(pretty_sepal_length),
    max(pretty_sepal_length),
    pretty_sepal_length
  )
  testthat::expect_identical(step, 0.05)

  pretty_mpg <- pretty(mtcars$mpg, n = 100)

  filter_state <- test_class$new(pretty_mpg, varname = "test")
  step <- filter_state$test_get_pretty_range_step(
    min(pretty_mpg),
    max(pretty_mpg),
    pretty_mpg
  )
  testthat::expect_identical(step, 0.2)
})

testthat::test_that("private$get_pretty_range_inputs returns nicely rounded values", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = RangeFilterState,
    public = list(
      test_get_pretty_range_inputs = function(values) {
        private$get_pretty_range_inputs(values)
      }
    )
  )

  filter_state <- test_class$new(iris$Sepal.Length, varname = "test")
  pretty_vals <- filter_state$test_get_pretty_range_inputs(iris$Sepal.Length)
  expected_vals <- c(min = 4.30, max = 7.90, step = 0.05)
  testthat::expect_equal(pretty_vals, expected_vals, tolerance = 0.01)

  filter_state <- test_class$new(mtcars$mpg, varname = "test")
  pretty_vals <- filter_state$test_get_pretty_range_inputs(mtcars$mpg)
  expected_vals <- c(min = 10.4, max = 34, step = 0.2)
  testthat::expect_identical(pretty_vals, expected_vals)
})
