# initialize ----
testthat::test_that("The constructor accepts character as varname", {
  testthat::expect_no_error(FilterState$new(c(7), varname = "test", dataname = "data"))
  testthat::expect_error(FilterState$new(c(7), varname = quote(pi), dataname = "data"))
  testthat::expect_error(FilterState$new(c(7), varname = call("test")))
})

testthat::test_that("The constructor requires a varname", {
  testthat::expect_error(FilterState$new(c(7), dataname = "data"), regexp = "argument \"varname\" is missing")
})

testthat::test_that("The constructor requires a dataname", {
  testthat::expect_error(FilterState$new(c(7), varname = "data"), regexp = "argument \"dataname\" is missing")
})

testthat::test_that("The constructor accepts a string as varlabel", {
  testthat::expect_no_error(FilterState$new(c(7), varname = "test", varlabel = "test", dataname = "data"))
})

# get_call ----
testthat::test_that("get_call returns NULL", {
  filter_state <- FilterState$new(7, varname = "7", dataname = "data")
  testthat::expect_null(filter_state$get_call())
})

# get_varlabel ----
testthat::test_that("get_varlabel returns a string passed to the constructor", {
  filter_state <- FilterState$new(7, varname = "7", varlabel = "test", dataname = "data")
  testthat::expect_equal(filter_state$get_varlabel(), "test")
})

# get_varname ----
testthat::test_that("get_varname() returns a name if varname passed to the constructor is a string", {
  filter_state <- FilterState$new(7, varname = "7", dataname = "data")
  testthat::expect_equal(filter_state$get_varname(), "7")
})

testthat::test_that("get_varname() returns a string if varname passed to the constructor is a string", {
  filter_state <- FilterState$new(7, varname = "7", dataname = "data")
  testthat::expect_equal(filter_state$get_varname(), "7")
})

# get_get_selected ----
testthat::test_that("get_selected returns NULL after initialization", {
  filter_state <- FilterState$new(7, varname = "7", dataname = "data")
  testthat::expect_null(shiny::isolate(filter_state$get_selected()))
})

# set_selected ----
testthat::test_that("set_selected sets value, get_selected returns the same", {
  filter_state <- FilterState$new(7L, varname = "7", dataname = "data")
  filter_state$set_selected(7L)
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), 7L)
})

# get_keep_na ----
testthat::test_that("get_keep_na returns FALSE after initialization", {
  filter_state <- FilterState$new(7, varname = "7", dataname = "data")
  testthat::expect_false(shiny::isolate(filter_state$get_keep_na()))
})

# set_state ----
testthat::test_that("set_state sets selected and keep_na", {
  filter_state <- FilterState$new(c("a", NA_character_), varname = "var", dataname = "data")
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

# get_state ----
testthat::test_that("get_state returns a list identical to set_state input", {
  filter_state <- FilterState$new(c("a", NA_character_), varname = "var", dataname = "data")
  state <- list(selected = "a", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

# add_keep_na_call ----
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
    filter_state <- test_class$new(c(1, NA), varname = "test", dataname = "data")
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
    filter_state <- test_class$new(c(1, NA), varname = "test", dataname = "data")
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
    filter_state <- test_class$new(c(1, NA), varname = "test", dataname = "data")
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
    filter_state <- test_class$new(c(1), varname = "test", dataname = "data")
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  }
)

# format ----
testthat::test_that("$format() is a FilterStates's method that accepts indent", {
  filter_state <- FilterState$new(c(7), varname = "test", dataname = "data")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
})

testthat::test_that("$format() asserts that indent is numeric", {
  filter_state <- FilterState$new(c(7), varname = "test", dataname = "data")
  testthat::expect_error(
    filter_state$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() returns a string representation the FilterState object", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- FilterState$new(values, varname = "test", dataname = "data")
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
  filter_state <- FilterState$new(values, varname = "test", dataname = "data")
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
  filter_state <- FilterState$new(values, varname = "test", dataname = "data")
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
  filter_state <- FilterState$new(values, varname = "test", dataname = "data")
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

testthat::test_that("disable sets all state elements to NULL", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = FilterState,
    public = list(
      disable = function() private$disable()
    )
  )
  fs <- testfs$new(c(1:10, NA), varname = "x", dataname = "data")
  fs$disable()
  testthat::expect_equal(
    fs$get_state(),
    list(selected = NULL, keep_na = NULL)
  )
})

testthat::test_that("disable copies last state to the cache", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = FilterState,
    public = list(
      disable = function() private$disable(),
      get_cache = function() private$cache
    )
  )
  fs <- testfs$new(c(1:10, NA), varname = "x", dataname = "data")
  fs$set_state(list(selected = c(4, 5), keep_na = TRUE))
  last_state <- fs$get_state()
  fs$disable()
  testthat::expect_identical(fs$get_cache(), last_state)
})

testthat::test_that("enable sets state back to the last state", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = FilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(c(1:10, NA), varname = "x", dataname = "data")
  fs$set_state(list(selected = c(4, 5), keep_na = TRUE))
  last_state <- fs$get_state()
  fs$disable()
  fs$enable()
  testthat::expect_equal(fs$get_state(), last_state)
})

testthat::test_that("enable clears cache", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = FilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable(),
      get_cache = function() private$cache
    )
  )
  fs <- testfs$new(c(1:10, NA), varname = "x", dataname = "data")
  fs$set_state(list(selected = c(4, 5), keep_na = TRUE))
  fs$disable()
  fs$enable()
  testthat::expect_null(fs$get_cache())
})
