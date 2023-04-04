# initialize ----
testthat::test_that("constructor checks arguments", {
  testthat::expect_error(FilterState$new(), "argument \"dataname\" is missing")
  testthat::expect_error(FilterState$new(dataname = "data"), "argument \"varname\" is missing")
  testthat::expect_error(FilterState$new(dataname = "data", varname = "test"), "argument \"x\" is missing")

  testthat::expect_error(
    FilterState$new(7, dataname = quote(pi), varname = "test"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    FilterState$new(7, dataname = call("data"), varname = "test"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    FilterState$new(7, dataname = "data", varname = quote(pi)),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    FilterState$new(7, dataname = "data", varname = call("test")),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "test", x_reactive = NULL),
    "Assertion on 'x_reactive' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "test", keep_na = "TRUE"),
    "Assertion on 'keep_na' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "test", keep_inf = "TRUE"),
    "Assertion on 'keep_inf' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "test", fixed = NULL),
    "Assertion on 'fixed' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "test", disabled = NULL),
    "Assertion on 'disabled' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "test", extract_type = "other"),
    "Assertion on 'extract_type' failed"
  )
})

# set_state ----
testthat::test_that("set_state can set mutable fields", {
  fs <- FilterState$new(x = 7, dataname = "data", varname = "test")
  state <- filter_var("data", "varname", selected = 7)
  testthat::expect_no_error(shiny::isolate(fs$set_state(state)))
  state <- filter_var("data", "varname", keep_na = TRUE)
  testthat::expect_no_error(shiny::isolate(fs$set_state(state)))
  state <- filter_var("data", "varname", keep_inf = TRUE)
  testthat::expect_no_error(shiny::isolate(fs$set_state(state)))
  state <- filter_var("data", "varname", disabled = TRUE)
  testthat::expect_no_error(shiny::isolate(fs$set_state(state)))
})

# get_state ----
testthat::test_that("get_state returns a `teal_slice` identical to set_state input", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "var")
  state <- filter_var(dataname = "data", varname = "var", selected = "a", keep_na = TRUE, keep_inf = FALSE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

# set_state, ctd. ----
testthat::test_that("set_state cannot set mutable fields in a fixed FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "var", fixed = TRUE)
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "var",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disabled = FALSE
  )
  filter_state$set_state(new_state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), old_state)
})

testthat::test_that("set_state cannot set mutable fields in a disabled FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "var", disabled = TRUE)
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "var",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disabled = TRUE
  )
  shiny::isolate(filter_state$set_state(new_state))
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), old_state)
})

testthat::test_that("set_state can enable FilerState and set mutable fields in a disabled FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "var", disabled = TRUE)
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "var",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disabled = FALSE
  )
  filter_state$set_state(new_state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), new_state)
})



# get_state: dataname ----
testthat::test_that("get_state returns the dataname passed to constructor", {
  testthat::expect_error(FilterState$new("7", dataname = 7, varname = "7"), "Assertion on 'dataname' failed")
  testthat::expect_no_error(FilterState$new("7", dataname = "data", varname = "7"))

  filter_state <- FilterState$new("7", dataname = "data", varname = "7")
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$dataname), "data")
})

# get_state: varname ----
testthat::test_that("get_state returns varname passed to constructor", {
  testthat::expect_error(FilterState$new(7, dataname = "data", varname = 7), "Assertion on 'varname' failed")
  testthat::expect_no_error(FilterState$new(7, dataname = "data", varname = "7"))

  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_equal(shiny::isolate(filter_state$get_state()$varname), "7")
})

# set_selected ----
testthat::test_that("set_state sets value, get_selected returns the same", {
  filter_state <- FilterState$new(7L, dataname = "data", varname = "7")
  filter_state$set_state(filter_var(dataname = "data", varname = "7", selected = 7L))
  testthat::expect_identical(
    shiny::isolate(filter_state$get_state()),
    filter_var(dataname = "data", varname = "7", selected = 7L)
  )
})

# initialize, ctd. ----
testthat::test_that("constructor initializes selected = NULL by default", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_state()$selected))
})

testthat::test_that("constructor initializes keep_na = NULL by default", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_state())$keep_na)
})

testthat::test_that("constructor initializes keep_inf = NULL by default", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_state())$keep_inf)
})



# get_call ----
testthat::test_that("get_call reaises error", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_error(filter_state$get_call(), "this is a virtual method")
})

# format ----
testthat::test_that("$format() is a FilterStates's method that accepts indent", {
  filter_state <- FilterState$new(c(7), dataname = "data", varname = "test")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
})

testthat::test_that("$format() asserts that indent is numeric", {
  filter_state <- FilterState$new(c(7), dataname = "data", varname = "test")
  testthat::expect_error(
    filter_state$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() returns a string representation the FilterState object", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- FilterState$new(values, dataname = "data", varname = "test")
  filter_state$set_state(filter_var(dataname = "data", varname = "test", selected = values, keep_na = FALSE))
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
  filter_state <- FilterState$new(values, dataname = "data", varname = "test")
  filter_state$set_state(filter_var(dataname = "data", varname = "test", selected = values, keep_na = FALSE))
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
  filter_state <- FilterState$new(values, dataname = "data", varname = "test")
  filter_state$set_state(filter_var(selected = values, dataname = "data", varname = "test"))
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
  filter_state <- FilterState$new(values, dataname = "data", varname = "test")
  filter_state$set_state(filter_var(selected = values, dataname = "data", varname = "test"))
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
    list(fs$get_state()$selected, fs$get_state()$keep_na),
    list(NULL, NULL)
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
  fs$set_state(filter_var(selected = c(4, 5), keep_na = TRUE, varname = "x", dataname = "data"))
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
  fs$set_state(filter_var(selected = c(4, 5), keep_na = TRUE, varname = "x", dataname = "data"))
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
  fs$set_state(filter_var(selected = c(4, 5), keep_na = TRUE, varname = "x", dataname = "data"))
  fs$disable()
  fs$enable()
  testthat::expect_null(fs$get_cache())
})



# private methods ----
# add_keep_na_call ----
testthat::test_that("add_keep_na_call does not add anything by default", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = FilterState,
    public = list(
      test_add_keep_na_call = function() {
        private$add_keep_na_call(TRUE)
      }
    )
  )
  filter_state <- test_class$new(c(1, NA), dataname = "data", varname = "test")
  testthat::expect_identical(
    shiny::isolate(filter_state$test_add_keep_na_call()),
    quote(TRUE)
  )
})

testthat::test_that("add_keep_na_call adds `is.na` when `keep_na` is set", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = FilterState,
    public = list(
      test_add_keep_na_call = function() {
        private$add_keep_na_call(TRUE)
      }
    )
  )
  filter_state <- test_class$new(c(1, NA), dataname = "data", varname = "test")
  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "test", keep_na = TRUE)))

  testthat::expect_identical(
    shiny::isolate(filter_state$test_add_keep_na_call()),
    quote(is.na(test) | TRUE)
  )
})

testthat::test_that("Setting private$na_rm to TRUE adds `!is.na` before condition via add_keep_na_call", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = FilterState,
    public = list(
      test_add_keep_na_call = function() {
        private$add_keep_na_call(TRUE)
      },
      set_na_rm = function(value) {
        checkmate::assert_flag(value)
        private$na_rm <- value
        invisible(NULL)
      }
    )
  )
  filter_state <- test_class$new(c(1, NA), dataname = "data", varname = "test")
  filter_state$set_na_rm(TRUE)

  testthat::expect_identical(
    shiny::isolate(filter_state$test_add_keep_na_call()),
    quote(!is.na(test) & TRUE)
  )
})

testthat::test_that(
  "Setting private$na_rm to TRUE doesn't add `!is.na` before condition via add_keep_na_call
  when variable has no NAs", {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        },
        set_na_rm = function(value) {
          checkmate::assert_flag(value)
          private$na_rm <- value
          invisible(NULL)
        }
      )
    )
    filter_state <- test_class$new(c(1), dataname = "data", varname = "test")
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  })

# private$get_varlabel ----
testthat::test_that(
  "private$get_varlabel returns a string if x has the label attribute different to varname", {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        get_varlabel = function() {
          private$varlabel
        }
      )
    )

    seven <- c(7)
    filter_state <- test_class$new(seven, dataname = "data", varname = "7")
    testthat::expect_equal(shiny::isolate(filter_state$get_varlabel()), character(0))

    seven <- c(7)
    attr(seven, "label") <- "7"
    filter_state <- test_class$new(seven, dataname = "data", varname = "7")
    testthat::expect_equal(shiny::isolate(filter_state$get_varlabel()), character(0))

    attr(seven, "label") <- "test"
    filter_state <- test_class$new(seven, dataname = "data", varname = "7")
    testthat::expect_equal(shiny::isolate(filter_state$get_varlabel()), "test")
  })
