# initialize ----
testthat::test_that("constructor checks arguments", {
  testthat::expect_error(FilterState$new(), "argument \"dataname\" is missing")
  testthat::expect_error(FilterState$new(dataname = "data"), "argument \"varname\" is missing")
  testthat::expect_error(FilterState$new(dataname = "data", varname = "variable"), "argument \"x\" is missing")

  testthat::expect_error(
    FilterState$new(7, dataname = quote(pi), varname = "variable"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    FilterState$new(7, dataname = call("data"), varname = "variable"),
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
    FilterState$new(x = 7, dataname = "data", varname = "variable", x_reactive = NULL),
    "Assertion on 'x_reactive' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "variable", keep_na = "TRUE"),
    "Assertion on 'keep_na' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "variable", keep_inf = "TRUE"),
    "Assertion on 'keep_inf' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "variable", disabled = NULL),
    "Assertion on 'disabled' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "variable", fixed = NULL),
    "Assertion on 'fixed' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "variable", locked = NULL),
    "Assertion on 'locked' failed"
  )
  testthat::expect_error(
    FilterState$new(x = 7, dataname = "data", varname = "variable", extract_type = "other"),
    "Assertion on 'extract_type' failed"
  )
})

# set_state ----
testthat::test_that("set_state can set mutable fields", {
  filter_state <- FilterState$new(x = 7, dataname = "data", varname = "variable")
  state <- filter_var("data", "varname", selected = 7)
  testthat::expect_no_error(shiny::isolate(filter_state$set_state(state)))
  state <- filter_var("data", "varname", keep_na = TRUE)
  testthat::expect_no_error(shiny::isolate(filter_state$set_state(state)))
  state <- filter_var("data", "varname", keep_inf = TRUE)
  testthat::expect_no_error(shiny::isolate(filter_state$set_state(state)))
  state <- filter_var("data", "varname", disabled = TRUE)
  testthat::expect_no_error(shiny::isolate(filter_state$set_state(state)))
})

# get_state ----
testthat::test_that("get_state returns a `teal_slice` object", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "variable")
  state <- shiny::isolate(filter_state$get_state())
  testthat::expect_s3_class(state, "teal_slice")
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

testthat::test_that("get_state returns a `teal_slice` identical to set_state input", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "variable")
  state <- filter_var(dataname = "data", varname = "variable", selected = "a", keep_na = TRUE, keep_inf = FALSE)
  filter_state$set_state(state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), state)
})

# set_state, ctd. ----
testthat::test_that("set_state only sets properties defined in `teal_slice", {
  filter_state <- FilterState$new(x = 7, dataname = "data", varname = "variable")
  testthat::expect_identical(
    shiny::isolate(filter_state$get_state())[c("selected", "keep_na", "keep_inf")],
    list(selected = NULL, keep_na = NULL, keep_inf = NULL)
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = 7))
  testthat::expect_identical(
    shiny::isolate(filter_state$get_state())[c("selected", "keep_na", "keep_inf")],
    list(selected = 7, keep_na = NULL, keep_inf = NULL)
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_identical(
    shiny::isolate(filter_state$get_state())[c("selected", "keep_na", "keep_inf")],
    list(selected = 7, keep_na = FALSE, keep_inf = NULL)
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_inf = FALSE))
  testthat::expect_identical(
    shiny::isolate(filter_state$get_state())[c("selected", "keep_na", "keep_inf")],
    list(selected = 7, keep_na = FALSE, keep_inf = FALSE)
  )
})

testthat::test_that("set_state cannot set mutable fields in a fixed FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "variable", fixed = TRUE)
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "variable",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disabled = FALSE
  )
  testthat::expect_output(filter_state$set_state(new_state), "WARN.+attempt to set state on fixed filter")
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), old_state)
})

testthat::test_that("set_state can set mutable fields in a locked FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "variable", locked = TRUE)
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "variable",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disable = FALSE,
    locked = TRUE
  )
  testthat::expect_output(filter_state$set_state(new_state), "WARN.+attempt to disable a locked filter aborted")
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), new_state)
})

testthat::test_that("set_state cannot set disabled field in a locked FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "variable", locked = TRUE)
  new_state <- filter_var(
    dataname = "data",
    varname = "variable",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disable = FALSE,
    locked = TRUE
  )
  testthat::expect_output(filter_state$set_state(new_state), "WARN.+attempt to disable a locked filter aborted")
})

testthat::test_that("set_state cannot set mutable fields in a disabled FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "variable")
  shiny::isolate(filter_state$set_state(filter_var("data", "variable", disabled = TRUE)))
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "variable",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disabled = TRUE
  )
  testthat::expect_output(
    shiny::isolate(filter_state$set_state(new_state)),
    "WARN.+attempt to set state on disabled filter"
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), old_state)
})

testthat::test_that("set_state can enable FilerState and set mutable fields in a disabled FilterState", {
  filter_state <- FilterState$new(c("a", NA_character_), dataname = "data", varname = "variable")
  shiny::isolate(filter_state$set_state(filter_var("data", "variable", disabled = TRUE)))
  old_state <- shiny::isolate(filter_state$get_state())
  new_state <- filter_var(
    dataname = "data",
    varname = "variable",
    selected = "a",
    keep_na = TRUE,
    keep_inf = FALSE,
    disabled = FALSE
  )
  filter_state$set_state(new_state)
  testthat::expect_identical(shiny::isolate(filter_state$get_state()), new_state)
})

testthat::test_that("set_state can disable and enable a FilterState", {
  test_class <- R6::R6Class(
    classname = "testfs",
    inherit = FilterState,
    public = list(
      is_disabled = function() private$is_disabled()
    )
  )
  filter_state <- test_class$new(c("a", NA_character_), dataname = "data", varname = "variable")
  testthat::expect_false(shiny::isolate(filter_state$is_disabled()))
  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "variable", disabled = TRUE)))
  testthat::expect_true(shiny::isolate(filter_state$is_disabled()))
  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "variable", disabled = FALSE)))
  testthat::expect_false(shiny::isolate(filter_state$is_disabled()))
})

# get_state: dataname ----
testthat::test_that("get_state returns the dataname passed to constructor", {
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

# initialize, ctd. ----
testthat::test_that("constructor initializes selected = NULL by default", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_state()$selected))
})

testthat::test_that("constructor initializes keep_na = TRUE by default if data contains NAs", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_null(shiny::isolate(filter_state$get_state())$keep_na)
  filter_state <- FilterState$new(c(7, NA), dataname = "data", varname = "7")
  testthat::expect_true(shiny::isolate(filter_state$get_state())$keep_na)
})

# get_call ----
testthat::test_that("get_call raises error", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_error(filter_state$get_call(), "this is a virtual method")
})


# PRIVATE METHODS ----
# set_selected / get_selected ----
testthat::test_that("set_selected sets value, get_selected returns the same value", {
  test_class <- R6::R6Class(
    classname = "testfs",
    inherit = FilterState,
    public = list(
      set_selected = function(value) private$set_selected(value),
      get_selected = function() private$get_selected()
    )
  )
  filter_state <- test_class$new(7L, dataname = "data", varname = "7")
  selection <- 7
  filter_state$set_selected(selection)
  testthat::expect_identical(
    shiny::isolate(filter_state$get_selected()),
    selection
  )
})

# add_keep_na_call ----
testthat::test_that("add_keep_na_call modifies call if keep_na set to TRUE", {
  test_class <- R6::R6Class(
    classname = "TestClass",
    inherit = FilterState,
    public = list(
      test_add_keep_na_call = function() {
        private$add_keep_na_call(TRUE)
      }
    )
  )
  filter_state <- test_class$new(c(1, NA), dataname = "data", varname = "variable", keep_na = FALSE)
  testthat::expect_identical(
    shiny::isolate(filter_state$test_add_keep_na_call()),
    quote(TRUE)
  )

  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE)))
  testthat::expect_identical(
    shiny::isolate(filter_state$test_add_keep_na_call()),
    quote(is.na(variable) | TRUE)
  )
})

testthat::test_that("setting private$na_rm to TRUE adds `!is.na` before condition via add_keep_na_call", {
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
  filter_state <- test_class$new(c(1, NA), dataname = "data", varname = "variable", keep_na = FALSE)
  filter_state$set_na_rm(TRUE)

  testthat::expect_identical(
    shiny::isolate(filter_state$test_add_keep_na_call()),
    quote(!is.na(variable) & TRUE)
  )
})

testthat::test_that(
  "setting private$na_rm to TRUE doesn't add `!is.na` before condition
  via add_keep_na_call when variable has no NAs",
  {
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
    filter_state <- test_class$new(c(1), dataname = "data", varname = "variable", keep_na = FALSE)
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      shiny::isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  }
)

# get_varlabel ----
testthat::test_that("get_varlabel returns a string if x has the label attribute different to varname", {
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

# format ---
testthat::test_that("format accepts logical show_all", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = TRUE)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = FALSE)))
  testthat::expect_error(shiny::isolate(filter_state$format(show_all = 1)), "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'")
  testthat::expect_error(shiny::isolate(filter_state$format(show_all = 0)), "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'")
  testthat::expect_error(shiny::isolate(filter_state$format(show_all = "TRUE")), "Assertion on 'show_all' failed")
})


testthat::test_that("format returns a properly formatted string representation", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "FilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "FilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  filter_state <- FilterState$new(7, dataname = "data", varname = "7")
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print())),
    c(
      "FilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()))),
      " "
    )
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c(
      "FilterState:",
      utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)),
      " "
    )
  )
})
