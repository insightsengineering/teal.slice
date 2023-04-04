logs <- as.logical(c(rbinom(10, 1, 0.5), NA))

testthat::test_that("The constructor accepts logical values", {
  testthat::expect_no_error(
    LogicalFilterState$new(x = c(TRUE), x_reactive = reactive(NULL), varname = "test", dataname = "data"))
})

testthat::test_that("The constructor accepts NA values", {
  testthat::expect_no_error(
    LogicalFilterState$new(x = c(TRUE, NA), x_reactive = reactive(NULL), varname = "test", dataname = "data"))
})

testthat::test_that("get_call returns FALSE values from data passed to selector", {
  filter_state <- LogicalFilterState$new(
    logs, x_reactive = reactive(NULL), varname = "logs", dataname = "data", selected = FALSE
  )
  expect_identical(
    eval(shiny::isolate(filter_state$get_call())),
    !logs
  )
})
filter_state$set_state(filter_var(selected = FALSE, varname = "logs", dataname = "data" ))


testthat::test_that("set_state accepts a logical of length 1 as selected", {
  filter_state <- LogicalFilterState$new(logs, x_reactive = reactive(NULL), varname = "logs", dataname = "data")
  testthat::expect_no_error(filter_state$set_state(filter_var(selected = TRUE, varname = "logs", dataname = "data")))
  testthat::expect_no_error(filter_state$set_state(filter_var(selected = FALSE, varname = "logs", dataname = "data")))
  testthat::expect_error(
    filter_state$set_state(filter_var(selected = c(TRUE, TRUE), varname = "logs", dataname = "data"),
                           "should be a logical scalar")
  )
})

testthat::test_that("set_state accepts a non-logical coercible to logical of length 1 as selected", {
  filter_state <- LogicalFilterState$new(logs, x_reactive = reactive(NULL), varname = "logs", dataname = "data")
  testthat::expect_no_error(filter_state$set_state(filter_var(selected = "TRUE", varname = "logs", dataname = "data")))
  testthat::expect_no_error(filter_state$set_state(filter_var(selected = "FALSE", varname = "logs", dataname = "data")))
  testthat::expect_error(
    filter_state$set_state(filter_var(selected = c("TRUE", "TRUE"), varname = "logs", dataname = "data"),
                           "should be a logical scalar")
  )
})

testthat::test_that("get_call returns appropriate call depending on selection state", {
  filter_state <- LogicalFilterState$new(
    logs, x_reactive = reactive(NULL), varname = "logs", dataname = "data", selected = FALSE
  )
  expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!logs)
  )
  filter_state$set_state(filter_var(selected = TRUE, varname = "logs", dataname = "data"))
  expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(logs)
  )
  filter_state$set_state(filter_var(keep_na = TRUE, varname = "logs", dataname = "data"))
  expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(logs) | logs)
  )
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  filter_state <- LogicalFilterState$new(
    x = c(TRUE, FALSE, NA), x_reactive = reactive(NULL), varname = "test", dataname = "data")
  filter_state$set_state(filter_var(selected = FALSE, keep_na = TRUE, varname = "test", dataname = "data"))
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), FALSE)
  testthat::expect_true(shiny::isolate(filter_state$get_state()$keep_na))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- LogicalFilterState$new(
    x = c(TRUE, FALSE, NA), x_reactive = reactive(NULL), varname = "test", dataname = "data")
  filter_state$set_state(filter_var(selected = FALSE, keep_na = TRUE, varname = "test", dataname = "data"))
  testthat::expect_no_error(filter_state$set_state(filter_var(selected = TRUE, varname = "test", dataname = "data")))
  testthat::expect_true(shiny::isolate(filter_state$get_state()$selected))
  testthat::expect_true(shiny::isolate(filter_state$get_state()$keep_na))
})

testthat::test_that(
  "LogicalFilterState$is_any_filtered works properly when NA is present in data",
  code = {
    filter_state <- teal.slice:::LogicalFilterState$new(
      x = rep(c(TRUE, NA), 10),
      x_reactive = reactive(NULL),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )
    filter_state$set_state(filter_var(selected = TRUE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

testthat::test_that(
  "LogicalFilterState$is_any_filtered works properly when both TRUE and FALSE are present",
  code = {
    filter_state <- teal.slice:::LogicalFilterState$new(
      x = rep(c(TRUE, FALSE, NA), 10),
      x_reactive = reactive(NULL),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )
    filter_state$set_state(filter_var(selected = TRUE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = TRUE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = TRUE, keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = FALSE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = FALSE, keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

testthat::test_that(
  "LogicalFilterState$is_any_filtered works properly when only TRUE or FALSE is present",
  code = {
    filter_state <- teal.slice:::LogicalFilterState$new(
      x = rep(c(TRUE, NA), 10),
      x_reactive = reactive(NULL),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )
    filter_state$set_state(filter_var(selected = TRUE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = TRUE, keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = FALSE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = FALSE, keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state <- teal.slice:::LogicalFilterState$new(
      x = rep(c(FALSE, NA), 10),
      x_reactive = reactive(NULL),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )
    filter_state$set_state(filter_var(selected = FALSE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = FALSE, keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = TRUE, keep_na = TRUE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    filter_state$set_state(filter_var(selected = TRUE, keep_na = FALSE, varname = "x", dataname = "data"))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)

testthat::test_that("is_any_filtered returns TRUE when enabled", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = LogicalFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(c(TRUE, FALSE), varname = "x", dataname = "data")
  fs$set_state(filter_var(selected = TRUE, keep_na = TRUE, varname = "x", dataname = "data"))
  fs$disable()
  fs$enable()
  testthat::expect_true(fs$is_any_filtered())
})
