chars <- c("item1", "item2", "item3")
facts <- as.factor(c("item1", "item2", "item3"))
nums <- c(1, 2, 3)
dates <- as.Date("2000-01-01") + 0:2
posixct <- as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:2
posixlt <- as.POSIXlt(as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:2)


# constructor ----
testthat::test_that("constructor accepts all data classes", {
  testthat::expect_no_error(ChoicesFilterState$new(chars, dataname = "data", varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(facts, dataname = "data", varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(nums, dataname = "data", varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(dates, dataname = "data", varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(posixct, dataname = "data", varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(posixlt, dataname = "data", varname = "variable"))
})

testthat::test_that("constructor raises warning if choices out of range", {
  testthat::expect_warning(
    ChoicesFilterState$new(chars, dataname = "data", varname = "variable", choices = c(chars, "item4")),
    "Some of the choices not within variable values, adjusting"
  )
})

testthat::test_that("constructor raises warning if choices out of range", {
  testthat::expect_warning(
    ChoicesFilterState$new(chars, dataname = "data", varname = "variable", choices = "item4"),
    "Some of the choices not within variable values, adjusting"
  )
  testthat::expect_warning(
    ChoicesFilterState$new(chars, dataname = "data", varname = "variable", choices = "item4"),
    "Invalid choices: none of them within the values in the variable"
  )
})

testthat::test_that("constructor raises warning if selected out of range", {
  testthat::expect_warning(
    ChoicesFilterState$new(chars, dataname = "data", varname = "variable", selected = c(chars, "item4")),
    "not in choices"
  )
  testthat::expect_warning(
    ChoicesFilterState$new(chars, dataname = "data", varname = "variable", selected = "item4"),
    "not in choices"
  )
})

# get_call ----
testthat::test_that("get_call returns call that evaluated leaves all values passed to constructor", {
  filter_state <- ChoicesFilterState$new(chars, dataname = "data", varname = "variable")
  variable <- chars
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(facts, dataname = "data", varname = "variable")
  variable <- facts
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(nums, dataname = "data", varname = "variable")
  variable <- nums
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(dates, dataname = "data", varname = "variable")
  variable <- dates
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(posixct, dataname = "data", varname = "variable")
  variable <- posixct
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(posixlt, dataname = "data", varname = "variable")
  variable <- posixlt
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})


testthat::test_that("get_call returns condition that specifies values passed to set_selected", {
  filter_state <- ChoicesFilterState$new(
    chars,
    dataname = "data", varname = "variable", selected = chars[1]
  )
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == "item1"))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = chars[1:2]))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% c("item1", "item2")))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% c("item1", "item2")))
  )

  filter_state <- ChoicesFilterState$new(
    facts,
    dataname = "data", varname = "variable", selected = facts[1]
  )
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == "item1"))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = facts[1:2]))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% c("item1", "item2")))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% c("item1", "item2")))
  )

  filter_state <- ChoicesFilterState$new(
    nums,
    dataname = "data", varname = "variable", selected = nums[1]
  )
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == 1))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = nums[1:2]))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% c(1, 2)))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% c(1, 2)))
  )

  filter_state <- ChoicesFilterState$new(
    dates,
    dataname = "data", varname = "variable", selected = dates[1]
  )
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == as.Date("2000-01-01")))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = dates[1:2]))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% as.Date(c("2000-01-01", "2000-01-02"))))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% as.Date(c("2000-01-01", "2000-01-02"))))
  )

  filter_state <- ChoicesFilterState$new(
    posixct,
    dataname = "data", varname = "variable", selected = posixct[1]
  )
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == as.POSIXct("2000-01-01 12:00:00", tz = "GMT")))
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[1:2]))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% as.POSIXct(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT")))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(
      quote(is.na(variable) |
        variable %in% as.POSIXct(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT"))
    )
  )

  filter_state <- ChoicesFilterState$new(
    posixlt,
    dataname = "data", varname = "variable", selected = posixlt[1]
  )
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == as.POSIXlt("2000-01-01 12:00:00", tz = "GMT")))
  )

  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixlt[1:2]))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% as.POSIXlt(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT")))
  )
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", keep_na = TRUE))
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(
      quote(is.na(variable) |
        variable %in% as.POSIXlt(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT"))
    )
  )
})

# set_state ----
testthat::test_that("set_state raises warning when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new(chars, dataname = "data", varname = "variable")
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "item4")),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("item1", "item4"))),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(facts, dataname = "data", varname = "variable")
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = "item4")),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c("item1", "item4"))
    ),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(nums, dataname = "data", varname = "variable")
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = 4)),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c(1, 4))
    ),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = dates[3] + 1)),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c(dates[1], dates[3] + 1))
    ),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_warning(
    filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = posixct[3] + 1)),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      filter_var(dataname = "data", varname = "variable", selected = c(posixct[1], posixct[3] + 1))
    ),
    "not in choices"
  )
})

testthat::test_that("set_statre sets the intersection of choices and the passed values", {
  filter_state <- ChoicesFilterState$new(chars, dataname = "data", varname = "variable")
  suppressWarnings(
    shiny::isolate(
      filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("item1", "item4")))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state())$selected, "item1")

  filter_state <- ChoicesFilterState$new(facts, dataname = "data", varname = "variable")
  suppressWarnings(
    shiny::isolate(
      filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c("item1", "item4")))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "item1")

  filter_state <- ChoicesFilterState$new(nums, dataname = "data", varname = "variable")
  suppressWarnings(
    shiny::isolate(
      filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(1, 4)))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "1")

  filter_state <- ChoicesFilterState$new(dates, dataname = "data", varname = "variable")
  testthat::expect_warning(
    shiny::isolate(
      filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = c(dates[1], dates[3] + 1)))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "2000-01-01")

  filter_state <- ChoicesFilterState$new(posixct, dataname = "data", varname = "variable")
  testthat::expect_warning(
    shiny::isolate(
      filter_state$set_state(
        filter_var(selected = c(posixct[1], posixct[3] + 1), dataname = "data", varname = "variable")
      )
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "2000-01-01 12:00:00")

  filter_state <- ChoicesFilterState$new(posixlt, dataname = "data", varname = "variable")
  testthat::expect_warning(
    shiny::isolate(
      filter_state$set_state(
        filter_var(selected = as.POSIXlt(c(posixlt[1], posixlt[3] + 1)), dataname = "data", varname = "variable")
      )
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "2000-01-01 12:00:00")
})


# format ----
testthat::test_that("format accepts numeric as indent", {
  filter_state <- ChoicesFilterState$new(7, dataname = "data", varname = "variable")
  testthat::expect_no_error(shiny::isolate(filter_state$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_state$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format returns properly formatted string representation", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- ChoicesFilterState$new(values, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = values, keep_na = FALSE))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste(
      "  Filtering on: variable",
      "    Selected values: value_1, value_2, value_3",
      "    Include missing values: FALSE",
      sep = "\n"
    )
  )
})

testthat::test_that("format prepends spaces to every line of the returned string", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- ChoicesFilterState$new(values, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(dataname = "data", varname = "variable", selected = values, keep_na = FALSE))
  for (i in 0:3) {
    testthat::expect_equal(
      shiny::isolate(filter_state$format(indent = i)),
      paste(format("", width = i),
        c(
          "Filtering on: variable",
          sprintf("%sSelected values: value_1, value_2, value_3", format("", width = i)),
          sprintf("%sInclude missing values: FALSE", format("", width = i))
        ),
        sep = "", collapse = "\n"
      )
    )
  }
})

testthat::test_that("format returns a properly wrapped string", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- ChoicesFilterState$new(values, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(selected = values, dataname = "data", varname = "variable"))
  line_width <- 76L # arbitrary value given in method body
  manual <- 4L # manual third order indent given in method body
  for (i in 1:10) {
    output <- shiny::isolate(filter_state$format(indent = i))
    captured <- utils::capture.output(cat(output))
    line_lengths <- vapply(captured, nchar, integer(1L))
    testthat::expect_lte(max(line_lengths), line_width + i + manual)
  }
})

testthat::test_that("format line wrapping breaks if strings are too long", {
  values <- c("exceedinglylongvaluenameexample", "exceedingly long value name example with spaces")
  filter_state <- ChoicesFilterState$new(values, dataname = "data", varname = "variable")
  filter_state$set_state(filter_var(selected = values, dataname = "data", varname = "variable"))
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


# is_any_filtered ----
testthat::test_that("is_any_filtered works properly when NA is present in data", {
  filter_state <- teal.slice:::ChoicesFilterState$new(
    c(LETTERS[1:2], NA),
    x_reactive = reactive(NULL),
    dataname = "data",
    varname = "x",
    extract_type = character(0)
  )

  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "x", selected = LETTERS[1:2])))
  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "x", keep_na = FALSE)))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "x", keep_na = TRUE)))
  testthat::expect_false(shiny::isolate(filter_state$is_any_filtered()))

  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "x", selected = LETTERS[1])))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  shiny::isolate(filter_state$set_state(filter_var(dataname = "data", varname = "x", keep_na = FALSE)))
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))
})



testthat::test_that("is_any_filtered returns TRUE when enabled and FALSE when disabled", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = ChoicesFilterState,
    public = list(
      disable = function() private$disable(),
      enable = function() private$enable()
    )
  )
  fs <- testfs$new(c("a", "b"), varname = "x", dataname = "data")
  fs$set_state(filter_var(dataname = "data", varname = "x", selected = "a", keep_na = TRUE))
  testthat::expect_true(shiny::isolate(fs$is_any_filtered()))
  shiny::isolate(fs$disable())
  testthat::expect_false(shiny::isolate(fs$is_any_filtered()))
})

testthat::test_that("is_any_filtered is changed by choices parameter", {
  filter_state <- ChoicesFilterState$new(
    chars,
    dataname = "data", varname = "variable", choices = chars[c(1, 2)]
  )
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))

  filter_state <- ChoicesFilterState$new(
    chars,
    dataname = "data", varname = "variable", choices = chars[c(1, 2, 3)]
  )
  testthat::expect_false(shiny::isolate(filter_state$is_any_filtered()))

  filter_state <- ChoicesFilterState$new(
    chars,
    dataname = "data", varname = "variable", choices = chars[1]
  )
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))
})

# is_choice_limited ----
testthat::test_that("is_choice_limited is set properly", {
  test <- R6::R6Class(
    inherit = ChoicesFilterState,
    public = list(
      test_is_choice_limited = function() private$is_choice_limited
    )
  )

  x <- rep(c("A", "B", "C", "D", "E", "F"), times = 5)
  xr <- c(rep(c("F", "A", "D"), times = 2), "D")

  filter_state <- test$new(
    x = x,
    x_reactive = reactive(xr),
    dataname = "data",
    varname = "x",
    choices = c("B", "C", "D", "E", "F"),
    extract_type = character(0)
  )
  testthat::expect_true(shiny::isolate(filter_state$is_any_filtered()))
  testthat::expect_true(shiny::isolate(filter_state$test_is_choice_limited()))
})

testthat::test_that("ChoicesFilterState private methods return proper filtered counts", {
  test <- R6::R6Class(
    inherit = ChoicesFilterState,
    public = list(
      test_get_choices_counts = function() private$get_choices_counts(),
      test_choices_counts = function() private$choices_counts
    )
  )

  x <- rep(c("A", "B", "C", "D", "E", "F"), times = 5)
  xr <- c(rep(c("F", "A", "D"), times = 2), "D")

  filter_state <- test$new(
    x = x,
    x_reactive = reactive(xr),
    varname = "x",
    dataname = "data",
    extract_type = character(0)
  )

  testthat::expect_identical(
    shiny::isolate(filter_state$test_get_choices_counts()),
    table(factor(xr, levels = unique(x)))
  )


  testthat::expect_identical(
    shiny::isolate(filter_state$test_choices_counts()),
    unname(table(x))
  )
})
