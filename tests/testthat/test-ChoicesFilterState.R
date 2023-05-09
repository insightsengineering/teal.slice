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

testthat::test_that("constructor sets default state", {
  fs <- ChoicesFilterState$new(letters, dataname = "data", varname = "variable")
  testthat::expect_identical(
    shiny::isolate(fs$get_state()),
    filter_var(
      dataname = "data",
      varname = "variable",
      choices = letters,
      selected = letters
    )
  )
})

# get_call ----
testthat::test_that("method get_call of default ChoicesFilterState object returns NULL", {
  filter_state <- ChoicesFilterState$new(letters, dataname = "data", varname = "variable")
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns NULL if all choices are selected", {
  filter_state <- ChoicesFilterState$new(letters, dataname = "data", varname = "variable", selected = letters)
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- ChoicesFilterState$new(letters, dataname = "data", varname = "variable", selected = letters[1:3])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call returns NULL if disabled", {
  filter_state <- ChoicesFilterState$new(
    letters, dataname = "data", varname = "variable", selected = letters[1:3], disabled = TRUE
  )
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- ChoicesFilterState$new(
    letters, dataname = "data", varname = "variable",
    choices = letters[1:3], selected = letters[1:3]
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- ChoicesFilterState$new(
    letters, dataname = "data", varname = "variable", selected = letters[1:3], extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname$variable %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- ChoicesFilterState$new(
    letters, dataname = "data", varname = "variable", selected = letters[1:3], extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname[, "variable"] %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call uses `==` comparison when single value selected", {
  filter_state <- ChoicesFilterState$new(chars, dataname = "data", varname = "variable", selected = chars[1])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable == "item1")
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- ChoicesFilterState$new(
    c(chars, NA), dataname = "data", varname = "variable",
    selected = chars[1:2], keep_na = TRUE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | variable %in% c("item1", "item2"))
  )
})

testthat::test_that("get_call returns call if all selected but NA exists", {
  filter_state <- ChoicesFilterState$new(
    c(chars, NA), dataname = "data", varname = "variable", keep_na = FALSE
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable %in% c("item1", "item2", "item3"))
  )
})

testthat::test_that("get_call returns calls appropriate for factor variable", {
  filter_state <- ChoicesFilterState$new(facts, dataname = "data", varname = "variable", selected = facts[1])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable == "item1")
  )
})

testthat::test_that("get_call returns calls appropriate for numeric variable", {
  filter_state <- ChoicesFilterState$new(nums, dataname = "data", varname = "variable", selected = nums[1])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable == 1)
  )
})

testthat::test_that("get_call returns calls appropriate for date variable", {
  filter_state <- ChoicesFilterState$new(dates, dataname = "data", varname = "variable", selected = dates[1])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable == as.Date("2000-01-01"))
  )
})

testthat::test_that("get_call returns calls appropriate for posixct variable", {
  filter_state <- ChoicesFilterState$new(posixct, dataname = "data", varname = "variable", selected = posixct[1])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable == as.POSIXct("2000-01-01 12:00:00", tz = "GMT"))
  )
})

testthat::test_that("get_call returns calls appropriate for posixlt variable", {
  filter_state <- ChoicesFilterState$new(posixlt, dataname = "data", varname = "variable", selected = posixlt[1])
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable == as.POSIXlt("2000-01-01 12:00:00", tz = "GMT"))
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

testthat::test_that("set_state sets intersection of choices and passed values", {
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
