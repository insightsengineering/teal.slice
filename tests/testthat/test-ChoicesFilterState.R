chars <- c("item1", "item2", "item3")
facts <- as.factor(c("item1", "item2", "item3"))
nums <- c(1, 2, 3)
dates <- as.Date("2000-01-01") + 0:2
posixct <- as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:2
posixlt <- as.POSIXlt(as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:2)


# constructor ----
testthat::test_that("constructor accepts all data classes", {
  testthat::expect_no_error(ChoicesFilterState$new(chars, slice = teal_slice(dataname = "data", varname = "var")))
  testthat::expect_no_error(ChoicesFilterState$new(facts, slice = teal_slice(dataname = "data", varname = "var")))
  testthat::expect_no_error(ChoicesFilterState$new(nums, slice = teal_slice(dataname = "data", varname = "var")))
  testthat::expect_no_error(ChoicesFilterState$new(dates, slice = teal_slice(dataname = "data", varname = "var")))
  testthat::expect_no_error(ChoicesFilterState$new(posixct, slice = teal_slice(dataname = "data", varname = "var")))
  testthat::expect_no_error(ChoicesFilterState$new(posixlt, slice = teal_slice(dataname = "data", varname = "var")))
})

testthat::test_that("constructor raises warning if choices out of range", {
  testthat::expect_warning(
    ChoicesFilterState$new(
      chars,
      slice = teal_slice(dataname = "data", varname = "var", choices = c(chars, "item4"))
    ),
    "Some choices not found in data. Adjusting."
  )
  testthat::expect_warning(
    ChoicesFilterState$new(
      chars,
      slice = teal_slice(dataname = "data", varname = "var", choices = "item4")
    ),
    "Some choices not found in data. Adjusting.|None of the choices were found in data. Setting defaults."
  )
})

testthat::test_that("constructor raises warning if selected out of range", {
  testthat::expect_warning(
    ChoicesFilterState$new(
      chars,
      slice = teal_slice(dataname = "data", varname = "var", selected = "item4")
    ),
    "not in choices"
  )
})

testthat::test_that("constructor sets default state", {
  fs <- ChoicesFilterState$new(letters, slice = teal_slice(dataname = "data", varname = "var"))
  expect_identical_slice(
    fs$get_state(),
    teal_slice(
      dataname = "data",
      varname = "var",
      choices = letters,
      multiple = TRUE,
      selected = letters
    )
  )
})

testthat::test_that("constructor forces single selected when multiple is FALSE", {
  testthat::expect_warning(
    state <- ChoicesFilterState$new(
      x = letters,
      slice = teal_slice(dataname = "data", varname = "var", selected = c("b", "c"), multiple = FALSE)
    )
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()$selected),
    "b"
  )
})

# get_call ----
testthat::test_that("method get_call of default ChoicesFilterState object returns NULL", {
  filter_state <- ChoicesFilterState$new(letters, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns NULL if all choices are selected", {
  filter_state <- ChoicesFilterState$new(
    letters,
    slice = teal_slice(dataname = "data", varname = "var", selected = letters)
  )
  testthat::expect_null(shiny::isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- ChoicesFilterState$new(
    letters,
    slice = teal_slice(dataname = "data", varname = "var", selected = letters[1:3])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- ChoicesFilterState$new(
    letters,
    slice = teal_slice(
      dataname = "data", varname = "var", choices = letters[1:3], selected = letters[1:3]
    )
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    NULL
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- ChoicesFilterState$new(
    letters,
    slice = teal_slice(dataname = "data", varname = "var", selected = letters[1:3]), extract_type = "list"
  )

  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname$var %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- ChoicesFilterState$new(
    letters,
    slice = teal_slice(dataname = "data", varname = "var", selected = letters[1:3]),
    extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(dataname[, "var"] %in% c("a", "b", "c"))
  )
})

testthat::test_that("get_call uses `==` comparison when single value selected", {
  filter_state <- ChoicesFilterState$new(
    chars,
    slice = teal_slice(dataname = "data", varname = "var", selected = chars[1])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var == "item1")
  )
})

testthat::test_that("get_call adds is.na(var) to returned call if keep_na is true", {
  filter_state <- ChoicesFilterState$new(
    x = c(chars, NA),
    slice = teal_slice(dataname = "data", varname = "var", selected = chars[1:2], keep_na = TRUE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(var) | var %in% c("item1", "item2"))
  )
})

testthat::test_that("get_call returns call if all selected but NA exists", {
  filter_state <- ChoicesFilterState$new(
    x = c(chars, NA),
    slice = teal_slice(dataname = "data", varname = "var", keep_na = FALSE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!is.na(var))
  )
})

testthat::test_that("get_call returns calls appropriate for factor var", {
  filter_state <- ChoicesFilterState$new(
    x = facts,
    slice = teal_slice(dataname = "data", varname = "var", selected = facts[1])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var == "item1")
  )
})

testthat::test_that("get_call returns calls appropriate for numeric var", {
  filter_state <- ChoicesFilterState$new(
    nums,
    slice = teal_slice(dataname = "data", varname = "var", selected = nums[1])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var == 1)
  )
})

testthat::test_that("get_call returns calls appropriate for date var", {
  filter_state <- ChoicesFilterState$new(
    x = dates,
    slice = teal_slice(dataname = "data", varname = "var", selected = dates[1])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var == as.Date("2000-01-01"))
  )
})

testthat::test_that("get_call returns calls appropriate for posixct var", {
  filter_state <- ChoicesFilterState$new(
    x = posixct,
    slice = teal_slice(dataname = "data", varname = "var", selected = posixct[1])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var == as.POSIXct("2000-01-01 12:00:00", tz = "GMT"))
  )
})

testthat::test_that("get_call returns calls appropriate for posixlt var", {
  filter_state <- ChoicesFilterState$new(
    x = posixlt,
    slice = teal_slice(dataname = "data", varname = "var", selected = posixlt[1])
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(var == as.POSIXlt("2000-01-01 12:00:00", tz = "GMT"))
  )
})

# set_state ----
testthat::test_that("set_state raises warning when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new(x = chars, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = "item4")),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c("item1", "item4"))),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(facts, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = "item4")),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      teal_slice(dataname = "data", varname = "var", selected = c("item1", "item4"))
    ),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(nums, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = 4)),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      teal_slice(dataname = "data", varname = "var", selected = c(1, 4))
    ),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(dates, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = dates[3] + 1)),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      teal_slice(dataname = "data", varname = "var", selected = c(dates[1], dates[3] + 1))
    ),
    "not in choices"
  )

  filter_state <- ChoicesFilterState$new(posixct, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = posixct[3] + 1)),
    "not in choices"
  )
  testthat::expect_warning(
    filter_state$set_state(
      teal_slice(dataname = "data", varname = "var", selected = c(posixct[1], posixct[3] + 1))
    ),
    "not in choices"
  )
})

testthat::test_that("set_state sets intersection of choices and passed values", {
  filter_state <- ChoicesFilterState$new(chars, slice = teal_slice(dataname = "data", varname = "var"))
  suppressWarnings(
    shiny::isolate(
      filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c("item1", "item4")))
    )
  )
  shiny::isolate(testthat::expect_identical(filter_state$get_state()$selected, "item1"))

  filter_state <- ChoicesFilterState$new(facts, slice = teal_slice(dataname = "data", varname = "var"))
  suppressWarnings(
    shiny::isolate(
      filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c("item1", "item4")))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "item1")

  filter_state <- ChoicesFilterState$new(nums, slice = teal_slice(dataname = "data", varname = "var"))
  suppressWarnings(
    shiny::isolate(
      filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c(1, 4)))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "1")

  filter_state <- ChoicesFilterState$new(dates, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    shiny::isolate(
      filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c(dates[1], dates[3] + 1)))
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "2000-01-01")

  filter_state <- ChoicesFilterState$new(posixct, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    shiny::isolate(
      filter_state$set_state(
        teal_slice(selected = c(posixct[1], posixct[3] + 1), dataname = "data", varname = "var")
      )
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "2000-01-01 12:00:00")

  filter_state <- ChoicesFilterState$new(posixlt, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_warning(
    shiny::isolate(
      filter_state$set_state(
        teal_slice(selected = as.POSIXlt(c(posixlt[1], posixlt[3] + 1)), dataname = "data", varname = "var")
      )
    )
  )
  testthat::expect_identical(shiny::isolate(filter_state$get_state()$selected), "2000-01-01 12:00:00")
})

testthat::test_that("set_state aborts multiple selection is aborted when multiple = FALSE", {
  filter_state <- ChoicesFilterState$new(
    x = chars,
    slice = teal_slice(dataname = "data", varname = "var", multiple = TRUE)
  )
  testthat::expect_no_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c("item1", "item3")))
  )

  filter_state <- ChoicesFilterState$new(
    x = chars,
    slice = teal_slice(dataname = "data", varname = "var", multiple = FALSE)
  )
  testthat::expect_no_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = "item3"))
  )
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = c("item1", "item3"))),
    "Maintaining previous selection."
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$get_state()$selected),
    "item3"
  )
})

# format ----
testthat::test_that("format accepts logical show_all", {
  filter_state <- ChoicesFilterState$new(7, slice = teal_slice(dataname = "data", varname = "var"))
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

testthat::test_that("format returns properly formatted string representation", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- ChoicesFilterState$new(values, slice = teal_slice(dataname = "data", varname = "var"))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "ChoicesFilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "ChoicesFilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# print ---
testthat::test_that("print returns properly formatted string representation", {
  values <- paste("value", 1:3, sep = "_")
  filter_state <- ChoicesFilterState$new(values, slice = teal_slice(dataname = "data", varname = "var"))
  filter_state$set_state(teal_slice(dataname = "data", varname = "var", selected = values, keep_na = FALSE))
  testthat::expect_equal(
    utils::capture.output(filter_state$print()),
    c("ChoicesFilterState:", utils::capture.output(print(shiny::isolate(filter_state$get_state()))))
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c("ChoicesFilterState:", utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)))
  )
})
