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
      selected = NULL
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

# get_call table ----

testthat::test_that("get_call works for various combinations", {
  # Scenarios
  #      NAs in data | keep_na | selected |    class    | result
  ## 1.      Yes     |   NULL  |  'all'   | 'character' | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:8])
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 2.       No     |   NULL  |  'all'   | 'character' | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:8])
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 3.      Yes     |   TRUE  |  'all'   | 'character' | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:8], keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 4.       No     |   TRUE  |  'all'   | 'character' | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:8], keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 5.      Yes     |  FALSE  |  'all'   | 'character' | !is.na(x) ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:8], keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(!is.na(x)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 6.       No     |  FALSE  |  'all'   | 'character' | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:8], keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 7.      Yes     |   NULL  |'limited' | 'character' | is.na(x) | x %in% c('a', 'b') ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:2])
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x %in% c("a", "b")))
  #      NAs in data | keep_na | selected |    class    | result
  ## 8.       No     |   NULL  |'limited' | 'character' | x %in% c('a', 'b') ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:2])
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x %in% c("a", "b")))
  #      NAs in data | keep_na | selected |    class    | result
  ## 9.      Yes     |   TRUE  |'limited' | 'character' | is.na(x) | x %in% c('a', 'b') ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:2], keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x %in% c("a", "b")))
  #      NAs in data | keep_na | selected |    class    | result
  ## 10.       No     |   TRUE  |'limited' | 'character' | x %in% c('a', 'b') ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:2], keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x %in% c("a", "b")))
  #      NAs in data | keep_na | selected |    class    | result
  ## 11.      Yes     |  FALSE  |'limited' | 'character' | !is.na(x) & x %in% c("a", "b") ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:2], keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(!is.na(x) & x %in% c("a", "b")))
  #      NAs in data | keep_na | selected |    class    | result
  ## 12.       No     |  FALSE  |'limited' | 'character' | x %in% c('a', 'b') ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1:2], keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x %in% c("a", "b")))
  #      NAs in data | keep_na | selected |    class    | result
  ## 13.      Yes     |   NULL  | 'single' | 'character' | is.na(x) | x == "a" ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1])
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x == "a"))
  #      NAs in data | keep_na | selected |    class    | result
  ## 14.       No     |   NULL  | 'single' | 'character' | x == 'a' ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1])
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x == "a"))
  #      NAs in data | keep_na | selected |    class    | result
  ## 15.      Yes     |   TRUE  | 'single' | 'character' | is.na(x) | x == 'a' ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1], keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x == "a"))
  #      NAs in data | keep_na | selected |    class    | result
  ## 16.       No     |   TRUE  | 'single' | 'character' | x == 'a' ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1], keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x == "a"))
  #      NAs in data | keep_na | selected |    class    | result
  ## 17.      Yes     |  FALSE  | 'single' | 'character' | !is.na(x) & x == "a" ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1], keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(!is.na(x) & x == "a"))
  #      NAs in data | keep_na | selected |    class    | result
  ## 18.       No     |  FALSE  | 'single' | 'character' | x == 'a' ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", selected = letters[1], keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x == "a"))
  #      NAs in data | keep_na | selected |    class    | result
  ## 19.      Yes     |   NULL  |  'none'  | 'character' | is.na(x) ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x")
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 20.       No     |   NULL  |  'none'  | 'character' | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x")
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 21.      Yes     |   TRUE  |  'none'  | 'character' | is.na(x) ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = TRUE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 22.       No     |   TRUE  |  'none'  | 'character' | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = TRUE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 23.      Yes     |  FALSE  |  'none'  | 'character' | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8], NA_character_),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = FALSE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 24.       No     |  FALSE  |  'none'  | 'character' | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(letters[1:8]),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = FALSE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 25.      Yes     |   NULL  |  'all'   | "numeric" | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:4)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 26.       No     |   NULL  |  'all'   | "numeric" | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:4)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 27.      Yes     |   TRUE  |  'all'   | "numeric" | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:4, keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 28.       No     |   TRUE  |  'all'   | "numeric" | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:4, keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 29.      Yes     |  FALSE  |  'all'   | "numeric" | !is.na(x) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:4, keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(!is.na(x)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 30.       No     |  FALSE  |  'all'   | "numeric" | NULL ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:4, keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), NULL)
  #      NAs in data | keep_na | selected |    class    | result
  ## 31.      Yes     |   NULL  |'limited' | "numeric" | is.na(x) | x %in% c(1, 2) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:2)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x %in% c(1, 2)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 32.       No     |   NULL  |'limited' | "numeric" | x %in% c(1, 2) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:2)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x %in% c(1L, 2L)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 33.      Yes     |   TRUE  |'limited' | "numeric" | is.na(x) | x %in% c(1, 2) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:2, keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x %in% c(1, 2)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 34.       No     |   TRUE  |'limited' | "numeric" | x %in% c(1, 2) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:2, keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x %in% c(1L, 2L)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 35.      Yes     |  FALSE  |'limited' | "numeric" | !is.na(x) & x %in% c(1, 2) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:2, keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(!is.na(x) & x %in% c(1, 2)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 36.       No     |  FALSE  |'limited' | "numeric" | x %in% c(1, 2) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1:2, keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x %in% c(1L, 2L)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 37.      Yes     |   NULL  | 'single' | "numeric" | is.na(x) | x == 1 ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x == 1))
  #      NAs in data | keep_na | selected |    class    | result
  ## 38.       No     |   NULL  | 'single' | "numeric" | x == 1 ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x == 1L))
  #      NAs in data | keep_na | selected |    class    | result
  ## 39.      Yes     |   TRUE  | 'single' | "numeric" | is.na(x) | x == 1 ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1, keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x) | x == 1))
  #      NAs in data | keep_na | selected |    class    | result
  ## 40.       No     |   TRUE  | 'single' | "numeric" | x == 1 ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1, keep_na = TRUE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x == 1L))
  #      NAs in data | keep_na | selected |    class    | result
  ## 41.      Yes     |  FALSE  | 'single' | "numeric" | !is.na(x) & x == 1 ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1, keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(!is.na(x) & x == 1))
  #      NAs in data | keep_na | selected |    class    | result
  ## 42.       No     |  FALSE  | 'single' | "numeric" | x == 1 ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", selected = 1, keep_na = FALSE)
  )
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(x == 1L))
  #      NAs in data | keep_na | selected |    class    | result
  ## 43.      Yes     |   NULL  |  'none'  | "numeric" | is.na(x) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x")
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 44.       No     |   NULL  |  'none'  | "numeric" | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x")
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 45.      Yes     |   TRUE  |  'none'  | "numeric" | is.na(x) ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = TRUE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), quote(is.na(x)))
  #      NAs in data | keep_na | selected |    class    | result
  ## 46.       No     |   TRUE  |  'none'  | "numeric" | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = TRUE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 47.      Yes     |  FALSE  |  'none'  | "numeric" | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4, NA_real_),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = FALSE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  #      NAs in data | keep_na | selected |    class    | result
  ## 48.       No     |  FALSE  |  'none'  | "numeric" | FALSE ----
  filter_state <- ChoicesFilterState$new(
    x = c(1:4),
    slice = teal_slice(dataname = "data", varname = "x", keep_na = FALSE)
  )
  filter_state$set_state(teal_slice(dataname = "data", varname = "x", selected = character()))
  testthat::expect_equal(shiny::isolate(filter_state$get_call()), FALSE)
  # 49.      Yes     |   NULL  |  'all'   |   'dates'   | NULL
  # 50.       No     |   NULL  |  'all'   |   'dates'   | NULL
  # 51.      Yes     |   TRUE  |  'all'   |   'dates'   | NULL
  # 52.       No     |   TRUE  |  'all'   |   'dates'   | NULL
  # 53.      Yes     |  FALSE  |  'all'   |   'dates'   | !is.na(x)
  # 54.       No     |  FALSE  |  'all'   |   'dates'   | NULL
  # 55.      Yes     |   NULL  |'limited' |   'dates'   | x %in% c('2000-01-01', '2000-01-02')
  # 56.       No     |   NULL  |'limited' |   'dates'   | x %in% c('2000-01-01', '2000-01-02')
  # 57.      Yes     |   TRUE  |'limited' |   'dates'   | is.na(x) | x %in% c('2000-01-01', '2000-01-02')
  # 58.       No     |   TRUE  |'limited' |   'dates'   | x %in% c('2000-01-01', '2000-01-02')
  # 59.      Yes     |  FALSE  |'limited' |   'dates'   | x %in% c('2000-01-01', '2000-01-02')
  # 60.       No     |  FALSE  |'limited' |   'dates'   | x %in% c('2000-01-01', '2000-01-02')
  # 61.      Yes     |   NULL  | 'single' |   'dates'   | x == '2000-01-01'
  # 62.       No     |   NULL  | 'single' |   'dates'   | x == '2000-01-01'
  # 63.      Yes     |   TRUE  | 'single' |   'dates'   | is.na(x) | x == '2000-01-01'
  # 64.       No     |   TRUE  | 'single' |   'dates'   | x == '2000-01-01'
  # 65.      Yes     |  FALSE  | 'single' |   'dates'   | x == '2000-01-01'
  # 66.       No     |  FALSE  | 'single' |   'dates'   | x == '2000-01-01'
  # 67.      Yes     |   NULL  |  'none'  |   'dates'   |
  # 68.       No     |   NULL  |  'none'  |   'dates'   |
  # 69.      Yes     |   TRUE  |  'none'  |   'dates'   |
  # 70.       No     |   TRUE  |  'none'  |   'dates'   |
  # 71.      Yes     |  FALSE  |  'none'  |   'dates'   |
  # 72.       No     |  FALSE  |  'none'  |   'dates'   |
  # 73.      Yes     |   NULL  |  'all'   |  'POSIXct'  | NULL
  # 74.       No     |   NULL  |  'all'   |  'POSIXct'  | NULL
  # 75.      Yes     |   TRUE  |  'all'   |  'POSIXct'  | NULL
  # 76.       No     |   TRUE  |  'all'   |  'POSIXct'  | NULL
  # 77.      Yes     |  FALSE  |  'all'   |  'POSIXct'  | !is.na(x)
  # 78.       No     |  FALSE  |  'all'   |  'POSIXct'  | NULL
  # 79.      Yes     |   NULL  |'limited' |  'POSIXct'  | x %in% c("2000-01-01 12:00:00 GMT", "2000-01-01 12:00:01 GMT")
  # 80.       No     |   NULL  |'limited' |  'POSIXct'  | x %in% c("2000-01-01 12:00:00 GMT", "2000-01-01 12:00:01 GMT")
  # 81.      Yes     |   TRUE  |'limited' |  'POSIXct'  | is.na(x) | x %in% c("2000-01-01 12:00:00 GMT", "2000-01-01 12:00:01 GMT") # nolint
  # 82.       No     |   TRUE  |'limited' |  'POSIXct'  | x %in% c("2000-01-01 12:00:00 GMT", "2000-01-01 12:00:01 GMT")
  # 83.      Yes     |  FALSE  |'limited' |  'POSIXct'  | x %in% c("2000-01-01 12:00:00 GMT", "2000-01-01 12:00:01 GMT")
  # 84.       No     |  FALSE  |'limited' |  'POSIXct'  | x %in% c("2000-01-01 12:00:00 GMT", "2000-01-01 12:00:01 GMT")
  # 85.      Yes     |   NULL  | 'single' |  'POSIXct'  | x == "2000-01-01 12:00:00 GMT"
  # 86.       No     |   NULL  | 'single' |  'POSIXct'  | x == "2000-01-01 12:00:00 GMT"
  # 87.      Yes     |   TRUE  | 'single' |  'POSIXct'  | is.na(x) | x == "2000-01-01 12:00:00 GMT"
  # 88.       No     |   TRUE  | 'single' |  'POSIXct'  | x == "2000-01-01 12:00:00 GMT"
  # 89.      Yes     |  FALSE  | 'single' |  'POSIXct'  | x == "2000-01-01 12:00:00 GMT"
  # 90.       No     |  FALSE  | 'single' |  'POSIXct'  | x == "2000-01-01 12:00:00 GMT"
  # 91.      Yes     |   NULL  |  'none'  |  'POSIXct'  |
  # 92.       No     |   NULL  |  'none'  |  'POSIXct'  |
  # 93.      Yes     |   TRUE  |  'none'  |  'POSIXct'  |
  # 94.       No     |   TRUE  |  'none'  |  'POSIXct'  |
  # 95.      Yes     |  FALSE  |  'none'  |  'POSIXct'  |
  # 96.       No     |  FALSE  |  'none'  |  'POSIXct'  |
})
