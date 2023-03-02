chars <- c("item1", "item2", "item3")
facts <- as.factor(c("item1", "item2", "item3"))
nums <- c(1, 2, 3)
dates <- as.Date("2000-01-01") + 0:2
posixct <- as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:2
posixlt <- as.POSIXlt(as.POSIXct("2000-01-01 12:00:00", tz = "GMT") + 0:2)


# constructor ----
testthat::test_that("constructor accepts all data classes", {
  testthat::expect_no_error(ChoicesFilterState$new(chars, varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(facts, varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(nums, varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(dates, varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(posixct, varname = "variable"))
  testthat::expect_no_error(ChoicesFilterState$new(posixlt, varname = "variable"))
})

# get_call ----
testthat::test_that("get_call returns call that evaluated leaves all values passed to constructor", {
  filter_state <- ChoicesFilterState$new(chars, varname = "variable")
  variable <- chars
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(facts, varname = "variable")
  variable <- facts
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(nums, varname = "variable")
  variable <- nums
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(dates, varname = "variable")
  variable <- dates
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(posixct, varname = "variable")
  variable <- posixct
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))

  filter_state <- ChoicesFilterState$new(posixlt, varname = "variable")
  variable <- posixlt
  testthat::expect_true(all(eval(shiny::isolate(filter_state$get_call()))))
})


testthat::test_that("get_call returns condition that specifies values passed to set_selected", {
  filter_state <- ChoicesFilterState$new(chars, varname = "variable")
  filter_state$set_selected(chars[1])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == "item1"))
  )
  filter_state$set_selected(chars[1:2])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% c("item1", "item2")))
  )
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% c("item1", "item2")))
  )

  filter_state <- ChoicesFilterState$new(facts, varname = "variable")
  filter_state$set_selected(facts[1])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == "item1"))
  )
  filter_state$set_selected(facts[1:2])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% c("item1", "item2")))
  )
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% c("item1", "item2")))
  )

  filter_state <- ChoicesFilterState$new(nums, varname = "variable")
  filter_state$set_selected(nums[1])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == 1))
  )
  filter_state$set_selected(nums[1:2])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% c(1, 2)))
  )
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% c(1, 2)))
  )

  filter_state <- ChoicesFilterState$new(dates, varname = "variable")
  filter_state$set_selected(dates[1])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == as.Date("2000-01-01")))
  )
  filter_state$set_selected(dates[1:2])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% as.Date(c("2000-01-01", "2000-01-02"))))
  )
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(is.na(variable) | variable %in% as.Date(c("2000-01-01", "2000-01-02"))))
  )

  filter_state <- ChoicesFilterState$new(posixct, varname = "variable")
  filter_state$set_selected(posixct[1])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == as.POSIXct("2000-01-01 12:00:00", tz = "GMT")))
  )
  filter_state$set_selected(posixct[1:2])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% as.POSIXct(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT")))
  )
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(
      quote(is.na(variable) |
        variable %in% as.POSIXct(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT"))
    )
  )

  filter_state <- ChoicesFilterState$new(posixlt, varname = "variable")
  filter_state$set_selected(posixlt[1])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable == as.POSIXlt("2000-01-01 12:00:00", tz = "GMT")))
  )
  filter_state$set_selected(posixlt[1:2])
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(quote(variable %in% as.POSIXlt(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT")))
  )
  filter_state$set_keep_na(TRUE)
  testthat::expect_identical(
    deparse1(shiny::isolate(filter_state$get_call())),
    deparse1(
      quote(is.na(variable) |
        variable %in% as.POSIXlt(c("2000-01-01 12:00:00", "2000-01-01 12:00:01"), tz = "GMT"))
    )
  )
})

# set_selected ----
testthat::test_that("set_selected raises warning when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new(chars, varname = "variable")
  testthat::expect_warning(filter_state$set_selected("item4"), "not in choices")
  testthat::expect_warning(filter_state$set_selected(c("item1", "item4")), "not in choices")

  filter_state <- ChoicesFilterState$new(facts, varname = "variable")
  testthat::expect_warning(filter_state$set_selected("item4"), "not in choices")
  testthat::expect_warning(filter_state$set_selected(c("item1", "item4")), "not in choices")

  filter_state <- ChoicesFilterState$new(nums, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(4), "not in choices")
  testthat::expect_warning(filter_state$set_selected(c(1, 4)), "not in choices")

  filter_state <- ChoicesFilterState$new(dates, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(dates[3] + 1), "not in choices")
  testthat::expect_warning(filter_state$set_selected(c(dates[1], dates[3] + 1)), "not in choices")

  filter_state <- ChoicesFilterState$new(posixct, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(posixct[3] + 1), "not in choices")
  testthat::expect_warning(filter_state$set_selected(c(posixct[1], posixct[3] + 1)), "not in choices")

  filter_state <- ChoicesFilterState$new(posixlt, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(as.POSIXlt(posixlt[3] + 1)), "not in choices")
  testthat::expect_warning(filter_state$set_selected(as.POSIXlt(c(posixlt[1], posixlt[3] + 1))), "not in choices")
})

testthat::test_that("set_selected sets the intersection of choices and the passed values", {
  filter_state <- ChoicesFilterState$new(chars, varname = "variable")
  suppressWarnings(isolate(filter_state$set_selected(c("item1", "item4"))))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "item1")

  filter_state <- ChoicesFilterState$new(facts, varname = "variable")
  suppressWarnings(isolate(filter_state$set_selected(c("item1", "item4"))))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "item1")

  filter_state <- ChoicesFilterState$new(nums, varname = "variable")
  suppressWarnings(isolate(filter_state$set_selected(c(1, 4))))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "1")

  filter_state <- ChoicesFilterState$new(dates, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(c(dates[1], dates[3] + 1)), "not in choices")
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "2000-01-01")

  filter_state <- ChoicesFilterState$new(posixct, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(c(posixct[1], posixct[3] + 1)), "not in choices")
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "2000-01-01 12:00:00")

  filter_state <- ChoicesFilterState$new(posixlt, varname = "variable")
  testthat::expect_warning(filter_state$set_selected(as.POSIXlt(c(posixlt[1], posixlt[3] + 1))), "not in choices")
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "2000-01-01 12:00:00")
})

# set_state ----
testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "variable")
  testthat::expect_no_error(filter_state$set_state(list(selected = "a", keep_na = TRUE)))
  testthat::expect_error(filter_state$set_state(list(selected = "a", unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "variable")
  filter_state$set_state(list(selected = "a", keep_na = TRUE))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "a")
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "variable")
  filter_state$set_state(list(selected = "a", keep_na = TRUE))
  testthat::expect_no_error(filter_state$set_state(list(selected = "b")))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "b")
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that(
  "ChoicesFilterState$is_any_filtered works properly when NA is present in data",
  code = {
    filter_state <- teal.slice:::ChoicesFilterState$new(
      c(LETTERS[1:2], NA),
      varname = "x",
      dataname = "data",
      extract_type = character(0)
    )
    shiny::isolate(filter_state$set_keep_na(FALSE))
    shiny::isolate(filter_state$set_selected(LETTERS[1:2]))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_keep_na(TRUE))
    shiny::isolate(filter_state$set_selected(LETTERS[1:2]))
    testthat::expect_false(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_selected(LETTERS[1]))
    shiny::isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )

    shiny::isolate(filter_state$set_selected(LETTERS[1]))
    shiny::isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      shiny::isolate(filter_state$is_any_filtered())
    )
  }
)
