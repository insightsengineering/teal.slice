testthat::test_that("The constructor accepts character or factor", {
  testthat::expect_no_error(ChoicesFilterState$new("test", varname = "test"))
  testthat::expect_no_error(ChoicesFilterState$new(as.factor("test"), varname = "test"))
})

testthat::test_that("get_call returns a condition true for values passed in constructor", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  test <- "test"
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))

  filter_state <- ChoicesFilterState$new(factor("test"), varname = "test")
  test <- factor("test")
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition true for the values passed to set_selected", {
  filter_state <- ChoicesFilterState$new(c(letters[1:7]), varname = "test")
  filter_state$set_selected(letters[2:3])
  test <- letters[1:4]
  testthat::expect_equal(eval(shiny::isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
})

testthat::test_that("get_call returns a condition returning NA for NA values", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  test <- NA
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call())), NA)
})

testthat::test_that("get_call returns a condition true for NA values", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  filter_state$set_keep_na(TRUE)
  test <- NA
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("set_selected warns when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  testthat::expect_warning(filter_state$set_selected(c("test", 7)), "not in choices")
})

testthat::test_that("set_selected sets the intersection of choices and the passed values", {
  filter_state <- ChoicesFilterState$new(c("test1", "test2"), varname = "test")
  suppressWarnings(filter_state$set_selected(c("test1", 7)))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), "test1")
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "test")
  testthat::expect_no_error(filter_state$set_state(list(selected = "a", keep_na = TRUE)))
  testthat::expect_error(filter_state$set_state(list(selected = "a", unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "test")
  filter_state$set_state(list(selected = "a", keep_na = TRUE))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "a")
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "test")
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
      dataname = as.name("data"),
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
