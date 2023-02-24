testthat::test_that("The constructor accepts character or factor", {
  testthat::expect_no_error(ChoicesFilterState$new("test", x_reactive = reactive(NULL), varname = "test"))
  testthat::expect_no_error(ChoicesFilterState$new(as.factor("test"), reactive(NULL), varname = "test"))
})

testthat::test_that("The constructor accepts only reactive input for x_reactive", {
  testthat::expect_no_error(ChoicesFilterState$new("test", x_reactive = reactive(NULL), varname = "test"))
  testthat::expect_error(ChoicesFilterState$new("test", x_reactive = "test", varname = "test"))
  testthat::expect_error(ChoicesFilterState$new(as.factor("test"), NULL, varname = "test"))
})


testthat::test_that("get_call returns a condition true for values passed in constructor", {
  filter_state <- ChoicesFilterState$new("test", x_reactive = reactive(NULL), varname = "test")
  test <- "test"
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))

  filter_state <- ChoicesFilterState$new(factor("test"), x_reactive = reactive(NULL), varname = "test")
  test <- factor("test")
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition true for the values passed to set_selected", {
  filter_state <- ChoicesFilterState$new(c(letters[1:7]), x_reactive = reactive(NULL), varname = "test")
  filter_state$set_selected(letters[2:3])
  test <- letters[1:4]
  testthat::expect_equal(eval(shiny::isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
})

testthat::test_that("get_call returns a condition returning NA for NA values", {
  filter_state <- ChoicesFilterState$new("test", x_reactive = reactive(NULL), varname = "test")
  test <- NA
  testthat::expect_identical(eval(shiny::isolate(filter_state$get_call())), NA)
})

testthat::test_that("get_call returns a condition true for NA values", {
  filter_state <- ChoicesFilterState$new("test", x_reactive = reactive(NULL), varname = "test")
  filter_state$set_keep_na(TRUE)
  test <- NA
  testthat::expect_true(eval(shiny::isolate(filter_state$get_call())))
})

testthat::test_that("set_selected warns when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new("test", x_reactive = reactive(NULL), varname = "test")
  testthat::expect_warning(filter_state$set_selected(c("test", 7)), "not in choices")
})

testthat::test_that("set_selected sets the intersection of choices and the passed values", {
  filter_state <- ChoicesFilterState$new(c("test1", "test2"), x_reactive = reactive(NULL), varname = "test")
  suppressWarnings(filter_state$set_selected(c("test1", 7)))
  testthat::expect_equal(shiny::isolate(filter_state$get_selected()), "test1")
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), x_reactive = reactive(NULL), varname = "test")
  testthat::expect_no_error(filter_state$set_state(list(selected = "a", keep_na = TRUE)))
  testthat::expect_error(filter_state$set_state(list(selected = "a", unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), x_reactive = reactive(NULL), varname = "test")
  filter_state$set_state(list(selected = "a", keep_na = TRUE))
  testthat::expect_identical(shiny::isolate(filter_state$get_selected()), "a")
  testthat::expect_true(shiny::isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), x_reactive = reactive(NULL), varname = "test")
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
      x_reactive = reactive(NULL),
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

testthat::test_that(
  "ChoicesFilterState private methods return proper filtered counts and choice labels",
  code = {
    test <- R6::R6Class(
      inherit = ChoicesFilterState,
      public = list(
        test_get_filter_counts = function() private$get_filtered_counts(),
        test_get_choice_labels = function() private$get_choice_labels(),
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
      shiny::isolate(filter_state$test_get_filter_counts()),
      table(factor(xr, levels = unique(x)))
    )

    testthat::expect_identical(
      shiny::isolate(filter_state$test_get_choice_labels()),
      c("A (2/5)", "B (0/5)", "C (0/5)", "D (3/5)", "E (0/5)", "F (2/5)")
    )

    testthat::expect_identical(
      shiny::isolate(filter_state$test_choices_counts()),
      unname(table(x))
    )
  }
)

testthat::test_that("disabling/enabling", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  TestFs = R6::R6Class(
    classname = "TestFs",
    inherit = ChoicesFilterState,
    public = list(
      cache_state = function() {private$cache_state()},
      restore_state = function() {private$restore_state()},
      set_disabled = function(val) {private$set_disabled(val)},
      is_disabled = function() {private$is_disabled()}
    )
  )
  fs <- TestFs$new(c("a", "b", "c"), reactive(c("a", "b", "c")), 'x')

  testthat::expect_false(fs$is_disabled())
  # want to ensure there is some filtering to check correctness of
  #  is_any_filtered() when disabled/enabled
  fs$set_selected(c("a", "b"))
  testthat::expect_true(fs$is_any_filtered())

  fs$set_disabled(TRUE)
  testthat::expect_true(fs$is_disabled())
  testthat::expect_false(fs$is_any_filtered())

  fs$cache_state()
  testthat::expect_equal(
    fs$get_state(),
    list(selected = NULL, keep_na = NULL)
  )

  fs$set_disabled(FALSE)
  fs$restore_state()
  testthat::expect_false(fs$is_disabled())
  testthat::expect_true(fs$is_any_filtered())
  testthat::expect_equal(
    fs$get_state(),
    list(
      selected = c("a", "b"),
      keep_na = FALSE
    )
  )

})
