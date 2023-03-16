utils::data(miniACC, package = "MultiAssayExperiment")

testthat::test_that("The constructor does not throw", {
  testthat::expect_no_error(
    MAEFilterStates$new(data = miniACC, dataname = "test")
  )
})

testthat::test_that("MAEFilterStates accept vector as an input for varlabels", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "iris")
  testthat::expect_equal(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

testthat::test_that("get_fun returns the MAE specific subset function", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "test")
  testthat::expect_equal(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

testthat::test_that("The constructor initializes a state_list", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = MAEFilterStates,
    public = list(
      state_list_get = function(x) private$state_list_get(x)
    )
  )
  filter_states <- testfs$new(data = miniACC, dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1)))
})

testthat::test_that("get_call returns executable subsetByColData call ", {
  test <- miniACC
  filter_states <- MAEFilterStates$new(data = test, dataname = "test")
  filter_states$set_filter_state(list(years_to_birth = list(selected = c(18, 60))))

  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(
      test <- MultiAssayExperiment::subsetByColData(test, y = test$years_to_birth >= 18 & test$years_to_birth <= 60)
    )
  )

  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_true(
    all(test$years_to_birth >= 18 & test$years_to_birth <= 60)
  )
})

testthat::test_that(
  "MAEFilterStates$set_filter_state sets filters in FilterState(s) specified by the named list",
  code = {
    maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
    fs <- list(
      years_to_birth = c(30, 50),
      gender = "female"
    )
    maefs$set_filter_state(state = fs)

    testthat::expect_equal(
      shiny::isolate(maefs$get_call()),
      quote(
        test <- MultiAssayExperiment::subsetByColData(
          test,
          y = test$years_to_birth >= 30 & test$years_to_birth <= 50 &
            test$gender == "female"
        )
      )
    )
  }
)

testthat::test_that("MAEFilterStates$set_filter_state updates filter state which was set already", {
  maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
  maefs$set_filter_state(state = list(years_to_birth = c(30, 50)))
  maefs$set_filter_state(
    state = list(
      years_to_birth = c(31, 50),
      gender = "female"
    )
  )

  testthat::expect_equal(
    shiny::isolate(maefs$get_filter_state()),
    list(
      years_to_birth = list(selected = c(31, 50), keep_na = FALSE, keep_inf = FALSE),
      gender = list(selected = "female", keep_na = FALSE)
    )
  )
})

testthat::test_that(
  "MAEFilterStates$set_filter_state throws error when not using a named list",
  code = {
    maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
    fs <- list(c(30, 50), "female")
    testthat::expect_error(maefs$set_filter_state(state = fs, data = miniACC))
  }
)

testthat::test_that(
  "MAEFilterStates$get_filter_state returns list identical to input",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
    fs <- list(
      years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
      gender = list(selected = "female", keep_na = TRUE)
    )
    maefs$set_filter_state(state = fs)
    testthat::expect_equal(shiny::isolate(maefs$get_filter_state()), fs)
  }
)

testthat::test_that(
  "MAEFilterStates$remove_filter_state removes filters in FilterState(s)",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
    fs <- list(
      years_to_birth = c(30, 50),
      gender = "female"
    )
    maefs$set_filter_state(state = fs)
    maefs$remove_filter_state("years_to_birth")

    testthat::expect_equal(
      shiny::isolate(maefs$get_call()),
      quote(test <- MultiAssayExperiment::subsetByColData(test, y = test$gender == "female"))
    )
  }
)

testthat::test_that(
  "MAEFilterStates$remove_filter_state throws warning when name is not in FilterStates",
  code = {
    teal.logger::suppress_logs()
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
    fs <- list(
      years_to_birth = c(30, 50),
      gender = "female"
    )
    maefs$set_filter_state(state = fs)
    testthat::expect_warning(maefs$remove_filter_state("years_to_birth2"))
  }
)

# Format ----
testthat::test_that("$format() is a method of MAEFilterStates", {
  maefs <- MAEFilterStates$new(data = miniACC, dataname = "iris")
  testthat::expect_no_error(
    shiny::isolate(maefs$format())
  )
})

testthat::test_that("$format() asserts the indent argument is a number", {
  maefs <- MAEFilterStates$new(data = miniACC, dataname = "iris")
  testthat::expect_error(
    maefs$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() concatenates its FilterState elements using \\n and indents the FilterState objects", {
  maefs <- MAEFilterStates$new(data = miniACC, dataname = "test")
  maefs$set_filter_state(
    state = list(
      years_to_birth = c(30, 50),
      vital_status = 1
    )
  )

  testthat::expect_equal(
    shiny::isolate(maefs$format()),
    paste(
      c(
        "Subject filters:",
        "  Filtering on: years_to_birth",
        "    Selected range: 30.000 - 50.000",
        "    Include missing values: FALSE",
        "  Filtering on: vital_status",
        "    Selected values: 1",
        "    Include missing values: FALSE"
      ),
      collapse = "\n"
    )
  )
})

testthat::test_that("get_filter_count returns the number of active filter states - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "test")
  filter_states$set_filter_state(
    state = list(
      years_to_birth = c(30, 50),
      vital_status = 1
    )
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  filter_states$remove_filter_state(state_id = "vital_status")
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
