utils::data(miniACC, package = "MultiAssayExperiment")

# initialize ----
testthat::test_that("constructor accepts a MultiAssayExperiment", {
  testthat::expect_no_error(
    MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  )
  testthat::expect_error(
    MAEFilterStates$new(data = miniACC[[1]], dataname = "miniACC"),
    "Assertion on 'data' failed"
  )
})

testthat::test_that("constructor initializes state_list with one element named 'y'", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = MAEFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = miniACC, dataname = "miniACC")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1, NULL)))
  testthat::expect_no_error(shiny::isolate(filter_states$state_list_get("y", NULL)))
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_get(2, NULL)),
    "Filter state list 2 has not been initialized"
  )
})

# get_fun ----
testthat::test_that("get_fun returns MultiAssayExperiment::subsetByColData", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  testthat::expect_identical(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

# set_filter_state ----
testthat::test_that("set_filter_state only accepts `teal_slices`", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  fs <- filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(30, 50),
               keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var(dataname = "miniACC", varname = "gender", selected = "female",
               keep_na = FALSE, datalabel = "subjects", target = "y")
  )
  testthat::expect_error(filter_states$set_filter_state(state = fs[[1]]), "Assertion on 'state' failed")
  testthat::expect_no_error(filter_states$set_filter_state(state = fs))
})


testthat::test_that("set_filter_state adds states to state_list", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = MAEFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = miniACC, dataname = "miniACC")
  fs <- filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(30, 50),
               keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var(dataname = "miniACC", varname = "gender", selected = "female",
               keep_na = FALSE, datalabel = "subjects", target = "y")
  )
  state_list <- shiny::isolate(filter_states$state_list_get(1, NULL))

  testthat::expect_length(state_list, 0)

  filter_states$set_filter_state(state = fs)
  state_list <- shiny::isolate(filter_states$state_list_get(1, NULL))

  testthat::expect_length(state_list, 2)
})

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` identical to that used to set state (choices excluded)", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  fs <- filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(31, 50),
               keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var(dataname = "miniACC", varname = "gender", selected = "female",
               keep_na = FALSE, datalabel = "subjects", target = "y")
  )
  filter_states$set_filter_state(fs)

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
})

# set_filter_state ctd. ----
testthat::test_that("set_filter_state updates existing filter states", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(30, 50),
                 keep_na = FALSE, datalabel = "subjects", target = "y")
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(31, 50),
               keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var(dataname = "miniACC", varname = "gender", selected = "female",
               keep_na = FALSE, datalabel = "subjects", target = "y")
  )
  filter_states$set_filter_state(fs)

  testthat::expect_equal(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
})

# remove_filter_state ----
testthat::test_that("remove_filter_state removes filters", {

  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  fs <- filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(33, 50),
               keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var(dataname = "miniACC", varname = "gender", selected = "female",
               keep_na = FALSE, datalabel = "subjects", target = "y")
  )
  filter_states$set_filter_state(state = fs)

  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 2)
  filter_states$remove_filter_state(fs[1])
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 1)

  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(miniACC <- MultiAssayExperiment::subsetByColData(miniACC, y = miniACC$gender == "female"))
  )
})

testthat::test_that("remove_filter_state raises warning when name is not in FilterStates", {
  teal.logger::suppress_logs()

  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  fs <- filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(33, 50),
               keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var(dataname = "miniACC", varname = "gender", selected = "female",
               keep_na = FALSE, datalabel = "subjects", target = "y")
  )
  filter_states$set_filter_state(state = fs)
  testthat::expect_warning(filter_states$remove_filter_state(filter_settings(
    filter_var(dataname = "miniACC", varname = "years_to_birth2", datalabel = "subjects", target = "y")))
  )
})

# format ----
testthat::test_that("format is a method of MAEFilterStates that accepts numeric indent argument", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "iris")
  testthat::expect_no_error(shiny::isolate(filter_states$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_states$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format concatenates its FilterState elements using \\n and adds header", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = MAEFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = miniACC, dataname = "miniACC")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(30, 50),
                 keep_na = FALSE, keep_inf = FALSE, target = "y", datalabel = "subjects"),
      filter_var(dataname = "miniACC", varname = "vital_status", selected = 1,
                 keep_na = FALSE, keep_inf = FALSE, target = "y", datalabel = "subjects")
    )
  )

  for (i in 0:3) {
    states_formatted <- shiny::isolate(
      lapply(filter_states$state_list_get(1, NULL), function(x) x$format(indent = i * 2))
    )
    header <- sprintf("%sSubject filters:", format("", width = i))

    testthat::expect_identical(
      shiny::isolate(filter_states$format(indent = i)),
      paste(c(header, states_formatted), collapse = "\n")
    )
  }
})

# get_call ----
testthat::test_that("get_call returns executable subsetByColData call ", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(18, 60),
                 keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y")
    )
  )

  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(
      miniACC <- MultiAssayExperiment::subsetByColData(miniACC,
                                                       y = miniACC$years_to_birth >= 18 &
                                                         miniACC$years_to_birth <= 60)
    )
  )

  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_true(
    all(miniACC$years_to_birth >= 18 & miniACC$years_to_birth <= 60)
  )
})

# get_filter_count ----
testthat::test_that("get_filter_count returns the number of active filter states - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "miniACC", varname = "years_to_birth", selected = c(30, 50),
                 keep_na = FALSE, keep_inf = FALSE, target = "y", datalabel = "subjects"),
      filter_var(dataname = "miniACC", varname = "vital_status", selected = 1,
                 keep_na = FALSE, keep_inf = FALSE, target = "y", datalabel = "subjects")
    )
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  filter_states$remove_filter_state(filter_settings(filter_var("test", "years_to_birth")))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
