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

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` with include_varname by default", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  fs <-   filter_settings(
    count_type = "all",
    include_varnames = list(miniACC = colnames(colData(miniACC)))
  )

  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    fs
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
      filter_var(
        dataname = "miniACC", varname = "years_to_birth", selected = c(30, 50),
        keep_na = FALSE, keep_inf = FALSE, target = "y", datalabel = "subjects"
      ),
      filter_var(
        dataname = "miniACC", varname = "vital_status", selected = 1,
        keep_na = FALSE, keep_inf = FALSE, target = "y", datalabel = "subjects"
      )
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
testthat::test_that("get_call returns executable subsetByColData call with varnames prefixed by dataname$", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(
        dataname = "miniACC", varname = "years_to_birth", selected = c(18, 60),
        keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", target = "y"
      )
    )
  )

  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(
      miniACC <- MultiAssayExperiment::subsetByColData(miniACC,
        y = miniACC$years_to_birth >= 18 &
          miniACC$years_to_birth <= 60
      )
    )
  )

  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_true(
    all(miniACC$years_to_birth >= 18 & miniACC$years_to_birth <= 60)
  )
})

