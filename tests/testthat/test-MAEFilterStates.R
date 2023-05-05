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
testthat::test_that("get_filter_state returns `teal_slices` with include_varname by default and count_type=none", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  fs <-   filter_settings(
    count_type = "none",
    include_varnames = list(miniACC = colnames(SummarizedExperiment::colData(miniACC)))
  )

  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    fs
  )
})

# get_call ----
testthat::test_that("get_call returns executable subsetByColData call with varnames prefixed by dataname$", {
  filter_states <- MAEFilterStates$new(data = miniACC, dataname = "miniACC")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(
        dataname = "miniACC", varname = "years_to_birth", selected = c(18, 60),
        keep_na = FALSE, keep_inf = FALSE, datalabel = "subjects", arg =  "y"
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

