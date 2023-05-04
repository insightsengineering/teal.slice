# initialize ----
testthat::test_that("contructor accepts a string as varlabels and keys", {
  testthat::expect_no_error(
    DFFilterStates$new(data = data.frame(), dataname = "test", varlabels = "test", keys = "test")
  )
})

# get_filter_state ----
testthat::test_that("get_filter_state return `teal_slices` with include_varnames by default", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    count_type = "all",
    include_varnames = list(iris = colnames(iris))
  )
  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    fs
  )
})


# format ----
testthat::test_that("format is a method of DFFilterStates that accepts numeric indent argument", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "test", varlabels = "test", keys = "test")
  testthat::expect_no_error(shiny::isolate(filter_states$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_states$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format concatenates its FilterState elements using \\n without additional indent", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = DFFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  filter_states$set_filter_state(state = fs)

  for (i in 1:3) {
    states_formatted <- shiny::isolate(
      lapply(filter_states$state_list_get(1, NULL), function(x) x$format(indent = i))
    )

    testthat::expect_identical(
      shiny::isolate(filter_states$format(indent = i)),
      paste(states_formatted, collapse = "\n")
    )
  }
})


# get_call ----
testthat::test_that("get_call returns filter call on dataname with unprefixed variables in logical expression", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )
  filter_states$set_filter_state(fs)
  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(
      iris <- dplyr::filter(
        iris,
        Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
          Species %in% c("setosa", "versicolor")
      )
    )
  )
})
