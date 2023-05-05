# initialize ----
testthat::test_that("contructor accepts a string as varlabels and keys", {
  testthat::expect_no_error(
    DFFilterStates$new(data = data.frame(), dataname = "test", varlabels = "test", keys = "test")
  )
})

testthat::test_that("constructor initializes state_list of length 1", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = DFFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = data.frame(), dataname = "test", varlabels = "test", keys = "test")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1, NULL)))
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_get(2, NULL)),
    "Filter state list 2 has not been initialized"
  )
})

# get_fun ----
testthat::test_that("get_fun returns dplyr::filter", {
  filter_states <- DFFilterStates$new(data = data.frame(), dataname = "test")
  testthat::expect_equal(filter_states$get_fun(), "dplyr::filter")
})

# set_filter_state ----
testthat::test_that("set_filter_state only accepts `teal_slices`", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )
  testthat::expect_error(filter_states$set_filter_state(state = fs[[1]]), "Assertion on 'state' failed")
  testthat::expect_no_error(filter_states$set_filter_state(state = fs))
})

testthat::test_that("set_filter_state adds states to state_list", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = DFFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )
  state_list <- shiny::isolate(filter_states$state_list_get(1, NULL))

  testthat::expect_length(state_list, 0)

  filter_states$set_filter_state(fs)
  state_list <- shiny::isolate(filter_states$state_list_get(1, NULL))

  testthat::expect_length(state_list, 2)
})

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` with features identical to those used in set_state", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )
  filter_states$set_filter_state(fs)

  fs_out <- unname(shiny::isolate(filter_states$get_filter_state()))
  testthat::expect_true(compare_slices(
    fs[[1]], fs_out[[1]],
    fields = c("dataname", "varname", "selected")
  ))
  testthat::expect_true(compare_slices(
    fs[[2]], fs_out[[2]],
    fields = c("dataname", "varname", "selected")
  ))
  testthat::skip("temporary")
  testthat::expect_equal(attributes(fs), attributes(fs_out))
})

# set_filter_state, ctd. ----
testthat::test_that("set_filter_state updates existing filter states", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  filter_states$set_filter_state(
    state = filter_settings(
      filter_var(
        dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE
      ),
      filter_var(
        dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE
      )
    )
  )
  filter_states$set_filter_state(
    state = filter_settings(
      filter_var(
        dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5.0), keep_na = FALSE, keep_inf = FALSE
      ),
      filter_var(dataname = "iris", varname = "Species", selected = "setosa")
    )
  )

  fs_expect <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = "setosa", keep_na = FALSE),
    filter_var(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5.0), keep_na = FALSE, keep_inf = FALSE)
  )
  fs_out <- unname(shiny::isolate(filter_states$get_filter_state()))

  testthat::expect_true(compare_slices(
    fs_expect[[1]], fs_out[[1]],
    fields = c("dataname", "varname", "selected", "keep_na", "keep_inf")
  ))
  testthat::expect_true(compare_slices(
    fs_expect[[2]], fs_out[[2]],
    fields = c("dataname", "varname", "selected", "keep_na")
  ))
  testthat::expect_true(compare_slices(
    fs_expect[[3]], fs_out[[3]],
    fields = c("dataname", "varname", "selected", "keep_na", "keep_inf")
  ))
  testthat::skip("temporary")
  testthat::expect_identical(attributes(fs_expect), attributes(fs_out))
})

testthat::test_that("set_filter_state sets sid after state_list_index and varname", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = DFFilterStates,
    public = list(
      get_filter_states_sid = function() {
        vapply(
          private$state_list[[1L]](),
          FUN = attr,
          which = "sid",
          FUN.VALUE = character(1)
        )
      }
    )
  )
  filter_states <- test_class$new(data = iris, dataname = "iris")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
      filter_var(dataname = "iris", varname = "Petal.Length", selected = c(1.5, 6.9))
    )
  )
  testthat::expect_true(
    all(grepl(
      "1-(Sepal.Length|Petal.Length)-[0-9]+$",
      shiny::isolate(filter_states$get_filter_states_sid())
    ))
  )
})

# remove_filter_state ----
testthat::test_that("remove_filter_state removes filters specified by `teal_slices`", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )
  filter_states$set_filter_state(state = fs)
  testthat::expect_error(
    filter_states$remove_filter_state(list(filter_var(dataname = "iris", varname = "Species"))),
    "Assertion on 'state' failed"
  )
  testthat::expect_no_error(
    filter_states$remove_filter_state(filter_settings(filter_var(dataname = "iris", varname = "Species")))
  )
  testthat::expect_identical(
    slices_field(shiny::isolate(filter_states$get_filter_state()), "varname"),
    "Sepal.Length"
  )
})

testthat::test_that("remove_filter_state raises warning when name is not in FilterStates", {
  teal.logger::suppress_logs()
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )
  shiny::isolate(filter_states$set_filter_state(state = fs))

  testthat::expect_warning(
    shiny::isolate(filter_states$remove_filter_state(
      filter_settings(filter_var(dataname = "iris", varname = "Species2"))
    )),
    "not found in state list"
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
testthat::test_that("get_call returns call composed of conditions of all existing filter states", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE)
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

testthat::test_that("get_call skips conditions form FilterState which are identified by sid", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = DFFilterStates,
    public = list(
      get_filter_states_sid = function() {
        vapply(
          private$state_list[[1L]](),
          FUN = attr,
          which = "sid",
          FUN.VALUE = character(1)
        )
      }
    )
  )
  filter_states <- test_class$new(data = iris, dataname = "iris")
  filter_states$set_filter_state(
    filter_settings(
      filter_var(
        dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE
      ),
      filter_var(
        dataname = "iris", varname = "Petal.Length", selected = c(1.5, 6.9), keep_na = FALSE, keep_inf = FALSE
      ),
      filter_var(
        dataname = "iris", varname = "Species", selected = "setosa", keep_na = FALSE, keep_inf = FALSE
      )
    )
  )
  sid_attrs <- unname(shiny::isolate(filter_states$get_filter_states_sid()))
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call(sid = sid_attrs[1])),
    quote(
      iris <- dplyr::filter(iris, Petal.Length >= 1.5 & Petal.Length <= 6.9 & Species == "setosa")
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_states$get_call(sid = sid_attrs[2])),
    quote(
      iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4 & Species == "setosa")
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_states$get_call(sid = sid_attrs[3])),
    quote(
      iris <- dplyr::filter(
        iris,
        Sepal.Length >= 5.1 & Sepal.Length <= 6.4 & (Petal.Length >= 1.5 & Petal.Length <= 6.9)
      )
    )
  )
  testthat::expect_null(
    shiny::isolate(filter_states$get_call(sid = sid_attrs))
  )
})


# get_filter_count ----
testthat::test_that("get_filter_count returns the number of active filter states - DFFilterStates", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 0)
  filter_states$set_filter_state(
    filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
      filter_var(dataname = "iris", varname = "Petal.Length", selected = c(1.5, 6.9))
    )
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  filter_states$remove_filter_state(filter_settings(filter_var(dataname = "iris", varname = "Petal.Length")))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})


# ui_add ----
testthat::test_that("ui_add returns a message inside a div when data has no columns or no rows", {
  filter_states <- DFFilterStates$new(data = data.frame(), dataname = "iris")
  testthat::expect_identical(
    filter_states$ui_add("id"),
    div("no sample variables available")
  )
})


# UI actions ----
testthat::test_that("selecting a new variable initializes a new filter state", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")
  shiny::testServer(
    filter_states$srv_add,
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )

  fs_expect <- filter_settings(filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(4.3, 7.9)))
  fs_out <- unname(shiny::isolate(filter_states$get_filter_state()))

  testthat::expect_true(compare_slices(fs_expect[[1]], fs_out[[1]], fields = c("dataname", "varname", "selected")))
  testthat::skip("temporary")
  testthat::expect_identical(attributes(fs_expect), attributes(fs_out))
})

testthat::test_that("adding 'var_to_add' adds another filter state", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "iris")

  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE)
  )
  shiny::isolate(filter_states$set_filter_state(state = fs))
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 1L)
  shiny::testServer(
    filter_states$srv_add,
    expr = {
      session$setInputs(var_to_add = "Petal.Length")
    }
  )
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 2L)
  shiny::testServer(
    filter_states$srv_add,
    expr = {
      session$setInputs(var_to_add = "Species")
    }
  )
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 3L)
})
