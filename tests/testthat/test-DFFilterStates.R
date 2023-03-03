testthat::test_that("The contructor accepts a string as varlabels and keys", {
  testthat::expect_no_error(DFFilterStates$new(
    dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  ))
})

testthat::test_that("get_fun returns dplyr::filter", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  )
  testthat::expect_equal(filter_states$get_fun(), "dplyr::filter")
})

testthat::test_that(
  "DFFilterStates$set_filter_state sets filters in FilterState(s) specified by the named list",
  code = {
    dffs <- DFFilterStates$new(
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    shiny::isolate(dffs$set_filter_state(state = fs, data = iris))
    testthat::expect_equal(
      shiny::isolate(dffs$get_call()),
      quote(
        iris <- dplyr::filter(
          iris,
          Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
            Species %in% c("setosa", "versicolor")
        )
      )
    )
  }
)

testthat::test_that("DFFilterStates$set_filter_state sets filters as a named/unnamed selected list", {
  dffs <- DFFilterStates$new(
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  fs <- list(
    Sepal.Length = list(c(5.1, 6.4)),
    Species = list(selected = c("setosa", "versicolor"))
  )
  shiny::isolate(dffs$set_filter_state(state = fs, data = iris))
  testthat::expect_equal(
    shiny::isolate(dffs$get_call()),
    quote(
      iris <- dplyr::filter(
        iris,
        Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
          Species %in% c("setosa", "versicolor")
      )
    )
  )
})

testthat::test_that("DFFilterStates$set_filter_state updates filter state which was set already", {
  dffs <- DFFilterStates$new(
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  shiny::isolate(dffs$set_filter_state(
    state = list(Sepal.Length = c(5.1, 6.4), Species = c("setosa", "versicolor")),
    data = iris
  ))
  shiny::isolate(dffs$set_filter_state(
    state = list(Species = "setosa", Petal.Length = c(2.0, 5.0)),
    data = iris
  ))
  testthat::expect_identical(
    shiny::isolate(dffs$get_filter_state()),
    list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      Species = list(selected = "setosa", keep_na = FALSE),
      Petal.Length = list(selected = c(2.0, 5.0), keep_na = FALSE, keep_inf = FALSE)
    )
  )
})

testthat::test_that(
  "DFFilterStates$set_filter_state throws error when using an unnamed list",
  code = {
    dffs <- DFFilterStates$new(
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    testthat::expect_error(dffs$set_filter_state(state = fs, data = iris))
  }
)

testthat::test_that(
  "DFFilterStates$get_filter_state returns list identical to input",
  code = {
    dffs <- DFFilterStates$new(
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
      Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    shiny::isolate(dffs$set_filter_state(state = fs, data = iris))
    testthat::expect_identical(shiny::isolate(dffs$get_filter_state()), fs)
  }
)

testthat::test_that("Selecting a new variable initializes a new filter state", {
  dffs <- DFFilterStates$new(
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  expect_null(
    shiny::isolate(dffs$state_list_get(state_list_index = 1, state_id = "Sepal.Length"))
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )

  testthat::expect_is(
    shiny::isolate(dffs$state_list_get(state_list_index = 1)),
    "list"
  )
  testthat::expect_is(
    shiny::isolate(dffs$state_list_get(state_list_index = 1, state_id = "Sepal.Length")),
    "RangeFilterState"
  )
  testthat::expect_identical(
    shiny::isolate(dffs$state_list_get(state_list_index = 1, state_id = "Sepal.Length"))$get_varname(),
    "Sepal.Length"
  )
})

testthat::test_that("Adding 'var_to_add' adds another filter state", {
  dffs <- DFFilterStates$new(
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )

  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4))
  )

  isolate(dffs$set_filter_state(state = fs, data = iris))

  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Species")
    }
  )

  testthat::expect_identical(
    shiny::isolate(dffs$get_call()),
    quote(iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4))
  )
})

testthat::test_that(
  "DFFilterStates$remove_filter_state removes specified filter in FilterState(s)",
  code = {
    dffs <- DFFilterStates$new(
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4)),
      Species = list(selected = c("setosa", "versicolor"))
    )

    shiny::isolate(dffs$set_filter_state(state = fs, data = iris))
    shiny::isolate(dffs$remove_filter_state("Species"))

    testthat::expect_equal(
      shiny::isolate(dffs$get_call()),
      quote(iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4))
    )
  }
)

testthat::test_that(
  "DFFilterStates$remove_filter_state throws warning when name is not in FilterStates",
  code = {
    teal.logger::suppress_logs()
    dffs <- DFFilterStates$new(
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4)),
      Species = list(selected = c("setosa", "versicolor"))
    )

    shiny::isolate(dffs$set_filter_state(state = fs, data = iris))
    testthat::expect_warning(shiny::isolate(dffs$remove_filter_state("Species2")))
  }
)

testthat::test_that(
  "DFFilterStates$ui_add_filter_state returns a message inside a div when data has no columns or no rows",
  code = {
    dffs <- DFFilterStates$new(
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    testthat::expect_identical(
      dffs$ui_add_filter_state("id", data.frame()),
      div("no sample variables available")
    )
    testthat::expect_identical(
      dffs$ui_add_filter_state("id", data.frame(A = numeric(0))),
      div("no samples available")
    )
  }
)

# Format
testthat::test_that("$format() is a method of DFFilterStates", {
  dffs <- DFFilterStates$new(
    dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  )
  testthat::expect_no_error(shiny::isolate(
    dffs$format()
  ))
})

testthat::test_that("$format() asserts the indent argument is a number", {
  dffs <- DFFilterStates$new(
    dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  )
  testthat::expect_error(
    dffs$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() concatenates its FilterState elements using \\n without additional indent", {
  dffs <- DFFilterStates$new(
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  shiny::isolate(dffs$set_filter_state(state = fs, data = iris))

  sepal_filter <- shiny::isolate(dffs$state_list_get(1L)[[1]])
  species_filter <- shiny::isolate(dffs$state_list_get(1L)[[2]])
  shiny::isolate(
    testthat::expect_equal(
      dffs$format(),
      paste(sepal_filter$format(indent = 0), species_filter$format(indent = 0), sep = "\n")
    )
  )
})

testthat::test_that(
  "DFFilterStates$set_filter_state sets sid after state_list_index and varname",
  code = {
    testFS <- R6::R6Class(
      "testFS",
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
    filter_states <- testFS$new(
      data = iris,
      dataname = "iris"
    )
    filter_states$set_filter_state(
      list(
        Sepal.Length = c(5.1, 6.4),
        Petal.Length = c(1.5, 6.9)
      )
    )
    testthat::expect_true(
      all(grepl(
        "1-(Sepal.Length|Petal.Length)-[0-9]+$",
        shiny::isolate(filter_states$get_filter_states_sid())
      ))
    )
  }
)

testthat::test_that(
  "DFFilterState$get_call skips conditions form FilterState which are identified by sid",
  code = {
    testFS <- R6::R6Class(
      "testFS",
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
    filter_states <- testFS$new(data = iris, dataname = "iris")
    filter_states$set_filter_state(
      list(
        Sepal.Length = c(5.1, 6.4),
        Petal.Length = c(1.5, 6.9),
        Species = "setosa"
      )
    )
    sid_attrs <- unname(shiny::isolate(filter_states$get_filter_states_sid()))
    testthat::expect_identical(
      shiny::isolate(filter_states$get_call(sid = sid_attrs[1])),
      quote(
        iris <- dplyr::filter(iris, Petal.Length >= 1.5 & Petal.Length <= 6.9 & Species == "setosa")
      )
    )
    testthat::expect_identical(
      shiny::isolate(filter_states$get_call(sid = sid_attrs[2])),
      quote(
        iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4 & Species == "setosa")
      )
    )
    testthat::expect_identical(
      shiny::isolate(filter_states$get_call(sid = sid_attrs[3])),
      quote(
        iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4 & (Petal.Length >= 1.5 & Petal.Length <= 6.9))
      )
    )
    testthat::expect_null(
      shiny::isolate(filter_states$get_call(sid = sid_attrs))
    )
  }
)
