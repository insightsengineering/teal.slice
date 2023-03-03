testthat::test_that("The contructor accepts a string as varlabels and keys", {
  testthat::expect_no_error(DFFilterStates$new(
    data = data.frame(),
    dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  ))
})

testthat::test_that("get_fun returns dplyr::filter", {
  filter_states <- DFFilterStates$new(
    data = data.frame(),
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
      data = iris,
      dataname = "iris",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    dffs$set_filter_state(state = fs)
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
    data = iris,
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  fs <- list(
    Sepal.Length = list(c(5.1, 6.4)),
    Species = list(selected = c("setosa", "versicolor"))
  )
  dffs$set_filter_state(state = fs)
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
    data = iris,
    dataname = "iris",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  dffs$set_filter_state(
    state = list(Sepal.Length = c(5.1, 6.4), Species = c("setosa", "versicolor"))
  )
  dffs$set_filter_state(
    state = list(Species = "setosa", Petal.Length = c(2.0, 5.0))
  )
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
    dffs <- DFFilterStates$new(data = iris, dataname = "iris")
    fs <- list(
      c(5.1, 6.4),
      c("setosa", "versicolor")
    )
    testthat::expect_error(dffs$set_filter_state(state = fs))
  }
)

testthat::test_that(
  "DFFilterStates$get_filter_state returns list identical to input",
  code = {
    dffs <- DFFilterStates$new(data = iris, dataname = "iris")
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
      Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dffs$set_filter_state(state = fs)
    testthat::expect_identical(shiny::isolate(dffs$get_filter_state()), fs)
  }
)

testthat::test_that("Selecting a new variable initializes a new filter state", {
  dffs <- DFFilterStates$new(data = iris, dataname = "iris")
  shiny::testServer(
    dffs$srv_add_filter_state,
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )
  testthat::expect_identical(
    shiny::isolate(dffs$get_filter_state()),
    list(
      Sepal.Length = list(selected = c(4.3, 7.9), keep_na = FALSE, keep_inf = FALSE)
    )
  )
})

testthat::test_that("Adding 'var_to_add' adds another filter state", {
  dffs <- DFFilterStates$new(data = iris, dataname = "iris")

  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4))
  )
  shiny::isolate(dffs$set_filter_state(state = fs))
  shiny::testServer(
    dffs$srv_add_filter_state,
    expr = {
      session$setInputs(var_to_add = "Petal.Length")
    }
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    expr = {
      session$setInputs(var_to_add = "Species")
    }
  )

  testthat::expect_identical(
    shiny::isolate(dffs$get_filter_state()),
    list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      Petal.Length = list(selected = c(1.0, 6.9), keep_na = FALSE, keep_inf = FALSE),
      Species = list(selected = c("setosa", "versicolor", "virginica"), keep_na = FALSE)
    )
  )
})

testthat::test_that(
  "DFFilterStates$remove_filter_state removes specified filter in FilterState(s)",
  code = {
    dffs <- DFFilterStates$new(data = iris, dataname = "iris")
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4)),
      Species = list(selected = c("setosa", "versicolor"))
    )

    dffs$set_filter_state(state = fs)
    dffs$remove_filter_state("Species")

    testthat::expect_identical(
      shiny::isolate(dffs$get_filter_state()),
      list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE)
      )
    )
  }
)

testthat::test_that(
  "DFFilterStates$remove_filter_state throws warning when name is not in FilterStates",
  code = {
    teal.logger::suppress_logs()
    dffs <- DFFilterStates$new(data = iris, dataname = "iris")
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4)),
      Species = list(selected = c("setosa", "versicolor"))
    )

    shiny::isolate(dffs$set_filter_state(state = fs))
    testthat::expect_warning(shiny::isolate(dffs$remove_filter_state("Species2")))
  }
)

testthat::test_that(
  "DFFilterStates$ui_add_filter_state returns a message inside a div when data has no columns or no rows",
  code = {
    dffs <- DFFilterStates$new(data = data.frame(), dataname = "iris")
    testthat::expect_identical(
      dffs$ui_add_filter_state("id"),
      div("no sample variables available")
    )
  }
)

# Format
testthat::test_that("$format() is a method of DFFilterStates", {
  dffs <- DFFilterStates$new(
    data = iris,
    dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  )
  testthat::expect_no_error(shiny::isolate(dffs$format()))
})

testthat::test_that("$format() asserts the indent argument is a number", {
  dffs <- DFFilterStates$new(
    data = iris,
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
  dffs <- DFFilterStates$new(data = iris, dataname = "iris")
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
  )
  dffs$set_filter_state(state = fs)

  testthat::expect_equal(
    shiny::isolate(dffs$format()),
    paste(
      c(
        "Filtering on: Sepal.Length",
        "  Selected range: 5.100 - 6.400",
        "  Include missing values: TRUE",
        "Filtering on: Species",
        "  Selected values: setosa, versicolor",
        "  Include missing values: FALSE"
      ),
      collapse = "\n"
    )
  )
})

testthat::test_that(
  "DFFilterStates$set_filter_state sets sid after state_list_index and varname",
  code = {
    testfs <- R6::R6Class(
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
    filter_states <- testfs$new(
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
    testfs <- R6::R6Class(
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
    filter_states <- testfs$new(data = iris, dataname = "iris")
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
  }
)

testthat::test_that("get_filter_count returns the number of active filter states - DFFilterStates", {
  filter_states <- DFFilterStates$new(data = iris, dataname = "test")
  filter_states$set_filter_state(
    list(
      Sepal.Length = c(5.1, 6.4),
      Petal.Length = c(1.5, 6.9)
    )
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  filter_states$remove_filter_state(state_id = "Petal.Length")
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
