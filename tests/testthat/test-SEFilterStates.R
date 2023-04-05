get_test_data <- function(no_data = FALSE) {
  library(SummarizedExperiment)
  nrows <- 200
  ncols <- 6
  counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
  row_ranges <- GRanges(
    rep(c("chr1", "chr2"), c(50, 150)),
    IRanges(floor(runif(200, 1e5, 1e6)), width = 100),
    strand = sample(c("+", "-"), 200, TRUE),
    feature_id = sprintf("ID%03d", 1:200)
  )
  cdata <- DataFrame(
    Treatment = rep(c("ChIP", "Input"), 3),
    row.names = LETTERS[1:6]
  )

  if (no_data) {
    list(
      SummarizedExperiment(
        rowData = data.frame(),
        colData = data.frame()
      ),
      SummarizedExperiment(
        rowData = data.frame(A = numeric(0)),
        colData = data.frame(A = numeric(0))
      )
    )
  } else {
    obj <- SummarizedExperiment(
      assays = list(counts = counts),
      rowRanges = row_ranges,
      colData = cdata
    )
  }
}

testthat::test_that("The constructor does not throw", {
  testthat::expect_no_error(SEFilterStates$new(
    data = get_test_data(),
    dataname = "test"
  ))
})

testthat::test_that("The constructor initializes two state_lists", {
  testfs <- R6::R6Class(
    classname = "testfs",
    inherit = SEFilterStates,
    public = list(
      state_list_get = function(x) private$state_list_get(x)
    )
  )
  filter_states <- testfs$new(data = get_test_data(), dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1)))
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(2)))
})

testthat::test_that("set_filter_state throws error when input is missing", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_error(filter_states$set_filter_state(), "argument \"state\" is missing, with no default")
})

testthat::test_that("set_filter_state throws error when state argument is not a list", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_error(filter_states$set_filter_state(
    state = filter_settings(c(dataname = "test", A = "test", varname = "feature_id",  select = "B"))),
    "Assertion on 'slices' failed: May only contain")
  testthat::expect_error(filter_states$set_filter_state(
    state = c(filter_var(dataname = "test", A = "test", varname = "feature_id",  select = "B"))),
    "Assertion on 'state' failed: Must inherit from class 'teal_slices'")
})

## acceptable inputs to set_filter_state
testthat::test_that(
  "set_filter_state returns NULL when state argument contains subset and select set as NULL",
  code = {
    filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
    testthat::expect_null(
      filter_states$set_filter_state(state = filter_settings(filter_var(
        subset = NULL, select = NULL, dataname = "test", varname = "feature_id", target = "subset"))
      )
    )
  }
)


testthat::test_that("get_call method returns NULL if no states added", {
  se <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_null(shiny::isolate(se$get_call()))
})

testthat::test_that("get_fun method returns subset", {
  se <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_equal(se$get_fun(), "subset")
})

testthat::test_that("SEFilterStates$set_filter_state sets state with only subset", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(data = obj, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset")
  )
  sefs$set_filter_state(state = fs)
  testthat::expect_equal(
    shiny::isolate(sefs$get_call()),
    quote(test <- subset(test, subset = feature_id %in% c("ID001", "ID002")))
  )
})

testthat::test_that("SEFilterStates$set_filter_state updates select state which has been set already", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(data = obj, dataname = "test")
  sefs$set_filter_state(
    state = filter_settings(filter_var(selected = c("ChIP", "Input"), dataname = "test",
                                       varname = "Treatment", target = "select"))
  )
  sefs$set_filter_state(state = filter_settings(
    filter_var(selected = "ChIP", dataname = "test", varname = "Treatment", target = "select")
  ))
  testthat::expect_equal(shiny::isolate(sefs$get_filter_state())$Treatment$selected, "ChIP")
})


testthat::test_that("SEFilterStates$set_filter_state updates subset state which has been set already", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(data = obj, dataname = "test")
  sefs$set_filter_state(
    state = filter_settings(filter_var(selected = c("ID001", "ID002"), dataname = "test",
                                       varname = "feature_id", target = "subset"))
  )
  sefs$set_filter_state(state = filter_settings(
    filter_var(selected = "ID001", dataname = "test", varname = "feature_id", target = "subset")
  ))
  testthat::expect_equal(shiny::isolate(sefs$get_filter_state())$feature_id$selected, "ID001")
})


testthat::test_that("SEFilterStates$get_filter_state returns list as in input", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(data = obj, dataname = "test")
  fs <- filter_settings(
    filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
               varname = "feature_id", target = "subset", keep_na = TRUE),
    filter_var(selected = c("ChIP"), choices = c("ChIP", "Input"), dataname = "test",
               varname = "Treatment", target = "select", keep_na = FALSE)
  )
  sefs$set_filter_state(state = fs)

  testthat::expect_identical(shiny::isolate(sefs$get_filter_state())[[1]], fs[[1]])
  testthat::expect_identical(shiny::isolate(sefs$get_filter_state())[[2]], fs[[2]])
})

testthat::test_that("SEFilterStates$remove_filter_state removes filters in filter_settings", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(data = obj, dataname = "test")

  fs <- filter_settings(
    filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
               varname = "feature_id", target = "subset", keep_na = FALSE),
    filter_var(selected = c("ChIP"), choices = c("ChIP", "Input"), dataname = "test",
               varname = "Treatment", target = "select", keep_na = FALSE)
  )

  sefs$set_filter_state(state = fs)
  shiny::isolate(
    sefs$remove_filter_state(state = filter_settings(filter_var(
      selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
      varname = "feature_id", target = "subset", keep_na = FALSE
      )))
  )

  eval(shiny::isolate(sefs$get_call()))
  testthat::expect_equal(
    test, subset(obj, select = Treatment == "ChIP")
  )
})


testthat::test_that("SEFilterStates$remove_filter_state removes all filters in state_list", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(data = obj, dataname = "test")

  fs <- filter_settings(
    filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
               varname = "feature_id", target = "subset", keep_na = TRUE),
    filter_var(selected = c("ChIP"), choices = c("ChIP", "Input"), dataname = "test",
               varname = "Treatment", target = "select", keep_na = FALSE)
  )

  sefs$set_filter_state(state = fs)

  sefs$set_filter_state(state = fs)
  shiny::isolate(sefs$remove_filter_state(fs))

  eval(shiny::isolate(sefs$get_call()))
  testthat::expect_equal(obj, test)
})

testthat::test_that(
  "SEFilterStates$remove_filter_state throws warning when list has unknown name in the FilterState",
  code = {
    # teal.logger::suppress_logs()
    # obj <- get_test_data()
    # test <- obj
    # sefs <- SEFilterStates$new(data = obj, dataname = "test")
    #
    # fs <- filter_settings(
    #   filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
    #              varname = "feature_id", target = "subset", keep_na = TRUE)
    # )
    #
    # sefs$set_filter_state(state = fs)
    # testthat::expect_warning(
    #   shiny::isolate(sefs$remove_filter_state(
    #     filter_settings(filter_var(
    #       selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
    #       varname = "wwww", target = "subset", keep_na = TRUE))
    #     )
    #   )
    # )
  }
)

testthat::test_that(
  "SEFilterStates$ui_add returns a message inside a div when data has no rows or no columns",
  code = {
    sefs <- SEFilterStates$new(data = get_test_data(TRUE)[[1]], dataname = "test")
    testthat::expect_identical(
      sefs$ui_add("id"),
      div(
        div("no sample variables available"),
        div("no sample variables available")
      )
    )

    sefs <- SEFilterStates$new(data = get_test_data(TRUE)[[2]], dataname = "test")
    testthat::expect_identical(
      sefs$ui_add("id"),
      div(
        div("no samples available"),
        div("no samples available")
      )
    )
  }
)

# Format
testthat::test_that("$format() is a method of SEFilterStates", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(data = obj, dataname = "test")
  testthat::expect_no_error(
    shiny::isolate(sefs$format())
  )
})

testthat::test_that("$format() asserts the indent argument is a number", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(data = obj, dataname = "test")
  testthat::expect_error(
    sefs$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that(
  "$format() concatenates its FilterState elements using \\n and indents the FilterState strings",
  code = {
    test <- get_test_data()
    sefs <- SEFilterStates$new(data = test, dataname = "test", datalabel = "Label")

    fs <- filter_settings(
      filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
                 varname = "feature_id", target = "subset", keep_na = FALSE),
      filter_var(selected = c("ChIP"), choices = c("ChIP", "Input"), dataname = "test",
                 varname = "Treatment", target = "select", keep_na = FALSE)
    )
    sefs$set_filter_state(state = fs)

    testthat::expect_equal(
      shiny::isolate(sefs$format()),
      paste(
        c(
          "Assay Label filters:",
          "  Subsetting:",
          "    Filtering on: feature_id",
          "      Selected values: ID001, ID002",
          "      Include missing values: FALSE",
          "  Selecting:",
          "    Filtering on: Treatment",
          "      Selected values: ChIP",
          "      Include missing values: FALSE"
        ),
        collapse = "\n"
      )
    )
  }
)

testthat::test_that("get_filter_count properly tallies multiple state lists - SEFilterStates", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test", datalabel = "test")

  fs <- filter_settings(
    filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
               varname = "feature_id", target = "subset", keep_na = FALSE),
    filter_var(selected = c("ChIP"), choices = c("ChIP", "Input"), dataname = "test",
               varname = "Treatment", target = "select", keep_na = FALSE)
  )
  filter_states$set_filter_state(state = fs)
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  shiny::isolate(filter_states$remove_filter_state(filter_settings(
    filter_var(selected = c("ID001", "ID002"), choices = c("ID001", "ID002"), dataname = "test",
               varname = "feature_id", target = "subset", keep_na = FALSE)
  )))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
