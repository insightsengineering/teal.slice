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

# initialize ----
testthat::test_that("constructor accepts a SummarizedExperiment", {
  testthat::expect_no_error(SEFilterStates$new(data = get_test_data(), dataname = "test"))
  testthat::expect_error(
    SEFilterStates$new(data = iris, dataname = "test"),
    "Assertion on 'data' failed"
  )
})

# set_filter_state ----
testthat::test_that("set_filter_state only accepts `teal_slices`", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  fs <- teal_slices()
  testthat::expect_error(
    filter_states$set_filter_state("anything"),
    "Assertion on 'state' failed"
  )
  testthat::expect_no_error(filter_states$set_filter_state(fs))
})

testthat::test_that("set_filter_state arg - ", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  fs <- teal_slices(
    teal_slice(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), arg = "subset"),
    teal_slice(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), arg = "select")
  )
  testthat::expect_error(
    filter_states$set_filter_state(fs[[1]]),
    "Assertion on 'state' failed"
  )
  testthat::expect_no_error(filter_states$set_filter_state(fs))
})

# get_call ----
testthat::test_that("get_call returns executable subset call ", {
  test <- get_test_data()
  filter_states <- SEFilterStates$new(data = test, dataname = "test")
  fs <- teal_slices(
    teal_slice(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), arg = "subset"),
    teal_slice(dataname = "test", varname = "Treatment", selected = "ChIP", arg = "select")
  )
  filter_states$set_filter_state(fs)

  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(
      test <- subset(test, select = Treatment == "ChIP", subset = feature_id %in% c("ID001", "ID002"))
    )
  )

  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_true(
    all(rowData(test)$feature_id %in% c("ID001", "ID002"))
  )
  testthat::expect_true(
    all(colData(test)$Treatment == "ChIP")
  )
})


# ui_add ----
testthat::test_that("ui_add returns a message inside a div when data has no rows or no columns", {
  filter_states <- SEFilterStates$new(data = get_test_data(TRUE)[[1]], dataname = "test")
  testthat::expect_identical(
    filter_states$ui_add("id"),
    div(
      div("no sample variables available"),
      div("no sample variables available")
    )
  )

  filter_states <- SEFilterStates$new(data = get_test_data(TRUE)[[2]], dataname = "test")
  testthat::expect_identical(
    filter_states$ui_add("id"),
    div(
      div("no samples available"),
      div("no samples available")
    )
  )
})
