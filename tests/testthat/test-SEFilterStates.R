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

testthat::test_that("constructor initializes state_list with two elements named 'subset' and 'select'", {
  test_class <- R6::R6Class(
    classname = "filter_states",
    inherit = SEFilterStates,
    public = list(
        state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = get_test_data(), dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(1, NULL)))
  testthat::expect_no_error(shiny::isolate(filter_states$state_list_get("subset", NULL)))
  testthat::expect_null(shiny::isolate(filter_states$state_list_get(2, NULL)))
  testthat::expect_no_error(shiny::isolate(filter_states$state_list_get("select", NULL)))
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_get(3, NULL)),
    "Filter state list 3 has not been initialized"
  )
})

# get_fun ----
testthat::test_that("get_fun method returns subset", {
  se <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_equal(se$get_fun(), "subset")
})

# set_filter_state ----
testthat::test_that("set_filter_state only accepts `teal_slices`", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), target = "select")
  )
  testthat::expect_error(
    filter_states$set_filter_state(fs[[1]]),
    "Assertion on 'state' failed"
  )
  testthat::expect_no_error(filter_states$set_filter_state(fs))
})

testthat::test_that("set_filter_state adds states to state_list", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = SEFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = get_test_data(), dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), target = "select")
  )

  state_list_subset <- shiny::isolate(filter_states$state_list_get("subset", NULL))
  testthat::expect_length(state_list_subset, 0)
  state_list_select <- shiny::isolate(filter_states$state_list_get("select", NULL))
  testthat::expect_length(state_list_select, 0)

  filter_states$set_filter_state(fs)

  state_list_subset <- shiny::isolate(filter_states$state_list_get("subset", NULL))
  testthat::expect_length(state_list_subset, 1)
  state_list_select <- shiny::isolate(filter_states$state_list_get("select", NULL))
  testthat::expect_length(state_list_select, 1)
})

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` identical to that used to set state (choices excluded)", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), keep_na = FALSE,
               target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), keep_na = FALSE,
               target = "select")
  )
  filter_states$set_filter_state(state = fs)

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
})

# set_filter_state, ctd. ----
testthat::test_that("set_filter_state updates select state which has been set already", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  filter_states$set_filter_state(
    state = filter_settings(
      filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset"),
      filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), target = "select")
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), target = "select")
  )
  filter_states$set_filter_state(fs)

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
})


# remove_filter_state ----
testthat::test_that("remove_filter_state removes filters specified by `teal_slices`", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP", "Input"), target = "select")
  )
  filter_states$set_filter_state(state = fs)
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 2)
  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs
  )
  shiny::isolate(
    filter_states$remove_filter_state(filter_settings(filter_var(dataname = "test", varname = "feature_id")))
  )
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 1)

  testthat::expect_identical(
    adjust_states(shiny::isolate(filter_states$get_filter_state())),
    fs[2]
  )
})

testthat::test_that("remove_filter_state raises warning when non-existent filter state specified", {
  teal.logger::suppress_logs()
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", target = "subset")
  )
  filter_states$set_filter_state(state = fs)
  testthat::expect_warning(
    shiny::isolate(filter_states$remove_filter_state(
      filter_settings(
        filter_var(dataname = "test", varname = "feature_ID", target = "subset")
      )
    )),
    "no filter states to remove"
  )
})

# format ----
testthat::test_that("format is a method of MAEFilterStates that accepts numeric indent argument", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_no_error(shiny::isolate(filter_states$format(indent = 0)))
  testthat::expect_error(shiny::isolate(filter_states$format(indent = "0")), "Assertion on 'indent' failed")
})

testthat::test_that("format concatenates its FilterState elements using \\n and adds header", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = SEFilterStates,
    public = list(
      state_list_get = function(state_list_index, state_id) private$state_list_get(state_list_index, state_id)
    )
  )
  filter_states <- test_class$new(data = get_test_data(), dataname = "test", datalabel = "Label")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), keep_na = FALSE,
               target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = c("ChIP"), keep_na = FALSE,
               target = "select")
  )
  filter_states$set_filter_state(state = fs)

  for (i in 0:3) {
    states_formatted_subset <- shiny::isolate(
      lapply(filter_states$state_list_get("subset", NULL), function(x) x$format(indent = i * 2))
    )
    states_formatted_select <- shiny::isolate(
      lapply(filter_states$state_list_get("select", NULL), function(x) x$format(indent = i * 2))
    )
    header1 <- sprintf("%sAssay Label filters:", format("", width = i))
    header2 <- sprintf("%sSubsetting:", format("", width = i * 2))
    header3 <- sprintf("%sSelecting:", format("", width = i * 2))

    testthat::expect_identical(
      shiny::isolate(filter_states$format(indent = i)),
      paste(c(header1, header2, states_formatted_subset, header3, states_formatted_select), collapse = "\n")
    )
  }
})

# get_call ----
testthat::test_that("get_call returns NULL if no states added", {
  filter_states <- SEFilterStates$new(data = get_test_data(), dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$get_call()))
})

testthat::test_that("get_call returns executable subset call ", {
  test <- get_test_data()
  filter_states <- SEFilterStates$new(data = test, dataname = "test")
  fs <- filter_settings(
    filter_var(dataname = "test", varname = "feature_id", selected = c("ID001", "ID002"), target = "subset"),
    filter_var(dataname = "test", varname = "Treatment", selected = "ChIP", target = "select")
  )
  filter_states$set_filter_state(fs)

  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    quote(
      test <- subset(test, subset = feature_id %in% c("ID001", "ID002"), select = Treatment == "ChIP")
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

# get_filter_count ----
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
