# Notice, the particular design of these tests. We don't test the particulars of
# the calls, but only whether they evaluate to the expected value.

# constructor ----
testthat::test_that("The constructor accepts only a string as dataname", {
  testthat::expect_no_error(
    FilterStates$new(dataname = "string", datalabel = "test")
  )
  testthat::expect_error(
    FilterStates$new(dataname = quote(name), datalabel = "test")
  )
  testthat::expect_error(
    FilterStates$new(dataname = call("call"), datalabel = "test")
  )
})

# validate_state_list_exists ----
testthat::test_that("validate_state_list_exists raises errors if no filters were added", {
  filter_states <- FilterStates$new(dataname = "test", "test")
  filter_state <- FilterState$new("test", varname = "test")
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_get(1, 1)),
    regexp = "Filter state list 1 has not been initialized in FilterStates object belonging to the dataset test"
  )
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_push(filter_state, 1, 1)),
    regexp = "Filter state list 1 has not been initialized in FilterStates object belonging to the dataset test"
  )
  testthat::expect_error(
    shiny::isolate(filter_states$state_list_remove(1, 1)),
    regexp = "Filter state list 1 has not been initialized in FilterStates object belonging to the dataset test"
  )
})

testthat::test_that("Emptying empty FilterStates does not raise errors", {
  filter_states <- FilterStates$new(dataname = "test", "test")
  testthat::expect_no_error(filter_states$state_list_empty())
})

testthat::test_that("Error message from validate_state_list_exists displays index if datalabel is character(0)", {
  filter_states <- FilterStates$new(dataname = "test", datalabel = character(0))
  testthat::expect_error(
    filter_states$state_list_get(7),
    regexp = "Filter state list 7 has not been initialized in FilterStates object belonging to the dataset"
  )
})


# state_list_push ----
testthat::test_that("state_list_push does not raise errors - DFFilterStates", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- InteractiveFilterState$new("test", varname = "test")
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  )
})

testthat::test_that("state_list_push does not raise errors - MatrixFilterStates", {
  filter_states <- MatrixFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- InteractiveFilterState$new("test", varname = "test")
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  )
})

testthat::test_that("state_list_push does not raise errors - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- InteractiveFilterState$new("test", varname = "test")
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  )
})

testthat::test_that("state_list_push does not raise errors - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- InteractiveFilterState$new("test", varname = "test")
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  )
})



testthat::test_that(
  "Passing a FilterState to state_list_push is the same as passing it in a list - DFFilterStates",
  code = {
    filter_states_no_list <- DFFilterStates$new(
      dataname = "test",
      datalabel = "test",
      varlabels = NULL,
      keys = NULL
    )
    filter_states_list <- DFFilterStates$new(
      dataname = "test",
      datalabel = "test",
      varlabels = NULL,
      keys = NULL
    )

    filter_state <- FilterState$new("test", varname = "test")
    shiny::isolate(filter_states_no_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    shiny::isolate(filter_states_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_identical(
      shiny::isolate(filter_states_no_list$state_list_get(state_list_index = 1)),
      shiny::isolate(filter_states_list$state_list_get(state_list_index = 1))
    )
  }
)

testthat::test_that(
  "Passing a FilterState to state_list_push is the same as passing it in a list - MatrixFilterStates",
  code = {
    filter_states_no_list <- MatrixFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )
    filter_states_list <- MatrixFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )

    filter_state <- FilterState$new("test", varname = "test")
    shiny::isolate(filter_states_no_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    shiny::isolate(filter_states_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_identical(
      shiny::isolate(filter_states_no_list$state_list_get(state_list_index = 1)),
      shiny::isolate(filter_states_list$state_list_get(state_list_index = 1))
    )
  }
)

testthat::test_that(
  "Passing a FilterState to state_list_push is the same as passing it in a list - SEFilterStates",
  code = {
    filter_states_no_list <- SEFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )
    filter_states_list <- SEFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )

    filter_state <- FilterState$new("test", varname = "test")
    shiny::isolate(filter_states_no_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    shiny::isolate(filter_states_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_identical(
      shiny::isolate(filter_states_no_list$state_list_get(state_list_index = 1)),
      shiny::isolate(filter_states_list$state_list_get(state_list_index = 1))
    )
  }
)

testthat::test_that(
  "Passing a FilterState to state_list_push is the same as passing it in a list - MAEFilterStates",
  code = {
    filter_states_no_list <- MAEFilterStates$new(
      dataname = "test",
      datalabel = "test",
      varlabels = NULL,
      keys = NULL
    )
    filter_states_list <- MAEFilterStates$new(
      dataname = "test",
      datalabel = "test",
      varlabels = NULL,
      keys = NULL
    )

    filter_state <- FilterState$new("test", varname = "test")
    shiny::isolate(filter_states_no_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    shiny::isolate(filter_states_list$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_identical(
      shiny::isolate(filter_states_no_list$state_list_get(state_list_index = 1)),
      shiny::isolate(filter_states_list$state_list_get(state_list_index = 1))
    )
  }
)


# state_list_get ----
testthat::test_that("state_list_get returns the list of FilterState objects pushed - DFFilterStates", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  checkmate::expect_list(shiny::isolate(filter_states$state_list_get(state_list_index = 1)), types = "FilterState")
})

testthat::test_that("state_list_get returns the list of FilterState objects pushed - MatrixFilterStates", {
  filter_states <- MatrixFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  checkmate::expect_list(shiny::isolate(filter_states$state_list_get(state_list_index = 1)), types = "FilterState")
})

testthat::test_that("state_list_get returns the list of FilterState objects pushed - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  checkmate::expect_list(shiny::isolate(filter_states$state_list_get(state_list_index = 1)), types = "FilterState")
})

testthat::test_that("state_list_get returns the list of FilterState objects pushed - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  checkmate::expect_list(shiny::isolate(filter_states$state_list_get(state_list_index = 1)), types = "FilterState")
})



testthat::test_that(
  "Elements of the list returned by state_list_get have names corresponding to varname - DFFilterStates",
  code = {
    filter_states <- DFFilterStates$new(
      dataname = "test",
      datalabel = "test",
      varlabels = NULL,
      keys = NULL
    )
    filter_state <- InteractiveFilterState$new("test", varname = "test")
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_equal(
      names(shiny::isolate(filter_states$state_list_get(1))),
      as.character(filter_state$get_varname())
    )
  }
)

testthat::test_that(
  "Elements of the list returned by state_list_get have names corresponding to varname - MatrixFilterStates",
  code = {
    filter_states <- MatrixFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )
    filter_state <- InteractiveFilterState$new("test", varname = "test")
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_equal(
      names(shiny::isolate(filter_states$state_list_get(1))),
      as.character(filter_state$get_varname())
    )
  }
)

testthat::test_that(
  "Elements of the list returned by state_list_get have names corresponding to varname - SEFilterStates",
  code = {
    filter_states <- SEFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )
    filter_state <- InteractiveFilterState$new("test", varname = "test")
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_equal(
      names(shiny::isolate(filter_states$state_list_get(1))),
      as.character(filter_state$get_varname())
    )
  }
)

testthat::test_that(
  "Elements of the list returned by state_list_get have names corresponding to varname - MAEFilterStates",
  code = {
    filter_states <- MAEFilterStates$new(
      dataname = "test",
      datalabel = "test",
      varlabels = NULL,
      keys = NULL
    )
    filter_state <- InteractiveFilterState$new("test", varname = "test")
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    testthat::expect_equal(
      names(shiny::isolate(filter_states$state_list_get(1))),
      as.character(filter_state$get_varname())
    )
  }
)


# state_list_remove ----
testthat::test_that("state_list_remove removes filter state without error - DFFilterStates", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  )
})

testthat::test_that("state_list_remove removes filter state without error - MatrixFilterStates", {
  filter_states <- MatrixFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  )
})

testthat::test_that("state_list_remove removes filter state without error - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  )
})

testthat::test_that("state_list_remove removes filter state without error - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  testthat::expect_no_error(
    shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  )
})



testthat::test_that("state_list is empty after pushing and removing an element from it - DFFilterStates", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})

testthat::test_that(
  "state_list is empty after pushing and removing an element from it - MatrixFilterStates",
  {
    filter_states <- MatrixFilterStates$new(
      dataname = "test",
      datalabel = "test"
    )
    filter_state <- FilterState$new("test", varname = "test")
    shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
    shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
    testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
  }
)

testthat::test_that("state_list is empty after pushing and removing an element from it - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})

testthat::test_that("state_list is empty after pushing and removing an element from it - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test"))
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})


# state_list_empty ----
testthat::test_that("state_list is empty after state_list_empty - DFFilterStates", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_empty())
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})

testthat::test_that("state_list is empty after state_list_empty - MatrixFilterStates", {
  filter_states <- MatrixFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_empty())
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})

testthat::test_that("state_list is empty after state_list_empty - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_empty())
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})

testthat::test_that("state_list is empty after state_list_empty - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_empty())
  testthat::expect_length(shiny::isolate(filter_states$state_list_get(1)), 0)
})


# get_fun ----
testthat::test_that("get_fun returns subset after initialization", {
  filter_states <- FilterStates$new(dataname = "test", "test")
  testthat::expect_equal(filter_states$get_fun(), "subset")
})


# get_call ----
testthat::test_that("get_call returns NULL after initialization if no filter applied", {
  filter_states <- FilterStates$new(
    dataname = "test",
    datalabel = "label"
  )
  testthat::expect_null(filter_states$get_call())
})


testthat::test_that("get_call returns a call filtering a data.frame based on a RangeFilterState", {
  range_dataset <- data.frame(numbers = 0:4)
  compare <- range_dataset
  filter_states <- DFFilterStates$new(
    dataname = "range_dataset",
    datalabel = "label",
    varlabels = "numbers",
    keys = "numbers"
  )
  range_filter <- RangeFilterState$new(x = range_dataset$numbers, varname = "numbers")
  shiny::isolate(range_filter$set_selected(c(1, 3)))
  shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = range_filter, state_id = "test"))
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_identical(
    range_dataset,
    dplyr::filter(compare, numbers >= 1 & numbers <= 3)
  )
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call()),
    quote(range_dataset <- dplyr::filter(range_dataset, numbers >= 1 & numbers <= 3))
  )
})

testthat::test_that("get_call returns a call filtering a data.frame based on a ChoicesFilterState", {
  choices_dataset <- data.frame(choices = c("a", "b", "c"))
  filter_states <- DFFilterStates$new(
    dataname = "choices_dataset",
    datalabel = "label",
    varlabels = "choices",
    keys = "choices"
  )
  choices_filter <- ChoicesFilterState$new(x = choices_dataset$choices, varname = "choices")
  shiny::isolate(choices_filter$set_selected(c("a", "c")))
  shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = choices_filter, state_id = "test"))
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_identical(
    choices_dataset,
    dplyr::filter(choices_dataset, choices %in% c("a", "c"))
  )
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call()),
    quote(choices_dataset <- dplyr::filter(choices_dataset, choices %in% c("a", "c")))
  )
})

testthat::test_that("get_call returns a call filtering a data.frame based on a LogicalFilterState", {
  logical_dataset <- data.frame(logical = c(TRUE, FALSE, FALSE))
  filter_states <- DFFilterStates$new(
    dataname = "logical_dataset",
    datalabel = "label",
    varlabel = "logical",
    keys = "logical"
  )
  logical_filter <- LogicalFilterState$new(x = logical_dataset$logical, varname = "logical")
  shiny::isolate(logical_filter$set_selected(FALSE))
  shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = logical_filter, state_id = "test"))
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_identical(
    logical_dataset,
    dplyr::filter(logical_dataset, !logical)
  )
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call()),
    quote(logical_dataset <- dplyr::filter(logical_dataset, !logical))
  )
})

testthat::test_that("get_call returns a call filtering a data.frame based on a DateFilterState", {
  date_dataset <- data.frame(date = seq(as.Date("2021/08/25"), by = "day", length.out = 3))
  filter_states <- DFFilterStates$new(
    dataname = "date_dataset",
    datalabel = "label",
    varlabels = "date",
    keys = "date"
  )
  date_filter <- DateFilterState$new(x = date_dataset$date, varname = "date")
  shiny::isolate(date_filter$set_selected(c("2021/08/25", "2021/08/26")))
  shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = date_filter, state_id = "test"))
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_identical(
    date_dataset,
    dplyr::filter(date_dataset, date >= as.Date("2021-08-25") & date <= as.Date("2021-08-26"))
  )
  testthat::expect_identical(
    shiny::isolate(filter_states$get_call()),
    quote(
      date_dataset <- dplyr::filter(
        date_dataset, date >= as.Date("2021-08-25") & date <= as.Date("2021-08-26")
      )
    )
  )
})

testthat::test_that("get_call returns a call filtering a data.frame based on a DatetimeFilterState", {
  datetime_dataset <- data.frame(
    datetime = seq(ISOdate(2021, 8, 25, tz = Sys.timezone()), by = "day", length.out = 3)
  )
  compare <- datetime_dataset
  filter_states <- DFFilterStates$new(
    dataname = "datetime_dataset",
    datalabel = "label",
    varlabels = "datetime",
    keys = "datetime"
  )
  datetime_filter <- DatetimeFilterState$new(x = datetime_dataset$datetime, varname = "datetime")
  shiny::isolate(datetime_filter$set_selected(rep(ISOdate(2021, 8, 27, tz = Sys.timezone()), 2)))
  shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = datetime_filter, state_id = "test"))
  eval(shiny::isolate(filter_states$get_call()))
  testthat::expect_equal(
    datetime_dataset,
    dplyr::filter(
      compare,
      datetime >= as.POSIXct("2021-08-27 12:00:00") &
        datetime < as.POSIXct("2021-08-27 12:00:01")
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_states$get_call()),
    bquote(datetime_dataset <- dplyr::filter(
      datetime_dataset,
      datetime >= as.POSIXct("2021-08-27 12:00:00", tz = .(Sys.timezone())) &
        datetime < as.POSIXct("2021-08-27 12:00:01", tz = .(Sys.timezone()))
    ))
  )
})

testthat::test_that(
  "get_call returns a call filtering a data.frame base on a combination of FilterState objects",
  {
    # setting up the test dataset
    test_dataset <- data.frame(
      numbers = 1:5,
      choices = letters[1:5],
      logical = c(FALSE, FALSE, FALSE, TRUE, FALSE),
      date = seq(as.Date("2021/08/25"), by = "day", length.out = 5),
      datetime = seq(ISOdate(2021, 8, 25, tz = Sys.timezone()), by = "day", length.out = 5)
    )
    compare <- test_dataset
    # setting up filters
    filter_states <- DFFilterStates$new(
      dataname = "test_dataset",
      datalabel = "label",
      varlabels = NULL,
      keys = NULL
    )

    range_filter <- RangeFilterState$new(x = test_dataset$numbers, varname = "numbers")
    shiny::isolate(range_filter$set_selected(c(1, 3)))
    choices_filter <- ChoicesFilterState$new(x = test_dataset$choices, varname = "choices")
    shiny::isolate(choices_filter$set_selected(c("a", "c")))
    logical_filter <- LogicalFilterState$new(x = test_dataset$logical, varname = "logical")
    shiny::isolate(logical_filter$set_selected(FALSE))
    date_filter <- DateFilterState$new(x = test_dataset$date, varname = "date")
    shiny::isolate(date_filter$set_selected(c("2021/08/25", "2021/08/26")))
    datetime_filter <- DatetimeFilterState$new(x = test_dataset$datetime, varname = "datetime")
    shiny::isolate(datetime_filter$set_selected(rep(ISOdate(2021, 8, 25, tz = Sys.timezone()), 2)))

    shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = range_filter, state_id = "test"))
    shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = choices_filter, state_id = "test"))
    shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = logical_filter, state_id = "test"))
    shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = date_filter, state_id = "test"))
    shiny::isolate(filter_states$state_list_push(state_list_index = 1, x = datetime_filter, state_id = "test"))

    eval(shiny::isolate(filter_states$get_call()))
    testthat::expect_equal(
      test_dataset,
      dplyr::filter(
        compare,
        numbers >= 1 & numbers <= 3 &
          choices %in% c("a", "c") &
          !logical &
          date >= as.Date("2021-08-25") & date <= as.Date("2021-08-26") &
          datetime >= as.POSIXct("2021-08-25 12:00:00") &
          datetime < as.POSIXct("2021-08-25 12:00:01")
      )
    )
    testthat::expect_equal(
      shiny::isolate(filter_states$get_call()),
      bquote(test_dataset <- dplyr::filter(
        test_dataset,
        numbers >= 1 & numbers <= 3 &
          choices %in% c("a", "c") &
          !logical &
          (date >= as.Date("2021-08-25") & date <= as.Date("2021-08-26")) &
          (datetime >= as.POSIXct("2021-08-25 12:00:00", tz = .(Sys.timezone())) &
            datetime < as.POSIXct("2021-08-25 12:00:01", tz = .(Sys.timezone())))
      ))
    )
  }
)


# data_choices_labeled ----
testthat::test_that("data_choices_labeled returns an empty character array if choices are an empty array", {
  testthat::expect_equal(data_choices_labeled(7, choices = c()), character(0))
})

testthat::test_that("data_choices_labeled returns a choices_labeled object if choices are not empty", {
  testthat::expect_true(is(data_choices_labeled(data.frame(a = 1), choices = c("a")), "choices_labeled"))
})

testthat::test_that("data_choices_labeled returns names of the elements matching the choices", {
  testthat::expect_equal(data_choices_labeled(data.frame(a = 1, b = 2), choices = c("a"))[1], c("a: a" = "a"))
})

testthat::test_that("data_choices_labeled returns labels of the elements matching the choices
  if the varlabels are provided for the elements", {
  result <- unname(data_choices_labeled(list(a = 1, b = 2), choices = c("a"), varlabels = c(a = "labelA"))[1])
  testthat::expect_equal(result, "a")
})


# get_filter_count ----
testthat::test_that("get_filter_count returns the number of active filter states - DFFilterStates", {
  filter_states <- DFFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 0)
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})

testthat::test_that("get_filter_count returns the number of active filter states - MartixFilterStates", {
  filter_states <- MatrixFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 0)
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})

testthat::test_that("get_filter_count returns the number of active filter states - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 0)
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})

testthat::test_that("get_filter_count returns the number of active filter states - MAEFilterStates", {
  filter_states <- MAEFilterStates$new(
    dataname = "test",
    datalabel = "test",
    varlabels = NULL,
    keys = NULL
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 0)
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test"))
  shiny::isolate(filter_states$state_list_push(x = filter_state, state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  shiny::isolate(filter_states$state_list_remove(state_list_index = 1, state_id = "test2"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})

testthat::test_that("get_filter_count properly tallies multiple state lists - SEFilterStates", {
  filter_states <- SEFilterStates$new(
    dataname = "test",
    datalabel = "test"
  )
  filter_state <- FilterState$new("test", varname = "test")
  shiny::isolate(
    filter_states$state_list_push(x = filter_state, state_list_index = "subset", state_id = "test")
  )
  shiny::isolate(
    filter_states$state_list_push(x = filter_state, state_list_index = "select", state_id = "test2")
  )
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 2)
  shiny::isolate(filter_states$state_list_remove(state_list_index = "subset", state_id = "test"))
  testthat::expect_equal(shiny::isolate(filter_states$get_filter_count()), 1)
})
