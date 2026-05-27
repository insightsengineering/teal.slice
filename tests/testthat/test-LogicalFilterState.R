logs <- as.logical(c(1, 0, 0, 0, 1, 1, 0, 1, 0, 1, NA))
logs_no_na <- as.logical(c(1, 0, 0, 0, 1, 1, 0, 1, 0, 1))

# initialize ----
testthat::test_that("constructor accepts logical values", {
  testthat::expect_no_error(
    LogicalFilterState$new(logs, slice = teal_slice(dataname = "data", varname = "variable"))
  )
  testthat::expect_error(
    LogicalFilterState$new(0:1, slice = teal_slice(dataname = "data", varname = "variable")),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("constructor raises error when selection is not logical", {
  testthat::expect_error(
    LogicalFilterState$new(logs, slice = teal_slice(dataname = "data", varname = "variable", selected = "TRUE")),
    "Must be of type 'logical'"
  )
})

testthat::test_that("constructor forces single selected when multiple is FALSE", {
  testthat::expect_no_error(
    state <- LogicalFilterState$new(
      x = logs,
      slice = teal_slice(dataname = "data", varname = "var", selected = c(TRUE, FALSE), multiple = FALSE)
    )
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()$selected),
    TRUE
  )
})

# set_state ----
testthat::test_that("set_state: selected accepts a logical vector (or coercible)", {
  filter_state <- LogicalFilterState$new(logs, slice = teal_slice(dataname = "data", varname = "variable"))
  testthat::expect_no_error(
    filter_state$set_state(teal_slice(dataname = "data", varname = "variable", selected = TRUE))
  )
  testthat::expect_no_error(
    filter_state$set_state(teal_slice(dataname = "data", varname = "variable", selected = "TRUE"))
  )
  testthat::expect_no_error(
    filter_state$set_state(teal_slice(dataname = "data", varname = "variable", selected = 1))
  )
  testthat::expect_no_error(
    filter_state$set_state(teal_slice(dataname = "data", varname = "variable", selected = c(TRUE, FALSE)))
  )
  testthat::expect_error(
    filter_state$set_state(teal_slice(dataname = "data", varname = "variable", selected = "a")),
    "Vector of set values must contain values coercible to logical"
  )
})

testthat::test_that("set_state: multiple selection is aborted when multiple = FALSE", {
  filter_state <- LogicalFilterState$new(
    x = logs,
    slice = teal_slice(dataname = "data", varname = "variable", multiple = FALSE)
  )

  fs <- teal_slice(dataname = "data", varname = "variable", selected = TRUE)
  testthat::expect_no_error(
    filter_state$set_state(fs)
  )
  testthat::expect_warning(
    filter_state$set_state(teal_slice(dataname = "data", varname = "variable", selected = c(FALSE, TRUE))),
    "Maintaining previous selection"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_state()$selected), TRUE
  )
})

# get_call ----
testthat::test_that("LogicalFilterState$get_call returns variable name when !multiple", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = teal_slice(dataname = "data", varname = "variable", multiple = FALSE)
  )
  expect_identical(shiny::isolate(filter_state$get_call()), quote(variable))
})

testthat::test_that("get_call returns call selected different than choices", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = teal_slice(dataname = "data", varname = "variable", choices = c(TRUE, FALSE), selected = FALSE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(!variable)
  )
})

testthat::test_that("get_call returns call always if choices are limited - regardless of selected", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = teal_slice(dataname = "data", varname = "variable", choices = FALSE)
  )

  # todo: what should this really return?
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(variable %in% c(TRUE, FALSE))
  )
})

testthat::test_that("get_call prefixes varname by dataname$varname if extract_type='list'", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = teal_slice(dataname = "data", varname = "variable", selected = FALSE),
    extract_type = "list"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(!dataname$variable)
  )
})

testthat::test_that("get_call prefixes varname by dataname[, 'varname'] if extract_type='matrix'", {
  filter_state <- LogicalFilterState$new(
    logs[1:10],
    slice = teal_slice(dataname = "data", varname = "variable", selected = FALSE),
    extract_type = "matrix"
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call(dataname = "dataname")),
    quote(!dataname[, "variable"])
  )
})

testthat::test_that("get_call adds is.na(variable) to returned call if keep_na is true", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", selected = FALSE, keep_na = TRUE)
  )
  testthat::expect_identical(
    shiny::isolate(filter_state$get_call()),
    quote(is.na(variable) | !variable)
  )
})

# is_any_filtered ----
testthat::test_that("is_any_filtered returns TRUE when both logical values exist in the data", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", selected = TRUE, keep_na = TRUE)
  )
  testthat::expect_true(
    shiny::isolate(filter_state$.__enclos_env__$private$is_any_filtered())
  )
})


testthat::test_that("is_any_filtered returns FALSE when one value in data, selected, NA kept", {
  filter_state <- LogicalFilterState$new(
    c(TRUE, TRUE, TRUE, NA),
    slice = teal_slice(
      dataname = "data", varname = "variable", selected = TRUE, keep_na = TRUE
    )
  )
  testthat::expect_false(
    shiny::isolate(filter_state$.__enclos_env__$private$is_any_filtered())
  )
})

testthat::test_that("is_any_filtered returns TRUE when keep_na is FALSE and NA values exist", {
  filter_state <- LogicalFilterState$new(
    c(TRUE, TRUE, TRUE, NA),
    slice = teal_slice(dataname = "data", varname = "variable", selected = TRUE, keep_na = FALSE)
  )
  testthat::expect_true(
    shiny::isolate(filter_state$.__enclos_env__$private$is_any_filtered())
  )
})


# ui_inputs ----
testthat::test_that("ui_inputs renders radioButtons when multiple is FALSE", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", multiple = FALSE)
  )
  ui_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$ui_inputs("test"))
  )
  testthat::expect_match(ui_html, "shiny-input-radiogroup")
  testthat::expect_no_match(ui_html, "shiny-input-checkboxgroup")
})

testthat::test_that("ui_inputs renders the expected component when multiple is TRUE", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(
      dataname = "data", varname = "variable",
      multiple = TRUE, selected = c(TRUE, FALSE)
    )
  )
  ui_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$ui_inputs("test"))
  )
  testthat::expect_match(ui_html, "shiny-input-checkboxgroup")
  testthat::expect_no_match(ui_html, "shiny-input-radiogroup")
})

testthat::test_that("ui_inputs renders TRUE and FALSE as choice labels", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable")
  )
  ui_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$ui_inputs("test"))
  )
  testthat::expect_match(ui_html, 'value="TRUE"', fixed = TRUE)
  testthat::expect_match(ui_html, 'value="FALSE"', fixed = TRUE)
})

testthat::test_that("ui_inputs renders keep NA checkbox when NA values exist", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable")
  )
  ui_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$ui_inputs("test"))
  )
  testthat::expect_match(ui_html, "Keep NA")
})

testthat::test_that("ui_inputs does not render keep NA checkbox when no NA values exist", {
  filter_state <- LogicalFilterState$new(
    logs_no_na,
    slice = teal_slice(dataname = "data", varname = "variable")
  )
  ui_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$ui_inputs("test"))
  )
  testthat::expect_no_match(ui_html, "Keep NA")
})

# format ----
testthat::test_that("format accepts logical show_all", {
  filter_state <- LogicalFilterState$new(logs, slice = teal_slice(dataname = "data", varname = "variable"))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = TRUE)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(show_all = FALSE)))
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = 1)),
    "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = 0)),
    "Assertion on 'show_all' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(show_all = "TRUE")),
    "Assertion on 'show_all' failed"
  )
})

testthat::test_that("format returns a properly formatted string representation", {
  filter_state <- LogicalFilterState$new(logs, slice = teal_slice(dataname = "data", varname = "variable"))
  filter_state$set_state(teal_slice(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    shiny::isolate(filter_state$format()),
    paste0(
      "LogicalFilterState:\n",
      format(shiny::isolate(filter_state$get_state()))
    )
  )
  testthat::expect_equal(
    shiny::isolate(filter_state$format(show_all = TRUE)),
    paste0(
      "LogicalFilterState:\n",
      format(shiny::isolate(filter_state$get_state()), show_all = TRUE)
    )
  )
})

# content_summary ----
testthat::test_that("content_summary shows selected value in summary-value span", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", selected = TRUE)
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "summary-value\">TRUE")
})

testthat::test_that("content_summary shows FALSE as selected value", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", selected = FALSE)
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "summary-value\">FALSE")
})

testthat::test_that("content_summary shows comma-separated values when multiple are selected", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(
      dataname = "data", varname = "variable",
      selected = c(TRUE, FALSE), multiple = TRUE
    )
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "TRUE, FALSE")
})

testthat::test_that("content_summary shows NA check icon when keep_na is TRUE and NA values exist", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", selected = TRUE, keep_na = TRUE)
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "fa-check")
  testthat::expect_match(summary_html, "text-success")
})

testthat::test_that("content_summary shows NA xmark icon when keep_na is FALSE and NA values exist", {
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(
      dataname = "data", varname = "variable", selected = TRUE, keep_na = FALSE
    )
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "fa-xmark")
  testthat::expect_match(summary_html, "text-danger")
})

testthat::test_that("content_summary shows NA check icon when keep_na is NULL", { # nolint
  filter_state <- LogicalFilterState$new(
    logs,
    slice = teal_slice(dataname = "data", varname = "variable", selected = TRUE)
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "fa-check")
  testthat::expect_match(summary_html, "text-success")
})

testthat::test_that("content_summary omits NA span when data has no NA values", {
  filter_state <- LogicalFilterState$new(
    logs_no_na,
    slice = teal_slice(dataname = "data", varname = "variable", selected = TRUE)
  )
  summary_html <- as.character(
    shiny::isolate(filter_state$.__enclos_env__$private$content_summary())
  )
  testthat::expect_match(summary_html, "filter-card-summary-controls")
  testthat::expect_no_match(summary_html, "fa-check")
  testthat::expect_no_match(summary_html, "fa-xmark")
})

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  filter_state <- LogicalFilterState$new(logs, slice = teal_slice(dataname = "data", varname = "variable"))
  filter_state$set_state(teal_slice(dataname = "data", varname = "variable", keep_na = FALSE))
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print())),
    c("LogicalFilterState:", utils::capture.output(print(shiny::isolate(filter_state$get_state()))))
  )
  testthat::expect_equal(
    utils::capture.output(cat(filter_state$print(show_all = TRUE))),
    c("LogicalFilterState:", utils::capture.output(print(shiny::isolate(filter_state$get_state()), show_all = TRUE)))
  )
})
