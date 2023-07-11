testthat::test_that("initialize require teal_slice_expr", {
  testthat::expect_no_error(FilterStateExpr$new(
    teal_slice(dataname = "x", id = "x", title = "x", expr = "x == 'x'")
  ))
  testthat::expect_error(FilterStateExpr$new(teal_slice(dataname = "x", varname = "x")), "slice")
})

testthat::test_that("get_call returns call identical to one passed in the constructor", {
  state <- FilterStateExpr$new(teal_slice(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = "x == 'x'"
  ))
  testthat::expect_identical(shiny::isolate(state$get_call()), quote(x == "x"))
})

# set/get_state -----
testthat::test_that("get_state returns list of state values", {
  state <- FilterStateExpr$new(teal_slice(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = "x == 'x'"
  ))
  expect_identical_slice(
    shiny::isolate(state$get_state()),
    teal_slice(id = "id", title = "testtitle", dataname = "x", expr = "x == 'x'")
  )
})

testthat::test_that("set_state ignores every attribute", {
  state <- FilterStateExpr$new(teal_slice(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
  state$set_state(
    teal_slice(id = "id2", title = "title2", dataname = "x2", expr = "x2 == 'x2'")
  )
  expect_identical_slice(
    state$get_state(),
    teal_slice(id = "id", title = "testtitle", dataname = "x", expr = "x == 'x'")
  )
})

# format ---
testthat::test_that("format returns a properly formatted string representation", {
  state <- FilterStateExpr$new(teal_slice(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
  testthat::expect_equal(
    shiny::isolate(state$format()),
    paste0(
      "FilterStateExpr:\n",
      format(shiny::isolate(state$get_state()))
    )
  )
})

testthat::test_that("format accepts logical show_all", {
  filter_state <- FilterStateExpr$new(teal_slice(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
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

testthat::test_that("format accepts logical trim_lines", {
  filter_state <- FilterStateExpr$new(teal_slice(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
  testthat::expect_no_error(shiny::isolate(filter_state$format(trim_lines = TRUE)))
  testthat::expect_no_error(shiny::isolate(filter_state$format(trim_lines = FALSE)))
  testthat::expect_error(
    shiny::isolate(filter_state$format(trim_lines = 1)),
    "Assertion on 'trim_lines' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(trim_lines = 0)),
    "Assertion on 'trim_lines' failed: Must be of type 'logical flag', not 'double'"
  )
  testthat::expect_error(
    shiny::isolate(filter_state$format(trim_lines = "TRUE")),
    "Assertion on 'trim_lines' failed"
  )
})

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  state <- FilterStateExpr$new(teal_slice(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
  testthat::expect_equal(
    utils::capture.output(cat(state$print())),
    c("FilterStateExpr:", utils::capture.output(print(shiny::isolate(state$get_state()))))
  )
})
