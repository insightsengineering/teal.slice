testthat::test_that("initialize require teal_slice_expr", {
  testthat::expect_no_error(FilterStateExpr$new(
    filter_expr(dataname = "x", id = "x", title = "x", expr = "x == 'x'")
  ))
  testthat::expect_error(FilterStateExpr$new(filter_var(dataname = "x", varname = "x")), "slice")
})

testthat::test_that("get_call returns call identical to one passed in the constructor", {
  state <- FilterStateExpr$new(filter_expr(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = "x == 'x'"
  ))
  testthat::expect_identical(shiny::isolate(state$get_call()), quote(x == "x"))
})

# set/get_state -----
testthat::test_that("get_state returns list of state values", {
  state <- FilterStateExpr$new(filter_expr(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = "x == 'x'"
  ))
  expect_identical_slice(
    shiny::isolate(state$get_state()),
    filter_expr(id = "id", title = "testtitle", dataname = "x", expr = "x == 'x'")
  )
})

testthat::test_that("set_state ignores every attribute", {
  state <- FilterStateExpr$new(filter_expr(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
  state$set_state(
    filter_expr(id = "id2", title = "title2", dataname = "x2", expr = "x2 == 'x2'")
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()),
    filter_expr(id = "id", title = "testtitle", dataname = "x", expr = "x == 'x'")
  )
})

# format ---
testthat::test_that("format returns a properly formatted string representation", {
  state <- FilterStateExpr$new(filter_expr(
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

# print ---
testthat::test_that("print returns a properly formatted string representation", {
  state <- FilterStateExpr$new(filter_expr(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  ))
  testthat::expect_equal(
    utils::capture.output(cat(state$print())),
    c(
      "FilterStateExpr:",
      utils::capture.output(print(shiny::isolate(state$get_state()))),
      " "
    )
  )
})
