testthat::test_that("initialize require id, title, dataname and expr to be specified", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x"), "expr")
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x"), "title")
  testthat::expect_error(FilterStateExpr$new(dataname = "x"), "id")
  testthat::expect_error(FilterStateExpr$new(), "dataname")
})


testthat::test_that("id has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = 1, title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = NULL, title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = character(0), title = "x", expr = "x == 'x'"), "length"
  )
})

testthat::test_that("title has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x", title = 1, expr = "x == 'x'"), "string")
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x", title = NULL, expr = "x == 'x'"), "string")
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = character(0), expr = "x == 'x'"), "length"
  )
})

testthat::test_that("dataname has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(FilterStateExpr$new(dataname = 1, id = "x", title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(FilterStateExpr$new(dataname = NULL, id = "x", title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(
    FilterStateExpr$new(dataname = character(0), id = "x", title = "x", expr = "x == 'x'"), "length"
  )
})

testthat::test_that("expr has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = "x == FALSE"))
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = "x <- 1")) # Ouch!
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = TRUE),
    "Assertion on 'expr' failed"
  )
})

testthat::test_that("get_call returns call identical to one passed in the constructor", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = "x == 'x'"
  )
  testthat::expect_identical(shiny::isolate(state$get_call()), quote(x == "x"))
})

# set/get_state -----
testthat::test_that("get_state returns list of state values", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = "x == 'x'"
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()),
    filter_expr(
      id = "id", title = "testtitle", dataname = "x", expr = "x == 'x'"
    )
  )
})

testthat::test_that("set_state ignores every attribute", {
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  )
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
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  )
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
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = "x == 'x'"
  )
  testthat::expect_equal(
    utils::capture.output(cat(state$print())),
    c(
      "FilterStateExpr:",
      utils::capture.output(print(shiny::isolate(state$get_state()))),
      " "
    )
  )
})
