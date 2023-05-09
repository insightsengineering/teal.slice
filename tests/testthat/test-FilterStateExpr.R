testthat::test_that("initialize require id, title, dataname and expr to be specified", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(x == "x")))
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x"), "expr")
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x"), "title")
  testthat::expect_error(FilterStateExpr$new(dataname = "x"), "id")
  testthat::expect_error(FilterStateExpr$new(), "dataname")
})


testthat::test_that("id has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(x == "x")))
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = 1, title = "x", expr = quote(x == "x")), "string")
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = NULL, title = "x", expr = quote(x == "x")), "string")
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = character(0), title = "x", expr = quote(x == "x")), "length"
  )
})

testthat::test_that("title has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(x == "x")))
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x", title = 1, expr = quote(x == "x")), "string")
  testthat::expect_error(FilterStateExpr$new(dataname = "x", id = "x", title = NULL, expr = quote(x == "x")), "string")
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = character(0), expr = quote(x == "x")), "length"
  )
})

testthat::test_that("dataname has to be a string", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(x == "x")))
  testthat::expect_error(FilterStateExpr$new(dataname = 1, id = "x", title = "x", expr = quote(x == "x")), "string")
  testthat::expect_error(FilterStateExpr$new(dataname = NULL, id = "x", title = "x", expr = quote(x == "x")), "string")
  testthat::expect_error(
    FilterStateExpr$new(dataname = character(0), id = "x", title = "x", expr = quote(x == "x")), "length"
  )
})

testthat::test_that("expr has to be a logical expression", {
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(x == FALSE)))
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(!is.na(x))))
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(is.finite(x))))
  testthat::expect_no_error(FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(grepl("x", x))))
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(x <- 2)),
    "has to be a logical expression"
  )
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = TRUE),
    "has to be a logical expression"
  )
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = 1),
    "has to be a logical expression"
  )
  testthat::expect_error(
    FilterStateExpr$new(dataname = "x", id = "x", title = "x", expr = quote(1)),
    "has to be a logical expression"
  )
})

testthat::test_that("get_call returns call identical to one passed in the constructor", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = quote(x == "x"),
    disabled = FALSE
  )
  testthat::expect_identical(shiny::isolate(state$get_call()), quote(x == "x"))
})

testthat::test_that("is_any_filtered returns TRUE by default (when disabled = FALSE)", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = quote(x == "x")
  )
  testthat::expect_true(shiny::isolate(state$is_any_filtered()))
})

testthat::test_that("disabled sets is_any_filtered to FALSE", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = quote(x == "x"),
    disabled = TRUE
  )
  testthat::expect_false(shiny::isolate(state$is_any_filtered()))
})


# set/get_state -----
testthat::test_that("get_state returns list of state values", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = quote(x == "x")
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()),
    filter_expr(
      id = "id", title = "testtitle", dataname = "x", expr = quote(x == "x"), disabled = FALSE
    )
  )
})

testthat::test_that("set_state accepts only filter_state_expr", {
  state <- FilterStateExpr$new(
    dataname = "x",
    id = "id",
    title = "testtitle",
    expr = quote(x == "x"),
    disabled = FALSE
  )

  testthat::expect_error(
    state$set_state(
      filter_var(
        dataname = "x2", varname = "x", id = "id2", title = "title2", expr = quote(x2 == "x2"), disabled = TRUE
      )
    )
  )
  testthat::expect_error(state$set_state(NULL))
})

testthat::test_that("set_state ignores every attribute except disabled", {
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = quote(x == "x"),
    disabled = FALSE
  )
  state$set_state(
    filter_expr(id = "id2", title = "title2", dataname = "x2", expr = quote(x2 == "x2"), disabled = TRUE)
  )
  testthat::expect_identical(
    shiny::isolate(state$get_state()),
    filter_expr(
      id = "id", title = "testtitle", dataname = "x", expr = quote(x == "x"), disabled = TRUE
    )
  )
})
