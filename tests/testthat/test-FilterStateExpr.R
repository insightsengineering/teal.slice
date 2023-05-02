testthat::test_that("get_call returns call identical to one passed in the constructor", {
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = quote(x == "x"),
    disabled = FALSE
  )
  testthat::expect_identical(shiny::isolate(state$get_call()), quote(x == "x"))
})

testthat::test_that("", {
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = quote(x == "x"),
    disabled = TRUE
  )

  shiny::testServer(
    state$server,
    expr = {
      testthat::expect_null(state$get_call())
      session$setInputs(enable = TRUE)
      testthat::expect_identical(state$get_call(), quote(x == "x"))
    }
  )
})