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

testthat::test_that("disabled state doesn't return call while enabled returns", {
  state <- FilterStateExpr$new(
    id = "id",
    title = "testtitle",
    dataname = "x",
    expr = quote(x == "x"),
    disabled = FALSE
  )

  shiny::testServer(
    state$server,
    expr = {
      session$setInputs(enable = FALSE)
    }
  )
  testthat::expect_null(shiny::isolate(state$get_call()))
})
