
testthat::test_that("calls_combine_by - different operators", {
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b)), operator = "&"),
    quote(a & b)
  )
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b)), operator = "||"),
    quote(a || b)
  )
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b()), quote(c())), operator = "%>%"),
    quote(a %>% b() %>% c())
  )
  testthat::expect_error(
    calls_combine_by(calls = list(quote(a), quote(b), quote(c)), operator = as.symbol("&"))
  )
  testthat::expect_error(
    calls_combine_by(calls = list(quote(a), quote(b), quote(c)), operator = c("&", "|"))
  )
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b), quote(c)), operator = "whatever"),
    quote(whatever(whatever(a, b), c))
  )
})

testthat::test_that("calls_combine_by - different forms of calls", {
  testthat::expect_null(calls_combine_by(calls = list(), operator = "&"))
  testthat::expect_identical(
    calls_combine_by(calls = list(as.name("a"), quote(b$b)), operator = "||"),
    quote(a || b$b)
  )
  testthat::expect_error(
    calls_combine_by(calls = list("a", quote(a)), operator = "%>%"),
    "Assertion.+ failed"
  )
})
