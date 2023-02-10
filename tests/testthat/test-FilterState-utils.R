
testthat::test_that("contain_interval accepts proper arguments", {
  testthat::expect_no_error(contain_interval(c(3, 5), 1:10))
  testthat::expect_error(contain_interval(c("1", "2"), 1:10), "Assertion on 'x' failed")
  testthat::expect_error(contain_interval(1, 1:10), "Assertion on 'x' failed")
  testthat::expect_error(contain_interval(c(1, NA), 1:10), "Assertion on 'x' failed")
  testthat::expect_error(contain_interval(c(5, 3), 1:10), "Assertion on 'x' failed")

  testthat::expect_error(contain_interval(c(3, 5), letters[1:10]), "Assertion on 'range' failed")
  testthat::expect_error(contain_interval(c(3, 5), 1), "Assertion on 'range' failed")
  testthat::expect_error(contain_interval(c(3, 5), c(1:10, NA)), "Assertion on 'range' failed")
  testthat::expect_error(contain_interval(c(3, 5), 10:1), "Assertion on 'range' failed")
})

testthat::test_that("contain_interval returns containing range", {
  testthat::expect_equal(contain_interval(c(3.2, 5.7), 1:10), c(3, 6))
})

testthat::test_that("contain_interval returns 'x' if interval matches ticks", {
  testthat::expect_equal(contain_interval(c(3, 5), 1:10), c(3, 5))
})

testthat::test_that("contain_interval returns 'range' if 'x' is x is out of bounds", {
  testthat::expect_equal(contain_interval(c(0, 11), 1:10), c(1, 10))
})

