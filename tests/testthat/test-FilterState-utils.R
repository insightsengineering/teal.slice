# check_in_range ----
testthat::test_that("check_in_range raises errors as desired", {
  testthat::expect_no_error(
    check_in_range(subinterval = c(4, 6), range = c(1, 10))
  )
  testthat::expect_error(
    check_in_range(subinterval = 4, range = c(1, 10)),
    "range length should be 2"
  )
  testthat::expect_error(
    check_in_range(subinterval = c(4, 3), range = c(1, 10)),
    "the upper bound of the range lower than the lower bound"
  )
  testthat::expect_error(
    check_in_range(subinterval = c(1, 10), range = c(3, 4)),
    "not valid for full range"
  )
})

testthat::test_that("check_in_subset prepends error message", {
  testthat::expect_error(
    check_in_range(subinterval = 4, range = c(1, 10), "premessage"),
    "^premessage.*range length should be 2"
  )
})



# check_in_subset ----
testthat::test_that("check_in_subset raises errors as desired", {
  testthat::expect_no_error(
    check_in_subset(subset = letters[4:6], letters)
  )
  testthat::expect_error(
    check_in_subset(subset = letters[4:6], letters[8:10]),
    "not in valid choices"
  )
})

testthat::test_that("check_in_subset prepends error message", {
  testthat::expect_error(
    check_in_subset(subset = letters[4:6], letters[8:10], "premessage"),
    "^premessage.*not in valid choices"
  )
})


# contain_interval ----
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
