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
