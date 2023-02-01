method <- function(a, b, ..., stop = TRUE, allowed_args = character()) {
  check_ellipsis(..., stop = stop, allowed_args = allowed_args)
}

testthat::test_that("check_ellipsis with no unused", {
  testthat::expect_silent(method(a = 1, b = 2))
})

testthat::test_that("check_ellipsis with extra unamed arguments", {
  testthat::expect_error(method(a = 1, b = 2, 5, 6), "2 total unused argument\\(s\\)\\.")
  testthat::expect_warning(method(a = 1, b = 2, 5, 6, stop = FALSE), "2 total unused argument\\(s\\)\\.")
})

testthat::test_that("check_ellipsis with extra named arguments", {
  testthat::expect_error(method(a = 1, b = 2, c = 5, d = 6), "2 total unused argument\\(s\\)\\. 2 with name\\(s\\): c, d\\.")
  testthat::expect_warning(
    method(a = 1, b = 2, c = 5, d = 6, stop = FALSE),
    "2 total unused argument\\(s\\)\\. 2 with name\\(s\\): c, d\\."
  )
})

testthat::test_that("check_ellipsis with extra named and unamed arguments", {
  testthat::expect_error(method(a = 1, b = 2, c = 5, 6), "2 total unused argument\\(s\\). 1 with name\\(s\\): c\\.")
  testthat::expect_warning(
    method(a = 1, b = 2, c = 5, 6, stop = FALSE),
    "2 total unused argument\\(s\\). 1 with name\\(s\\): c\\."
  )
})

testthat::test_that("check_ellipsis with extra named and unamed arguments in wrong order", {
  testthat::expect_error(method(c = 5, 6, a = 1, b = 2), "2 total unused argument\\(s\\)\\. 1 with name\\(s\\): c\\.")
  testthat::expect_warning(
    method(a = 1, c = 5, b = 2, 6, stop = FALSE),
    "2 total unused argument\\(s\\)\\. 1 with name\\(s\\): c\\."
  )
})

testthat::test_that("check_ellipsis with allowed args", {
  testthat::expect_silent(method(a = 1, b = 2, z = 3, allowed_args = c("z")))
  testthat::expect_error(
    method(a = 1, b = 2, y = 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): y\\."
  )
  testthat::expect_error(
    method(a = 1, b = 2, y = 3, z = 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): y\\."
  )
  testthat::expect_error(
    method(a = 1, b = 2, 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\."
  )
  testthat::expect_silent(method(a = 1, b = 2, 3, allowed_args = c("")))
  testthat::expect_error(
    method(a = 1, b = 2, 3, z = 9, allowed_args = c("")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): z\\."
  )
  testthat::expect_silent(method(a = 1, b = 2, 3, z = 5, allowed_args = c("", "z", "y")))
  testthat::expect_silent(method(a = 1, b = 2, 3, z = 5, y = 4, allowed_args = c("", "z", "y")))
  testthat::expect_silent(method(a = 1, b = 2, allowed_args = c("", "z", "y")))
})

