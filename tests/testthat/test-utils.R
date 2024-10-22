# make_c_call ----
testthat::test_that("make_c_call", {
  testthat::expect_identical(make_c_call(1:3), quote(c(1L, 2L, 3L)))
  testthat::expect_identical(make_c_call(1), 1)
})
