testthat::test_that("init_filtered_data accepts a list of `data.frame` objects", {
  testthat::expect_no_error(init_filtered_data(list(iris = iris)))
})


testthat::test_that("init_filtered_data.default asserts x has unique names", {
  testthat::expect_error(
    init_filtered_data(list("iris" = iris, "iris" = iris)),
    regexp = "Assertion on 'x' failed: Must have unique names, but element 2 is duplicated."
  )
})

testthat::test_that("init_filtered_data.default asserts join_keys is `join_keys`", {
  testthat::expect_error(
    init_filtered_data(list("iris" = iris), join_keys = "test"),
    regexp = "Assertion on 'join_keys' failed: Must inherit from class 'join_keys', but has class 'character'."
  )
})
