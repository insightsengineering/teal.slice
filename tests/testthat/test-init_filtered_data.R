testthat::test_that("init_filtered_data accepts a list of `data.frame` objects", {
  testthat::expect_no_error(init_filtered_data(list(iris = iris)))
})


testthat::test_that("init_filtered_data asserts x has unique names", {
  testthat::expect_error(
    init_filtered_data(list("iris" = iris, "iris" = iris)),
    regexp = "Assertion on 'x' failed: Must have unique names, but element 2 is duplicated."
  )
})

testthat::test_that("init_filtered_data asserts join_keys is `join_keys`", {
  testthat::expect_error(
    init_filtered_data(list("iris" = iris), join_keys = "test"),
    regexp = "Assertion on 'join_keys' failed: Must inherit from class 'join_keys', but has class 'character'."
  )
})

testthat::test_that("init_filtered_data ignores datasets if they are of different class than data.frame and MAE", {
  testthat::skip_if_not_installed("MultiAssayExperiment")
  utils::data(miniACC, package = "MultiAssayExperiment")
  fd <- init_filtered_data(
    list(
      a = character(),
      b = structure(data.frame(), class = "not data.frame"),
      c = data.frame(),
      d = miniACC
    )
  )
  testthat::expect_identical(fd$datanames(), c("c", "d"))
})
