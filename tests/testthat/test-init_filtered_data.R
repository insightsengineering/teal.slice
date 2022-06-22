testthat::test_that("init_filtered_data accepts a TealData object", {
  teal_data <- teal.data::teal_data(teal.data::dataset(dataname = "iris", x = iris))
  testthat::expect_error(init_filtered_data(teal_data), regexp = NA)
})

testthat::test_that("init_filtered_data accepts a CDISCTealData with mixed CDISC and Dataset", {
  adsl <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
  teal_data <- teal.data::cdisc_data(
    teal.data::dataset(dataname = "iris", x = iris),
    teal.data::cdisc_dataset("ADSL", adsl)
  )
  testthat::expect_error(init_filtered_data(teal_data), regexp = NA)
})

testthat::test_that("init_filtered_data accepts a list of `data.frame` objects", {
  testthat::expect_error(init_filtered_data(list("iris" = list(dataset = iris))), regexp = NA)
})


testthat::test_that("init_filtered_data.default asserts x has unique names", {
  testthat::expect_error(
    init_filtered_data(list("iris" = list(dataset = iris), "iris" = list(dataset = iris))),
    regexp = "Assertion on 'x' failed: Must have unique names, but element 2 is duplicated."
  )
})

testthat::test_that("init_filtered_data.default asserts code is `CodeClass`", {
  testthat::expect_error(
    init_filtered_data(list("iris" = list(dataset = iris)), code = "test"),
    regexp = "Assertion on 'code' failed: Must inherit from class 'CodeClass', but has class 'character'."
  )
})

testthat::test_that("init_filtered_data.default accepts NULL passed to code", {
  testthat::expect_error(init_filtered_data(list("iris" = list(dataset = iris)), code = NULL), regexp = NA)
})

testthat::test_that("init_filtered_data.default asserts join_keys is `JoinKeys`", {
  testthat::expect_error(
    init_filtered_data(list("iris" = list(dataset = iris)), join_keys = "test"),
    regexp = "Assertion on 'join_keys' failed: Must inherit from class 'JoinKeys', but has class 'character'."
  )
})

testthat::test_that("init_filtered_data.default accepts NULL passed to join_keys", {
  testthat::expect_error(init_filtered_data(list("iris" = list(dataset = iris)), join_keys = NULL), regexp = NA)
})

testthat::test_that("init_filtered_data.default asserts cdisc is logical(1)", {
  testthat::expect_error(
    init_filtered_data(list("iris" = list(dataset = iris)), cdisc = "test"),
    regexp = "Assertion on 'allowed_parent' failed: Must be of type 'logical flag', not 'character'."
  )
})

testthat::test_that("init_filtered_data.default asserts check is logical(1)", {
  testthat::expect_error(
    init_filtered_data(list("iris" = list(dataset = iris)), check = "test"),
    regexp = "Assertion on 'check' failed: Must be of type 'logical flag', not 'character'."
  )
})
