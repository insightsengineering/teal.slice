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
