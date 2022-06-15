testthat::test_that("init_filtered_dataset returns a DefaultFilteredDataset when passed a data.frame", {
  testthat::expect_error(filtered_dataset <- init_filtered_dataset(
    dataset = head(iris), dataname = "iris"
  ), NA)
  testthat::expect_true(is(filtered_dataset, "DefaultFilteredDataset"))
})

testthat::test_that("init_filtered_dataset returns an MAEFilteredDataset when passed an MAE", {
  utils::data("miniACC", package = "MultiAssayExperiment")

  testthat::expect_error(filtered_dataset <- init_filtered_dataset(
    dataset = miniACC, dataname = "MAE"
  ), NA)
  testthat::expect_true(is(filtered_dataset, "MAEFilteredDataset"))
})
