testthat::test_that("MAETealDataset constructors do not raise exceptions", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_silent(teal.data::dataset("testMAE", miniACC))
  code_class <- teal.data:::CodeClass$new(
    "utils::data(miniACC, package = \"MultiAssayExperiment\")
    testMAE <- miniACC",
    dataname = "testMAE"
  )
  testthat::expect_silent(
    teal.data::dataset(dataname = "testMAE", x = miniACC, code = code_class)
  )
})

testthat::test_that("MAETealDataset$recreate updates the class fields", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- teal.data::dataset("testMAE", miniACC)

  suppressWarnings(new_data <- miniACC[, , "RNASeq2GeneNorm"]) # warning only on rocker 4.1
  new_name <- "new_name"
  new_label <- "new_label"
  new_code <- "new_code"
  new_keys <- c("new_key")
  new_vars <- list(new_var = "new_var")

  testthat::expect_silent(mae$recreate(
    dataname = new_name,
    x = new_data,
    keys = new_keys,
    code = new_code,
    vars = new_vars
  ))
  testthat::expect_equal(mae$get_dataname(), new_name)
  testthat::expect_equal(mae$get_raw_data(), new_data)
  testthat::expect_equal(mae$get_keys(), new_keys)
  testthat::expect_equal(mae$get_code(), "new_var <- \"new_var\"\nnew_code")
})

testthat::test_that("MAETealDataset getters and setters", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- teal.data::dataset(dataname = "miniACC", x = miniACC)

  testthat::expect_equal(mae$get_dataname(), "miniACC")
  testthat::expect_equal(mae$get_datanames(), mae$get_dataname())

  testthat::expect_equal(mae$get_raw_data(), miniACC)

  new_label <- "new_label"
  testthat::expect_silent(mae$set_dataset_label(new_label))
  testthat::expect_equal(mae$get_dataset_label(), new_label)

  new_keys <- c("new_key")
  testthat::expect_silent(mae$set_keys(new_keys))
  testthat::expect_equal(mae$get_keys(), new_keys)

  new_code <- "new_code"
  testthat::expect_silent(mae$set_code(new_code))
  testthat::expect_equal(mae$get_code(), new_code)

  new_vars <- list(new_var = "new_var")
  testthat::expect_silent(mae$set_vars(new_vars))
})

testthat::test_that("MAETealDataset$is_pulled returns true", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- teal.data::dataset(dataname = "miniACC", x = miniACC)
  testthat::expect_true(mae$is_pulled())
})

testthat::test_that("MAETealDataset$check returns TRUE when constructed with the correct code", {
  exprss1 <- matrix(
    seq(from = 1, by = 0.1, length.out = 16),
    ncol = 4,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jill", "Bob", "Bobby"))
  )
  exprss2 <- matrix(
    seq(from = 5, by = 0.1, length.out = 12),
    ncol = 3,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jane", "Bob"))
  )
  double_exp <- list("methyl 2k" = exprss1, "methyl 3k" = exprss2)
  simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = double_exp)

  mae_dataset <- teal.data::dataset(
    dataname = "simple_mae",
    x = simple_mae,
    code = "exprss1 <- matrix(
      seq(from = 1, by = 0.1, length.out = 16),
      ncol = 4,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jill', 'Bob', 'Bobby'))
    )
    exprss2 <- matrix(
      seq(from = 5, by = 0.1, length.out = 12), ncol = 3,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jane', 'Bob'))
    )
    doubleExp <- list('methyl 2k' = exprss1, 'methyl 3k' = exprss2)
    simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments=doubleExp)"
  )
  testthat::expect_true(mae_dataset$check())
})

testthat::test_that("FALSE returned when executing MAETealDataset$check and code is not correct", {
  exprss1 <- matrix(
    seq(from = 1, by = 0.1, length.out = 16),
    ncol = 4,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jill", "Bob", "Bobby"))
  )
  exprss2 <- matrix(
    seq(from = 5, by = 0.1, length.out = 12),
    ncol = 3,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jane", "Bob"))
  )
  double_exp <- list("methyl 2k" = exprss1, "methyl 3k" = exprss2)
  simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = double_exp)

  mae_dataset <- teal.data::dataset(
    dataname = "simple_mae",
    x = simple_mae,
    code = "exprss1 <- matrix(
      seq(from = 1, by = 0.1, length.out = 16),
      ncol = 4,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jill', 'Bob', 'Bobby'))
    )
    exprss2 <- matrix(
      seq(from = 5, by = 0.1, length.out = 12), ncol = 3,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jane', 'Bob'))
    )
    doubleExp <- list('methyl 1k' = exprss1, 'methyl 3k' = exprss2)
    simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments=doubleExp)"
  )
  testthat::expect_false(mae_dataset$check())
})

testthat::test_that("Error raised when executing MAETealDataset$check and code is empty", {
  exprss1 <- matrix(
    seq(from = 1, by = 0.1, length.out = 16),
    ncol = 4,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jill", "Bob", "Bobby"))
  )
  exprss2 <- matrix(
    seq(from = 5, by = 0.1, length.out = 12),
    ncol = 3,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jane", "Bob"))
  )
  double_exp <- list("methyl 2k" = exprss1, "methyl 3k" = exprss2)
  simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = double_exp)

  mae_dataset <- teal.data::dataset(dataname = "simple_mae", x = simple_mae, code = "")
  testthat::expect_error(
    mae_dataset$check(),
    regexp = "Cannot check preprocessing code of"
  )
})

testthat::test_that("MAETealDataset$check_keys doesn't throw if constructed with correct keys", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- teal.data::dataset(dataname = "miniACC", x = miniACC, keys = "patientID")
  testthat::expect_silent(mae$check_keys())
})

testthat::test_that("MAETealDataset$check_keys throws if constructed with keys not present in colData", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- teal.data::dataset(dataname = "miniACC", x = miniACC, keys = "wrong keys")
  testthat::expect_error(
    mae$check_keys(),
    regexp = "do not exist in the data"
  )
})

testthat::test_that("Error raised executing MAETealDataset$check_keys and duplicate rows found in key columns", {
  array_data <- matrix(
    seq(101, 108),
    ncol = 4,
    dimnames = list(c("probe1", "probe2"), c("sample1", "sample2", "sample3", "sample4"))
  )
  col_data <- data.frame(
    sample_id = c("sample1", "sample2", "sample3", "sample3"),
    row.names = c("sample1", "sample2", "sample3", "sample4")
  )
  test_mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list("test_exp1" = array_data),
    colData = col_data
  )
  mae_dataset <- teal.data::dataset(dataname = "test_mae", x = test_mae, keys = "sample_id")
  testthat::expect_error(
    mae_dataset$check_keys(),
    regexp = "Duplicate primary key values found in the dataset 'test_mae'"
  )
})

testthat::test_that("dataset() does not throw when passed a MultiAssayExperiment object", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_error(teal.data::dataset("mae", miniACC), NA)
})

testthat::test_that("dataset() constructor returns the same as MAETealDataset$new()", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae1 <- teal.data::dataset("mae", miniACC)
  mae2 <- teal.data::dataset("mae", miniACC)
  testthat::expect_equal(mae1, mae2)
})
