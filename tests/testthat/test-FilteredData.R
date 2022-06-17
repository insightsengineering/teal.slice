testthat::test_that("The constructor does not throw", {
  testthat::expect_error(FilteredData$new(list(iris = list(dataset = iris)), keys = NULL), NA)
})

# TODO test set_dataset

testthat::test_that("get_keys returns an empty character when data has no keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = character(0))), keys = NULL)
  testthat::expect_equal(filtered_data$get_keys("iris"), character(0))
})

test_that("get_keys returns the same character array if a TealDataset has keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = "test")), keys = NULL)
  testthat::expect_equal(filtered_data$get_keys("iris"), "test")
})

testthat::test_that("get_varnames returns dataname's column names", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  testthat::expect_equal(filtered_data$get_varnames("iris"), colnames(iris))
})

testthat::test_that("get_varlabels returns an array of NAs when dataset has no variable labels", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep(as.character(NA), ncol(iris)), nm = colnames(iris))
  )
})

testthat::test_that("get_varlabels returns array's labels when dataset has variable labels", {
  mock_iris <- head(iris)
  formatters::var_labels(mock_iris) <- rep("test", ncol(mock_iris))
  filtered_data <- FilteredData$new(list(iris = list(dataset = mock_iris)), keys = NULL)
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep("test", ncol(mock_iris)), nm = colnames(mock_iris))
  )
})

testthat::test_that("get_datalabel returns character(0) for a dataset with no labels", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  testthat::expect_equal(filtered_data$get_datalabel("iris"), character(0))
})

testthat::test_that("get_datalabel returns the label of a passed dataset", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), label = "test")), keys = NULL)
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "test")
})

testthat::test_that("get_metadata throws error if dataset does not exist", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  testthat::expect_error(filtered_data$get_metadata("mtcars"), "data mtcars is not available")
})

testthat::test_that("get_metadata returns metadata if dataset exists", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris), metadata = list(E = TRUE)),
      iris2 = list(dataset = head(iris))
    ),
    keys = NULL
  )
  testthat::expect_equal(filtered_data$get_metadata("iris"), list(E = TRUE))
  testthat::expect_null(filtered_data$get_metadata("iris2"))
})

testthat::test_that("get_code return the code passed to set_code", {
  code <- teal.data:::CodeClass$new()
  code$set_code("'preprocessing code'", "iris")
  filtered_data <- FilteredData$new(
    list(iris = list(dataset = head(iris))),
    keys = NULL,
    code = code
  )
  testthat::expect_equal(filtered_data$get_code(), "\"preprocessing code\"")
})

testthat::test_that("get_data does not throw when passed a dataset name", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  testthat::expect_equal(isolate(filtered_data$get_data("iris")), head(iris))
})

testthat::test_that("get_filtered_dataset returns a list of FilteredDataset", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  checkmate::expect_list(filtered_data$get_filtered_dataset(), "FilteredDataset")
})

testthat::test_that("get_filtered_dataset returns a list with elements named after set datasets", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris)),
      mtcars = list(dataset = head(mtcars))
    ),
    keys = NULL
  )
  testthat::expect_equal(names(filtered_data$get_filtered_dataset()), c("iris", "mtcars"))
})

testthat::test_that("get_call returns a list of language objects", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), keys = NULL)
  checkmate::expect_list(filtered_data$get_call("iris"), types = "<-")
})

testthat::test_that("get call returns a call assigning the filtered object to <name>_FILTERED", {
  mock_iris <- head(iris)
  filtered_data <- FilteredData$new(list(mock_iris = list(dataset = mock_iris)), keys = NULL)
  eval(filtered_data$get_call("mock_iris")[[1]])
  testthat::expect_equal(mock_iris_FILTERED, mock_iris)
})

testthat::test_that(
  "FilteredData$set_filter_state sets filters in FilteredDataset specified by the named list",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      ),
      keys = NULL
    )

    fs <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = list()
      )
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_equal(
      isolate(datasets$get_call("iris")),
      list(
        filter = quote(
          iris_FILTERED <- dplyr::filter( # nolint
            iris,
            Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
              Species %in% c("setosa", "versicolor")
          )
        )
      )
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars_FILTERED <- dplyr::filter( # nolint
            mtcars,
            cyl %in% c("4", "6")
          )
        )
      )
    )
  }
)

testthat::test_that(
  "FilteredData$set_filter_state throws error with unnamed datasets list",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      ),
      keys = NULL
    )
    fs <- list(
      list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = list()
      )
    )
    testthat::expect_error(datasets$set_filter_state(state = fs))
  }
)

testthat::test_that(
  "FilteredData$set_filter_state throws error with unnamed variables list",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      ),
      keys = NULL
    )
    fs <- list(
      iris = list(
        list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = list()
      )
    )
    testthat::expect_error(datasets$set_filter_state(state = fs))
  }
)

testthat::test_that("FilteredData$get_filter_state returns list identical to input",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars),
        mae = list(dataset = miniACC)
      ),
      keys = NULL
    )

    fs <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      ),
      mae = list(
        subjects = list(
          years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
          vital_status = list(selected = "1", keep_na = FALSE),
          gender = list(selected = "female", keep_na = TRUE)
        ),
        RPPAArray = list(
          subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
        )
      )
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_identical(isolate(datasets$get_filter_state()), fs)
  }
)

testthat::test_that("FilteredData$remove_filter_state removes states defined in list", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    ),
    keys = NULL
  )
  fs <- list(
    iris = list(
      Sepal.Length = list(c(5.1, 6.4)),
      Species = c("setosa", "versicolor")
    ),
    mtcars = list(
      cyl = c(4, 6),
      disp = list()
    )
  )
  datasets$set_filter_state(state = fs)
  datasets$remove_filter_state(state = list(iris = "Sepal.Length", mtcars = c("cyl", "disp")))

  testthat::expect_identical(
    isolate(datasets$get_filter_state()),
    list(
      iris = list(
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      )
    )
  )
})

testthat::test_that(
  "FilteredData$remove_all_filter_states removes all filters of all datasets in FilteredData",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      ),
      keys = NULL
    )
    fs <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = list()
      )
    )
    datasets$set_filter_state(state = fs)
    datasets$remove_all_filter_states()

    testthat::expect_equal(
      isolate(datasets$get_call("iris")),
      list(filter = quote(iris_FILTERED <- iris)) # nolint
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars_FILTERED <- mtcars # nolint
        )
      )
    )
  }
)

testthat::test_that(
  "FilteredData$remove_all_filter_states remove the filters of the desired dataset only",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      ),
      keys = NULL
    )
    fs <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = list()
      )
    )
    datasets$set_filter_state(state = fs)
    datasets$remove_all_filter_states(datanames = "iris")

    testthat::expect_equal(
      isolate(datasets$get_call("iris")),
      list(filter = quote(iris_FILTERED <- iris)) # nolint
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars_FILTERED <- dplyr::filter( # nolint
            mtcars,
            cyl %in% c("4", "6")
          )
        )
      )
    )
  }
)


get_filtered_data_object <- function() {
  utils::data(miniACC, package = "MultiAssayExperiment")
  adsl <- as.data.frame(as.list(setNames(nm = c(teal.data::get_cdisc_keys("ADSL")))))
  adsl$sex <- c("F")

  CDISCFilteredData$new(
    list(
      ADSL = list(dataset = adsl, keys = teal.data::get_cdisc_keys("ADSL"), parent = character(0)),
      mock_iris = list(dataset = head(iris)),
      miniACC = list(dataset = miniACC)
    ),
    keys = NULL
  )
}


testthat::test_that("get_filter_overview accepts all datasets argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_error(isolate(datasets$get_filter_overview("all")), NA)
})

testthat::test_that("get_filter_overview accepts single dataset argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_error(isolate(datasets$get_filter_overview("ADSL")), NA)
  testthat::expect_error(isolate(datasets$get_filter_overview("mock_iris")), NA)
  testthat::expect_error(isolate(datasets$get_filter_overview("miniACC")), NA)
})

testthat::test_that("get_filter_overview throws error with empty argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_error(isolate(datasets$get_filter_overview()), "argument \"datanames\" is missing, with no default")
})

testthat::test_that("get_filter_overview throws error with wrong argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_error(isolate(datasets$get_filter_overview("AA")), "Some datasets are not available:")
  testthat::expect_error(isolate(datasets$get_filter_overview("")), "Some datasets are not available:")
  testthat::expect_error(isolate(datasets$get_filter_overview(23)), "Some datasets are not available:")
})

testthat::test_that("get_filter_overview returns overview matrix for non-filtered datasets", {
  datasets <- get_filtered_data_object()
  testthat::expect_equal(
    isolate(datasets$get_filter_overview(datasets$datanames())),
    matrix(
      list(
        "1/1", "1/1", "6/6", "6/6", "", "92/92", "79/79", "79/79", "90/90",
        "90/90", "46/46", "46/46", "90/90", "90/90", "80/80", "80/80"
      ),
      nrow = 8,
      byrow = TRUE,
      dimnames = list(
        c(
          "ADSL", "mock_iris", "miniACC", "- RNASeq2GeneNorm", "- gistict",
          "- RPPAArray", "- Mutations", "- miRNASeqGene"
        ),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("get_filter_overview returns overview matrix for filtered datasets", {
  datasets <- get_filtered_data_object()
  filter_state_adsl <- ChoicesFilterState$new(c("F", "M"), varname = "sex")
  filter_state_adsl$set_selected("M")
  queue <- datasets$get_filtered_dataset("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")
  filter_state_mae <- ChoicesFilterState$new(
    x = c("white", NA),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )
  filter_state_mae$set_na_rm(TRUE)
  queue <- datasets$get_filtered_dataset("miniACC")$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")
  testthat::expect_equal(
    isolate(datasets$get_filter_overview(datasets$datanames())),
    matrix(
      list(
        "0/1", "0/1", "6/6", "6/6", "", "78/92", "66/79", "66/79", "76/90",
        "76/90", "35/46", "35/46", "77/90", "77/90", "67/80", "67/80"
      ),
      nrow = 8,
      byrow = TRUE,
      dimnames = list(
        c(
          "ADSL", "mock_iris", "miniACC", "- RNASeq2GeneNorm", "- gistict",
          "- RPPAArray", "- Mutations", "- miRNASeqGene"
        ),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("restore_state_from_bookmark is a pure virtual method", {
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), keys = NULL)$restore_state_from_bookmark("test"),
    regexp = "Pure virtual method"
  )
})

testthat::test_that("get_filter_expr returns a string with a filtering expression", {
  datasets <- FilteredData$new(
    list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)),
    keys = NULL
  )
  testthat::expect_equal(
    get_filter_expr(datasets),
    paste("iris_FILTERED <- iris", "mtcars_FILTERED <- mtcars", sep = "\n")
  )
})

testthat::test_that("FilteredData from TealData preserves the check field when check is FALSE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    keys = NULL,
    code = code,
    check = FALSE
  )
  testthat::expect_false(filtered_data$get_check())
})

testthat::test_that("FilteredData preserves the check field when check is TRUE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    keys = NULL,
    code = code,
    check = TRUE
  )
  testthat::expect_true(filtered_data$get_check())
})



#TODO
# testthat::test_that("get_data(FALSE) returns the object passed to the constructor", {
#   filtered_dataset <- FilteredDataset$new(
#     dataset = head(iris), dataname = "iris"
#   )
#   testthat::expect_equal(filtered_dataset$get_data(filtered = FALSE), head(iris))
# })
#
# testthat::test_that("get_data(TRUE) throws an error due to Pure virtual method.", {
#   filtered_dataset <- FilteredDataset$new(
#     dataset = head(iris), dataname = "iris"
#   )
#   testthat::expect_error(isolate(filtered_dataset$get_data(filtered = TRUE)), regex = "Pure virtual method.")
# })
#
# testthat::test_that("get_data throws an error when filtered input is not logical.", {
#   filtered_dataset <- FilteredDataset$new(dataset = head(iris), dataname = "iris")
#   testthat::expect_error(
#     isolate(filtered_dataset$get_data(filtered = "TRUE")),
#     "Assertion on 'filtered' failed: Must be of type 'logical', not 'character'."
#   )
#   testthat::expect_error(
#     isolate(filtered_dataset$get_data(filtered = 1)),
#     "Assertion on 'filtered' failed: Must be of type 'logical', not 'double'."
#   )
#   testthat::expect_error(
#     isolate(filtered_dataset$get_data(filtered = list(TRUE))),
#     "Assertion on 'filtered' failed: Must be of type 'logical', not 'list'."
#   )
# })
#
# testthat::test_that("get_data_reactive throws an error due to pure virtual method", {
#   filtered_dataset <- FilteredDataset$new(
#     dataset = head(iris), dataname = "iris"
#   )
#   testthat::expect_error(isolate(filtered_dataset$get_data_reactive()()), regex = "Pure virtual method")
# })
# testthat::test_that("MAEFilteredDataset$get_data throws error without filtered argument given", {
#   utils::data(miniACC, package = "MultiAssayExperiment")
#   filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
#   expect_error(isolate(filtered_dataset$get_data()), "argument \"filtered\" is missing, with no default")
# })
#
# testthat::test_that("MAEFilteredDataset$get_data returns identical filtered and
#                     non-filtered MAE data when no filter is applied", {
#                       utils::data(miniACC, package = "MultiAssayExperiment")
#                       filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
#                       filtered_mae <- isolate(filtered_dataset$get_data(filtered = TRUE))
#                       non_filtered_mae <- isolate(filtered_dataset$get_data(filtered = FALSE))
#                       expect_identical(filtered_mae, non_filtered_mae)
#                     })
#
# testthat::test_that("MAEFilteredDataset get_data returns filtered MAE data when filter is applied", {
#   utils::data(miniACC, package = "MultiAssayExperiment")
#   filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
#   filter_state_mae <- ChoicesFilterState$new(
#     x = miniACC$race,
#     varname = as.name("race"),
#     input_dataname = as.name("miniACC"),
#     extract_type = "list"
#   )
#
#   filter_state_mae$set_selected("white")
#   filter_state_mae$set_na_rm(TRUE)
#
#   queue <- filtered_dataset$get_filter_states(1)
#   queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")
#
#   filtered_mae <- isolate(filtered_dataset$get_data(filtered = TRUE))
#   non_filtered_mae <- isolate(filtered_dataset$get_data(filtered = FALSE))
#
#   testthat::expect_false(identical(filtered_mae, non_filtered_mae))
#   testthat::expect_identical(unique(filtered_mae$race), "white")
# })
#
# testthat::test_that("MAEFilteredDataset$get_data throws error when filtered input is not logical", {
#   utils::data(miniACC, package = "MultiAssayExperiment")
#   filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
#   testthat::expect_error(
#     isolate(filtered_dataset$get_data(filtered = "TRUE")),
#     "Assertion on 'filtered' failed: Must be of type 'logical', not 'character'."
#   )
#   testthat::expect_error(
#     isolate(filtered_dataset$get_data(filtered = 1)),
#     "Assertion on 'filtered' failed: Must be of type 'logical', not 'double'."
#   )
#   testthat::expect_error(
#     isolate(filtered_dataset$get_data(filtered = list(TRUE))),
#     "Assertion on 'filtered' failed: Must be of type 'logical', not 'list'."
#   )
# })
