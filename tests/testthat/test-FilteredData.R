testthat::test_that("The constructor does not throw", {
  testthat::expect_error(FilteredData$new(list(iris = list(dataset = iris)), join_keys = NULL), NA)
})

testthat::test_that("set_dataset accepts a `data.frame` object", {
  filtered_data <- FilteredData$new(data_objects = list(), join_keys = NULL)
  dataset_args <- list(dataset = iris)
  testthat::expect_error(filtered_data$set_dataset(dataset_args = dataset_args, dataname = "iris"), regexp = NA)
})

testthat::test_that("set_dataset returns self", {
  filtered_data <- FilteredData$new(data_objects = list(), join_keys = NULL)
  dataset_args <- list(dataset = iris)
  testthat::expect_identical(filtered_data$set_dataset(dataset_args = dataset_args, dataname = "iris"), filtered_data)
})

testthat::test_that("get_keys returns an empty character when dataset has no keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = character(0))), join_keys = NULL)
  testthat::expect_equal(filtered_data$get_keys("iris"), character(0))
})

testthat::test_that("get_keys returns the same character array if a dataset has keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = "test")), join_keys = NULL)
  testthat::expect_equal(filtered_data$get_keys("iris"), "test")
})

testthat::test_that("get_join_keys returns NULL if no join_keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = "test")), join_keys = NULL)
  testthat::expect_null(filtered_data$get_join_keys())
})

testthat::test_that("get_join_keys returns join_keys object if it exists", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris)),
      iris2 = list(dataset = head(iris))
    ),
    join_keys = teal.data::join_keys(
      teal.data::join_key("iris", "iris2", c("Species" = "Species"))
    )
  )
  testthat::expect_equal(
    filtered_data$get_join_keys(),
    teal.data::join_keys(
      teal.data::join_key("iris", "iris2", c("Species" = "Species"))
    )
  )
})

testthat::test_that("get_varnames returns dataname's column names", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  testthat::expect_equal(filtered_data$get_varnames("iris"), colnames(iris))
})

testthat::test_that("get_varlabels returns an array of NAs when dataset has no variable labels", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep(as.character(NA), ncol(iris)), nm = colnames(iris))
  )
})

testthat::test_that("get_varlabels returns array's labels when dataset has variable labels", {
  mock_iris <- head(iris)
  formatters::var_labels(mock_iris) <- rep("test", ncol(mock_iris))
  filtered_data <- FilteredData$new(list(iris = list(dataset = mock_iris)), join_keys = NULL)
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep("test", ncol(mock_iris)), nm = colnames(mock_iris))
  )
})

testthat::test_that("get_datalabel returns character(0) for a dataset with no labels", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  testthat::expect_equal(filtered_data$get_datalabel("iris"), character(0))
})

testthat::test_that("get_datalabel returns the label of a passed dataset", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), label = "test")), join_keys = NULL)
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "test")
})

testthat::test_that("get_metadata throws error if dataset does not exist", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  testthat::expect_error(filtered_data$get_metadata("mtcars"), "data mtcars is not available")
})

testthat::test_that("get_metadata returns metadata if dataset exists", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris), metadata = list(E = TRUE)),
      iris2 = list(dataset = head(iris))
    ),
    join_keys = NULL
  )
  testthat::expect_equal(filtered_data$get_metadata("iris"), list(E = TRUE))
  testthat::expect_null(filtered_data$get_metadata("iris2"))
})

testthat::test_that("get_code returns the code passed to set_code", {
  code <- teal.data:::CodeClass$new()
  code$set_code("'preprocessing code'", "iris")
  filtered_data <- FilteredData$new(
    list(iris = list(dataset = head(iris))),
    join_keys = NULL,
    code = code
  )
  testthat::expect_equal(filtered_data$get_code(), "\"preprocessing code\"")
})

testthat::test_that("get_code returns a string when FilteredData has no code", {
  filtered_data <- FilteredData$new(data_objects = list(), join_keys = NULL)
  testthat::expect_equal(filtered_data$get_code(), "# No pre-processing code provided")
})

testthat::test_that("get_data does not throw when passed a dataset name", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  testthat::expect_equal(isolate(filtered_data$get_data("iris")), head(iris))
})

testthat::test_that("get_filtered_dataset returns a list of FilteredDataset", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  checkmate::expect_list(filtered_data$get_filtered_dataset(), "FilteredDataset")
})

testthat::test_that("get_filtered_dataset returns a list with elements named after set datasets", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris)),
      mtcars = list(dataset = head(mtcars))
    ),
    join_keys = NULL
  )
  testthat::expect_equal(names(filtered_data$get_filtered_dataset()), c("iris", "mtcars"))
})

testthat::test_that("get_call returns a list of language objects", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = NULL)
  checkmate::expect_list(filtered_data$get_call("iris"), types = "<-", null.ok = TRUE)

  # TODO add a test where it's not NULL
})

testthat::test_that(
  "FilteredData$set_filter_state sets filters in FilteredDataset specified by the named list",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      ),
      join_keys = NULL
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
          iris <- dplyr::filter(
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
          mtcars <- dplyr::filter(
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
      join_keys = NULL
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
      join_keys = NULL
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

testthat::test_that("FilteredData$get_filter_state returns list identical to input with attributes",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars),
        mae = list(dataset = miniACC)
      ),
      join_keys = NULL
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
    attr(fs, "formatted") <- isolate(datasets$get_formatted_filter_state())
    testthat::expect_identical(isolate(datasets$get_filter_state()), fs)
  }
)

testthat::test_that("FilteredData$get_filter_state returns list whose attribute is a character form of the list",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars),
        mae = list(dataset = miniACC)
      ),
      join_keys = NULL
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
    formatted_attr <- isolate(datasets$get_formatted_filter_state())

    testthat::expect_type(formatted_attr, "character")
    testthat::expect_identical(
      attr(isolate(datasets$get_filter_state()), "formatted"),
      formatted_attr
    )
  }
)

testthat::test_that("FilteredData$remove_filter_state removes states defined in list", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    ),
    join_keys = NULL
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
  fs_after_remove <- list(
    iris = list(
      Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
  )
  attr(fs_after_remove, "formatted") <- isolate(datasets$get_formatted_filter_state())

  testthat::expect_identical(
    isolate(datasets$get_filter_state()),
    fs_after_remove
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
      join_keys = NULL
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

    testthat::expect_null(
      isolate(datasets$get_call("iris"))
    )

    testthat::expect_null(
      isolate(datasets$get_call("mtcars"))
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
      join_keys = NULL
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

    testthat::expect_null(
      isolate(datasets$get_call("iris"))
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars <- dplyr::filter(
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
    join_keys = NULL
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
    FilteredData$new(list(iris = list(dataset = iris)), join_keys = NULL)$restore_state_from_bookmark("test"),
    regexp = "Pure virtual method"
  )
})

testthat::test_that("get_filter_expr returns a string with a filtering expression", {
  datasets <- FilteredData$new(
    list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)),
    join_keys = NULL
  )
  testthat::expect_equal(
    get_filter_expr(datasets),
    paste("", sep = "\n")
  )
})

testthat::test_that("FilteredData from TealData preserves the check field when check is FALSE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    join_keys = NULL,
    code = code,
    check = FALSE
  )
  testthat::expect_false(filtered_data$get_check())
})

testthat::test_that("FilteredData preserves the check field when check is TRUE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    join_keys = NULL,
    code = code,
    check = TRUE
  )
  testthat::expect_true(filtered_data$get_check())
})

testthat::test_that("get_data returns the object passed to the constructor", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)), join_keys = NULL)
  testthat::expect_equal(shiny::isolate(filtered_data$get_data("iris")), iris)
})

testthat::test_that("get_data assert the `filtered` argument is logical(1)", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)), join_keys = NULL)
  testthat::expect_error(
    filtered_data$get_data("iris", filtered = "Wrong type"),
    regexp = "Assertion on 'filtered' failed: Must be of type 'logical flag', not 'character'"
  )
})
