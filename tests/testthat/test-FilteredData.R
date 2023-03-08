testthat::test_that("The constructor does not throw with argument dataset specified only", {
  testthat::expect_no_error(FilteredData$new(list(iris = list(dataset = iris))))
})

testthat::test_that("The constructor accepts datasets as list containing data.frame and MAE objects only", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  df_dataset <- list(dataset = iris)
  mae_dataset <- list(dataset = miniACC)
  testthat::expect_no_error(FilteredData$new(list(iris = df_dataset, mae = mae_dataset)))

  dataset <- list(dataset = structure(list(), class = "customclass"))
  testthat::expect_error(FilteredData$new(list(iris = dataset)), "Must inherit")
})

testthat::test_that("The constructor accepts join_keys to be JoinKeys or NULL", {
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), join_keys = teal.data::join_keys())
  )
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)))
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), join_keys = list())
  )
})

testthat::test_that("The constructor accepts code to be CodeClass or NULL", {
  mockcodeclass <- R6::R6Class(classname = "CodeClass")
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), code = mockcodeclass$new())
  )
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), code = NULL)
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), code = list())
  )
})

testthat::test_that("The constructor accepts check to be a flag", {
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), check = TRUE)
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), check = NULL)
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), check = logical(0))
  )
})

testthat::test_that("FilteredData from TealData preserves the check field when check is FALSE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    code = code,
    check = FALSE
  )
  testthat::expect_false(filtered_data$get_check())
})

testthat::test_that("FilteredData preserves the check field when check is TRUE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    code = code,
    check = TRUE
  )
  testthat::expect_true(filtered_data$get_check())
})

testthat::test_that("datanames returns character vector reflecting names of set datasets", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(df1 = dataset, df2 = dataset))
  testthat::expect_identical(filtered_data$datanames(), c("df1", "df2"))
})

testthat::test_that("get_filterable_dataname throws when dataname is not a subset of current datanames", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(iris = dataset))
  testthat::expect_error(filtered_data$get_filterable_datanames("idontexist"))
})

testthat::test_that("get_filterable_dataname returns dataname same as input", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(iris = dataset))
  testthat::expect_identical(filtered_data$get_filterable_datanames("iris"), "iris")
})

testthat::test_that("set_dataset accepts a `data.frame` object", {
  filtered_data <- FilteredData$new(data_objects = list())
  dataset_args <- list(dataset = iris)
  testthat::expect_no_error(filtered_data$set_dataset(dataset_args = dataset_args, dataname = "iris"))
})

testthat::test_that("set_dataset accepts a `data.frame` object", {
  filtered_data <- FilteredData$new(data_objects = list())
  dataset_args <- list(dataset = iris)
  testthat::expect_no_error(filtered_data$set_dataset(dataset_args = dataset_args, dataname = "iris"))
})

testthat::test_that("set_dataset returns self", {
  filtered_data <- FilteredData$new(data_objects = list())
  dataset_args <- list(dataset = iris)
  testthat::expect_identical(filtered_data$set_dataset(dataset_args = dataset_args, dataname = "iris"), filtered_data)
})

testthat::test_that("set_dataset creates FilteredDataset object", {
  testfd <- R6::R6Class(
    classname = "testfd",
    inherit = FilteredData,
    public = list(
      get_filtered_datasets = function() private$filtered_datasets
    )
  )
  filtered_data <- testfd$new(data_objects = list())
  dataset_args <- list(dataset = iris)
  filtered_data$set_dataset(dataset_args = dataset_args, dataname = "iris")
  checkmate::expect_list(
    filtered_data$get_filtered_datasets(),
    types = "DefaultFilteredDataset"
  )
})

testthat::test_that("get_keys returns an empty character when dataset has no keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = character(0))))
  testthat::expect_equal(filtered_data$get_keys("iris"), character(0))
})

testthat::test_that("get_keys returns the same character array if a dataset has keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = "test")))
  testthat::expect_equal(filtered_data$get_keys("iris"), "test")
})

testthat::test_that("get_join_keys returns NULL if no join_keys", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), keys = "test")))
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

testthat::test_that("get_datalabel returns character(0) for a dataset with no labels", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), character(0))
})

testthat::test_that("get_datalabel returns the label of a passed dataset", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), label = "test")))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "test")
})

testthat::test_that("get_metadata throws error if dataset does not exist", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_error(filtered_data$get_metadata("mtcars"), "Assertion on 'dataname'")
})

testthat::test_that("get_metadata returns metadata if dataset exists", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris), metadata = list(E = TRUE)),
      iris2 = list(dataset = head(iris))
    )
  )
  testthat::expect_equal(filtered_data$get_metadata("iris"), list(E = TRUE))
  testthat::expect_null(filtered_data$get_metadata("iris2"))
})

testthat::test_that("get_code returns the code passed to set_code", {
  code <- teal.data:::CodeClass$new()
  code$set_code("'preprocessing code'", "iris")
  filtered_data <- FilteredData$new(
    list(iris = list(dataset = head(iris))),
    code = code
  )
  testthat::expect_equal(filtered_data$get_code(), "\"preprocessing code\"")
})

testthat::test_that("get_code returns a string when FilteredData has no code", {
  filtered_data <- FilteredData$new(data_objects = list())
  testthat::expect_equal(filtered_data$get_code(), "# No pre-processing code provided")
})

testthat::test_that(
  "FilteredData$get_call throws if dataname doesn't match available datasets",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
    )

    testthat::expect_error(shiny::isolate(datasets$get_call(dataname = "idontexist")))
  }
)

testthat::test_that(
  "FilteredData$get_call returns a NULL if no filters applied",
  code = {
    shiny::reactiveConsole(TRUE)
    on.exit(shiny::reactiveConsole(FALSE))
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
    )
    testthat::expect_null(datasets$get_call("iris"))
    testthat::expect_null(datasets$get_call("mtcars"))
  }
)

testthat::test_that(
  "FilteredData$get_call return a list of calls when filter applied",
  code = {
    shiny::reactiveConsole(TRUE)
    on.exit(shiny::reactiveConsole(FALSE))
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
    )
    fs <- list(
      iris = list(Sepal.Length = list(c(5.1, 6.4))),
      mtcars = list(cyl = c(4, 6))
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_identical(
      datasets$get_call("iris"),
      list(
        filter = quote(iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4))
      )
    )
    testthat::expect_identical(
      datasets$get_call("mtcars"),
      list(
        filter = quote(mtcars <- dplyr::filter(mtcars, cyl %in% c(4, 6)))
      )
    )
  }
)

testthat::test_that("get_filter_expr returns empty string when no filters applied", {
  datasets <- FilteredData$new(list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)))
  testthat::expect_identical(shiny::isolate(get_filter_expr(datasets)), "")
})

testthat::test_that(
  "get_filter_expr returns all filter calls as character",
  code = {
    shiny::reactiveConsole(TRUE)
    on.exit(shiny::reactiveConsole(FALSE))
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
    )
    fs <- list(
      iris = list(Sepal.Length = list(c(5.1, 6.4))),
      mtcars = list(cyl = c(4, 6))
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_identical(
      shiny::isolate(get_filter_expr(datasets)),
      paste(
        "iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4)",
        "mtcars <- dplyr::filter(mtcars, cyl %in% c(4, 6))",
        sep = "\n"
      )
    )
  }
)

testthat::test_that("get_data assert the `filtered` argument is logical(1)", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_error(
    filtered_data$get_data("iris", filtered = "Wrong type"),
    regexp = "Assertion on 'filtered' failed: Must be of type 'logical flag', not 'character'"
  )
})

testthat::test_that("get_data requires dataname being a subset of datanames", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_no_error(filtered_data$get_data("iris", filtered = FALSE))
  testthat::expect_error(filtered_data$get_data("mtcars", filtered = FALSE), "Must be a subset")
})

testthat::test_that("get_data filtered = FALSE returns the same object as passed to the constructor", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_equal(filtered_data$get_data("iris", filtered = FALSE), iris)
})

testthat::test_that("get_data returns the same object as passed to the constructor if no filter applied", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_equal(filtered_data$get_data("iris"), iris)
})

testthat::test_that(
  "FilteredData$get_data returns an object filtered by set filters",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris)
      )
    )
    fs <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4))
      )
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_identical(
      shiny::isolate(datasets$get_data("iris")),
      dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4)
    )
  }
)

testthat::test_that(
  "FilteredData$get_filter_state returns list identical to input with attribute format",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
    )

    fs <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      ),
      mtcars = list(
        cyl = list(selected = c("4", "6"), keep_na = FALSE)
      )
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_equal(
      shiny::isolate(datasets$get_filter_state()),
      structure(
        fs,
        formatted = paste0(
          c(
            "Filters for dataset: iris",
            "  Filtering on: Sepal.Length",
            "    Selected range: 5.100 - 6.400",
            "    Include missing values: FALSE",
            "  Filtering on: Species",
            "    Selected values: setosa, versicolor",
            "    Include missing values: FALSE",
            "Filters for dataset: mtcars",
            "  Filtering on: cyl",
            "    Selected values: 4, 6",
            "    Include missing values: FALSE"
          ),
          collapse = "\n"
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
      )
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
      )
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

testthat::test_that(
  "FilteredData$get_filter_state returns list whose attribute is a character form of the list",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars),
        mae = list(dataset = miniACC)
      )
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
    formatted_attr <- shiny::isolate(datasets$get_formatted_filter_state())

    testthat::expect_type(formatted_attr, "character")
    testthat::expect_identical(
      attr(shiny::isolate(datasets$get_filter_state()), "formatted"),
      formatted_attr
    )
  }
)

testthat::test_that("FilteredData$remove_filter_state removes states defined in list", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
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
  testthat::expect_identical(names(shiny::isolate(datasets$get_filter_state())[["iris"]]), "Species")
  testthat::expect_null(names(shiny::isolate(datasets$get_filter_state())[["mtcars"]]))
})

testthat::test_that(
  "FilteredData$clear_filter_states removes all filters of all datasets in FilteredData",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
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
    datasets$clear_filter_states()

    testthat::expect_identical(
      shiny::isolate(datasets$get_filter_state()),
      structure(
        list(),
        names = character(0),
        formatted = ""
      )
    )
  }
)

testthat::test_that(
  "FilteredData$clear_filter_states remove the filters of the desired dataset only",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars)
      )
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
    mtcars_filters <- shiny::isolate(datasets$get_filter_state())[["mtcars"]]
    datasets$clear_filter_states(datanames = "iris")

    testthat::expect_null(shiny::isolate(datasets$get_filter_state()[["iris"]]))
    testthat::expect_identical(shiny::isolate(datasets$get_filter_state())[["mtcars"]], mtcars_filters)
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
    )
  )
}

testthat::test_that("get_filter_overview accepts all datasets argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("all")))
})

testthat::test_that("get_filter_overview accepts single dataset argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("ADSL")))
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("mock_iris")))
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("miniACC")))
})

testthat::test_that("get_filter_overview throws error with empty argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_error(
    shiny::isolate(
      datasets$get_filter_overview()
    ),
    "argument \"datanames\" is missing, with no default"
  )
})

testthat::test_that("get_filter_overview throws error with wrong argument input", {
  datasets <- get_filtered_data_object()
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview("AA")), "Some datasets are not available:")
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview("")), "Some datasets are not available:")
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview(23)), "Some datasets are not available:")
})

testthat::test_that("get_filter_overview returns overview matrix for non-filtered datasets", {
  datasets <- get_filtered_data_object()
  testthat::expect_equal(
    shiny::isolate(datasets$get_filter_overview(datasets$datanames())),
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
  datasets$set_filter_state(
    list(
      mock_iris = list(Sepal.Length = c(5.1, 5.1)),
      miniACC = list(subjects = list(race = "white"))
    )
  )

  testthat::expect_equal(
    shiny::isolate(datasets$get_filter_overview(datasets$datanames())),
    matrix(
      list(
        "1/1", "1/1", "1/6", "1/6", "", "78/92", "66/79", "66/79", "76/90",
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


testthat::test_that("filter_panel_disable", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  filtered_data$set_filter_state(list(iris = list(Sepal.Width = c(3, 4))))
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      filtered_data$filter_panel_disable()
      testthat::expect_length(filtered_data$get_filter_state(), 0)
    }
  )
})

testthat::test_that("filter_panel_enable", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  filtered_data$set_filter_state(list(iris = list(Sepal.Width = c(3, 4))))
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      filtered_data$filter_panel_enable()
      testthat::expect_length(filtered_data$get_filter_state(), 1)
      testthat::expect_equal(filtered_data$get_filter_state()$iris$Sepal.Width$selected, c(3, 4))
    }
  )
})

testthat::test_that("disable/enable_filter_panel caches and restores state", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
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
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      cached <- filtered_data$get_filter_state()
      testthat::expect_true(filtered_data$get_filter_panel_active())
      filtered_data$filter_panel_disable()
      testthat::expect_identical(filtered_data$get_filter_state(), structure(list(a = NULL)[0], formatted = ""))
      testthat::expect_false(filtered_data$get_filter_panel_active())
      filtered_data$filter_panel_enable()
      testthat::expect_identical(filtered_data$get_filter_state(), cached)
      testthat::expect_true(filtered_data$get_filter_panel_active())
    }
  )
})

testthat::test_that("switching disable/enable button caches and restores state", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
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
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      cached <- filtered_data$get_filter_state()
      testthat::expect_true(filtered_data$get_filter_panel_active())
      session$setInputs(filter_panel_active = FALSE)
      testthat::expect_identical(filtered_data$get_filter_state(), structure(list(a = NULL)[0], formatted = ""))
      testthat::expect_false(filtered_data$get_filter_panel_active())
      session$setInputs(filter_panel_active = TRUE)
      testthat::expect_identical(filtered_data$get_filter_state(), cached)
      testthat::expect_true(filtered_data$get_filter_panel_active())
    }
  )
})

testthat::test_that("handle_active_datanames replaces 'all' with vector of available datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  testthat::expect_identical(filtered_data$handle_active_datanames("all"), c("iris", "mtcars"))
})

testthat::test_that("handle_active_datanames returns input datanames if they are subset of active datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  testthat::expect_identical(filtered_data$handle_active_datanames("iris"), "iris")
  testthat::expect_identical(filtered_data$handle_active_datanames(c("iris", "mtcars")), c("iris", "mtcars"))
})

testthat::test_that("handle_active_datanames throws when input dataname is not subset of active datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  testthat::expect_error(filtered_data$handle_active_datanames("idontexist"))
})


testthat::test_that("active_datanames in srv_filter_panel gets resolved to valid datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  shiny::testServer(
    filtered_data$srv_filter_panel,
    args = list(active_datanames = function() "all"),
    expr = {
      testthat::expect_identical(active_datanames_resolved(), c("iris", "mtcars"))
    }
  )
  shiny::testServer(
    filtered_data$srv_filter_panel,
    args = list(active_datanames = function() "iris"),
    expr = {
      testthat::expect_identical(active_datanames_resolved(), c("iris"))
    }
  )
})

testthat::test_that("active_datanames fails if returns dataname which isn't a subset of available datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  shiny::testServer(
    filtered_data$srv_filter_panel,
    args = list(active_datanames = function() c("iris", "idontexist")),
    expr = {
      testthat::expect_error(active_datanames_resolved())
    }
  )
})

testthat::test_that("srv_active - output$teal_filters_count returns (reactive) number of current filters applied", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
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
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_active,
    expr = {
      testthat::expect_identical(output$teal_filters_count, "4 filters applied across datasets")
    }
  )
})

testthat::test_that("srv_active - clicking remove_all button clears filters", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
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
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_active,
    expr = {
      session$setInputs(remove_all_filters = TRUE)
      testthat::expect_identical(
        filtered_data$get_filter_state(),
        structure(list(a = NULL)[0], formatted = "")
      )
    }
  )
})

testthat::test_that("turn filed by default equal to TRUE", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_true(filtered_data$get_filter_panel_active())
})

testthat::test_that("get_filter_panel_ui_id - empty when no shiny session", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_length(filtered_data$get_filter_panel_ui_id(), 0)
})

testthat::test_that("get_filter_panel_ui_id - non-empty when in shiny session", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      testthat::expect_length(filtered_data$get_filter_panel_ui_id(), 1)
    }
  )
})

testthat::test_that(
  "FilteredData$get_filter_count properly tallies active filter states",
  code = {
    datasets <- FilteredData$new(
      list(
        iris = list(dataset = iris),
        mtcars = list(dataset = mtcars),
        mae = list(dataset = miniACC)
      )
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
    shiny::isolate(testthat::expect_equal(datasets$.__enclos_env__$private$get_filter_count(), 0L))
    datasets$set_filter_state(state = fs)
    shiny::isolate(testthat::expect_equal(datasets$.__enclos_env__$private$get_filter_count(), 4L))
  }
)

testthat::test_that(
  "FilteredData$get_filter_count properly tallies active filter states for MAE objects",
  code = {
    datasets <- FilteredData$new(
      list(
        mae = list(dataset = miniACC)
      )
    )
    fs <- list(
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
    shiny::isolate(testthat::expect_equal(datasets$.__enclos_env__$private$get_filter_count(), 0L))
    datasets$set_filter_state(state = fs)
    shiny::isolate(testthat::expect_equal(datasets$.__enclos_env__$private$get_filter_count(), 4L))
  }
)
