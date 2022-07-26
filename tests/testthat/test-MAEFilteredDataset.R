testthat::test_that("MAEFilteredDataset accepts a MultiAssayExperiment object", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_error(
    MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC"),
    NA
  )
})

testthat::test_that("MAEFilteredDataset throws error with a data.frame passed to constructor", {
  testthat::expect_error(
    MAEFilteredDataset$new(dataset = head(iris), dataname = "iris"),
    "Must inherit from class 'MultiAssayExperiment'",
    fixed = TRUE
  )
})

testthat::test_that("MAEFilteredDataset$get_call returns NULL without applying filter", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
  get_call_output <- filtered_dataset$get_call()
  testthat::expect_null(get_call_output)
})

testthat::test_that("MAEFilteredDataset$get_call returns a call with applying filter", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
  filter_state_mae <- ChoicesFilterState$new(
    x = miniACC$race,
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )

  filter_state_mae$set_selected("white")
  filter_state_mae$set_na_rm(TRUE)

  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  get_call_output <- isolate(filtered_dataset$get_call())

  checkmate::expect_list(get_call_output, types = "<-")
  testthat::expect_identical(
    get_call_output$subjects,
    quote(
      miniACC <- MultiAssayExperiment::subsetByColData( # nolint
        miniACC,
        y = !is.na(miniACC$race) & miniACC$race == "white"
      )
    )
  )
})

testthat::test_that("get_filter_overview_info returns overview matrix for MAEFilteredDataset without filtering", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
  testthat::expect_equal(
    isolate(filtered_dataset$get_filter_overview_info()),
    matrix(
      list("", "92/92", "79/79", "79/79", "90/90", "90/90", "46/46", "46/46", "90/90", "90/90", "80/80", "80/80"),
      nrow = 6,
      byrow = TRUE,
      dimnames = list(
        c("miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene"),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("get_filter_overview_info returns overview matrix for MAEFilteredDataset with filtering", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")

  filter_state_mae <- ChoicesFilterState$new(
    x = c("white", NA_character_),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )
  filter_state_mae$set_na_rm(TRUE)
  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  testthat::expect_equal(
    isolate(filtered_dataset$get_filter_overview_info(
      MultiAssayExperiment::subsetByColData(
        miniACC,
        y = !is.na(miniACC$race) & miniACC$race == "white"
      )
    )),
    matrix(
      list("", "78/92", "66/79", "66/79", "76/90", "76/90", "35/46", "35/46", "77/90", "77/90", "67/80", "67/80"),
      nrow = 6,
      byrow = TRUE,
      dimnames = list(
        c("miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene"),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that(
  "MAEFilteredDataset$set_filter_state sets filters in FilterStates specified by list names",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "MAE")
    fs <- list(
      subjects = list(
        years_to_birth = c(30, 50),
        vital_status = 1,
        gender = "female"
      ),
      RPPAArray = list(
        subset = list(ARRAY_TYPE = "")
      )
    )
    dataset$set_filter_state(state = fs)
    testthat::expect_equal(
      isolate(dataset$get_call()),
      list(
        subjects = quote(
          MAE <- MultiAssayExperiment::subsetByColData( # nolint
            MAE,
            y = MAE$years_to_birth >= 30 & MAE$years_to_birth <= 50 &
              MAE$vital_status == "1" &
              MAE$gender == "female"
          )
        ),
        RPPAArray = quote(
          MAE[["RPPAArray"]] <- subset(
            MAE[["RPPAArray"]],
            subset = ARRAY_TYPE == ""
          )
        )
      )
    )
  }
)

testthat::test_that(
  "MAEFilteredDataset$set_filter_state throws error when using unnamed list",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
    fs <- list(
      list(
        years_to_birth = c(30, 50),
        vital_status = 1,
        gender = "female"
      ),
      RPPAArray = list(
        subset = list(ARRAY_TYPE = "")
      )
    )
    testthat::expect_error(dataset$set_filter_state(state = fs))
  }
)

testthat::test_that(
  "MAEFilteredDataset$set_filter_state throws error when using unnamed variables list",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
    fs <- list(
      subjects = list(
        c(30, 50),
        vital_status = 1,
        gender = "female"
      ),
      RPPAArray = list(
        subset = list(ARRAY_TYPE = "")
      )
    )
    testthat::expect_error(dataset$set_filter_state(state = fs))
  }
)

testthat::test_that("MAEFilteredDataset$set_filter_state throws error if state argument is not a list ", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
  fs <- c("not_list")
  testthat::expect_error(
    dataset$set_filter_state(state = fs),
    "Must be of type 'list', not 'character'.",
    fixed = TRUE
  )
})

testthat::test_that(
  "MAEFilteredDataset$get_filter_state returns list identical to input",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
    fs <- list(
      subjects = list(
        years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
        vital_status = list(selected = "1", keep_na = FALSE),
        gender = list(selected = "female", keep_na = TRUE)
      ),
      RPPAArray = list(
        subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
      )
    )
    dataset$set_filter_state(state = fs)
    testthat::expect_identical(isolate(dataset$get_filter_state()), fs)
  }
)

testthat::test_that(
  "MAEFilteredDataset$remove_filter_state removes desired filter",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "MAE")
    fs <- list(
      subjects = list(
        years_to_birth = c(30, 50),
        vital_status = 1,
        gender = "female"
      ),
      RPPAArray = list(
        subset = list(ARRAY_TYPE = "")
      )
    )
    dataset$set_filter_state(state = fs)
    dataset$remove_filter_state(element_id = list(subjects = list("years_to_birth")))

    testthat::expect_equal(
      isolate(dataset$get_call()),
      list(
        subjects = quote(
          MAE <- MultiAssayExperiment::subsetByColData( # nolint
            MAE,
            y = MAE$vital_status == "1" &
              MAE$gender == "female"
          )
        ),
        RPPAArray = quote(
          MAE[["RPPAArray"]] <- subset(
            MAE[["RPPAArray"]],
            subset = ARRAY_TYPE == ""
          )
        )
      )
    )
  }
)

testthat::test_that(
  "MAEFilteredDataset$remove_filter_state throws error if list in unnamed",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
    fs <- list(
      subjects = list(
        years_to_birth = c(30, 50),
        vital_status = 1,
        gender = "female"
      ),
      RPPAArray = list(
        subset = list(ARRAY_TYPE = "")
      )
    )
    dataset$set_filter_state(state = fs)
    testthat::expect_error(dataset$remove_filter_state(element_id = list("years_to_birth")))
  }
)
testthat::test_that("MAEFilteredDataset$get_filterable_varnames returns character(0)", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "miniACC")
  testthat::expect_identical(filtered_dataset$get_filterable_varnames(), character(0))
})

testthat::test_that("MAEFilteredDataset$get_varlabels returns column variable labels", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  x <- miniACC
  attr(SummarizedExperiment::colData(x)$ADS, "label") <- "ADS label"
  filtered_dataset <- MAEFilteredDataset$new(dataset = x, dataname = "miniACC")
  labels <- filtered_dataset$get_varlabels(c("COC", "ADS"))
  testthat::expect_equal(c("COC" = NA, ADS = "ADS label"), labels)
})

testthat::test_that("MAEFilteredDataset filters removed using remove_filters", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filtered_dataset <- MAEFilteredDataset$new(dataset = miniACC, dataname = "MAE")
  fs <- list(
    subjects = list(
      years_to_birth = c(30, 50),
      vital_status = 1,
      gender = "female"
    ),
    RPPAArray = list(
      subset = list(ARRAY_TYPE = "")
    )
  )

  filtered_dataset$set_filter_state(state = fs)

  testthat::expect_identical(
    isolate(filtered_dataset$get_call()),
    list(
      subjects = quote(
        MAE <- MultiAssayExperiment::subsetByColData( # nolint
          MAE,
          y = MAE$years_to_birth >= 30 & MAE$years_to_birth <= 50 &
            MAE$vital_status == "1" &
            MAE$gender == "female"
        )
      ),
      RPPAArray = quote(
        MAE[["RPPAArray"]] <- subset(
          MAE[["RPPAArray"]],
          subset = ARRAY_TYPE == ""
        )
      )
    )
  )

  shiny::testServer(
    filtered_dataset$server,
    expr = {
      session$setInputs(remove_filters = TRUE)
      testthat::expect_true(input$remove_filters)
    }
  )

  testthat::expect_null(isolate(filtered_dataset$get_call()))
})
