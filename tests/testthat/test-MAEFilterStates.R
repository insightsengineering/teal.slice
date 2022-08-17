testthat::test_that("The constructor does not throw", {
  testthat::expect_error(MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  ), NA)
})

testthat::test_that("MAEFilterStates accept vector as an input for varlabels", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris",
    datalabel = character(0),
    varlabels = c("", NA, paste0("varlabel", 1:100)),
    keys = character(0)
  )
  testthat::expect_equal(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

testthat::test_that("get_fun returns the MAE specific subset function", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  testthat::expect_equal(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

testthat::test_that("The constructor initializes a queue", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  testthat::expect_null(filter_states$queue_get(1))
})

testthat::test_that("get_call returns a call filtering an MAE object using ChoicesFilterState", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  filter_state <- ChoicesFilterState$new(
    x = c("white", NA_character_),
    varname = as.name("race"),
    input_dataname = as.name("test"),
    extract_type = "list"
  )
  filter_state$set_na_rm(TRUE)
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- miniACC
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(
    output,
    MultiAssayExperiment::subsetByColData(test, !is.na(test$race) & test$race == "white")
  )
})

testthat::test_that("get_call returns a call filtering an MAE object using RangeFilterState", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  filter_state <- RangeFilterState$new(
    x = miniACC$purity,
    varname = as.name("purity"),
    input_dataname = as.name("test"),
    extract_type = "list"
  )
  filter_state$set_na_rm(TRUE)
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- miniACC
  eval(isolate(filter_states$get_call()))

  min_purity <- min(miniACC$purity, na.rm = TRUE)
  max_purity <- max(miniACC$purity, na.rm = TRUE)

  testthat::expect_equal(
    output,
    MultiAssayExperiment::subsetByColData(
      test,
      !is.na(miniACC$purity) & (miniACC$purity >= min_purity & miniACC$purity <= max_purity)
    )
  )
})

testthat::test_that(
  "MAEFilterStates$set_filter_state sets filters in FilterState(s) specified by the named list",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      years_to_birth = c(30, 50),
      vital_status = 1,
      gender = "female"
    )
    maefs$set_filter_state(state = fs, data = miniACC)

    testthat::expect_equal(
      isolate(maefs$get_call()),
      quote(
        test_filtered <- MultiAssayExperiment::subsetByColData(
          test,
          y = test$years_to_birth >= 30 & test$years_to_birth <= 50 &
            test$vital_status == "1" &
            test$gender == "female"
        )
      )
    )
  }
)

testthat::test_that("MAEFilterStates$set_filter_state updates filter state which was set already", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  maefs <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )

  maefs$set_filter_state(
    state = list(
      years_to_birth = c(30, 50),
      vital_status = 1
    ),
    data = miniACC
  )

  maefs$set_filter_state(
    state = list(
      years_to_birth = c(31, 50),
      gender = "female"
    ),
    data = miniACC
  )

  testthat::expect_equal(
    isolate(maefs$get_filter_state()),
    list(
      years_to_birth = list(selected = c(31, 50), keep_na = FALSE, keep_inf = FALSE),
      vital_status = list(selected = "1", keep_na = FALSE),
      gender = list(selected = "female", keep_na = FALSE)
    )
  )
})

testthat::test_that(
  "MAEFilterStates$set_filter_state throws error when not using a named list",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      c(30, 50),
      vital_status = 1,
      gender = "female"
    )
    testthat::expect_error(maefs$set_filter_state(state = fs, data = miniACC))
  }
)

testthat::test_that(
  "MAEFilterStates$get_filter_state returns list identical to input",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
      vital_status = list(selected = "1", keep_na = FALSE),
      gender = list(selected = "female", keep_na = TRUE)
    )
    maefs$set_filter_state(state = fs, data = miniACC)
    testthat::expect_equal(isolate(maefs$get_filter_state()), fs)
  }
)

testthat::test_that(
  "MAEFilterStates$remove_filter_state removes filters in FilterState(s)",
  code = {
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      years_to_birth = c(30, 50),
      vital_status = 1,
      gender = "female"
    )
    years_to_birth_remove_fs <- "years_to_birth"

    maefs$set_filter_state(state = fs, data = miniACC)
    maefs$remove_filter_state(years_to_birth_remove_fs)

    testthat::expect_equal(
      isolate(maefs$get_call()),
      quote(
        test_filtered <- MultiAssayExperiment::subsetByColData(
          test,
          y = test$vital_status == "1" &
            test$gender == "female"
        )
      )
    )
  }
)

testthat::test_that(
  "MAEFilterStates$remove_filter_state throws warning when name is not in FilterStates",
  code = {
    teal.logger::suppress_logs()
    utils::data(miniACC, package = "MultiAssayExperiment")
    maefs <- MAEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      years_to_birth = c(30, 50),
      vital_status = 1,
      gender = "female"
    )
    years_to_birth_remove_fs <- "years_to_birth2"

    maefs$set_filter_state(state = fs, data = miniACC)
    testthat::expect_warning(maefs$remove_filter_state(years_to_birth_remove_fs))
  }
)

testthat::test_that(
  "MAEFilterStates$ui_add_filter_state returns a message inside a div when data has no rows or no columns",
  code = {
    maefs <- MAEFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )

    utils::data(miniACC, package = "MultiAssayExperiment")
    x <- miniACC
    x@colData <- MultiAssayExperiment::DataFrame()
    testthat::expect_identical(
      maefs$ui_add_filter_state("id", x),
      div("no sample variables available")
    )

    y <- miniACC
    y@colData <- MultiAssayExperiment::DataFrame(data.frame(A = numeric()))
    testthat::expect_identical(
      maefs$ui_add_filter_state("id", y),
      div("no samples available")
    )
  }
)

# Format
testthat::test_that("$format() is a method of DFFilterStates", {
  testthat::expect_error(MAEFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )$format(), NA)
})

testthat::test_that("$format() asserts the indent argument is a number", {
  testthat::expect_error(
    MAEFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )$format(indent = "wrong type"),
    regexp = "Assertion on 'indent' failed: Must be of type 'number'"
  )
})

testthat::test_that("$format() concatenates its FilterState elements using \\n and indents the FilterState objects", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  maefs <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )

  maefs$set_filter_state(
    state = list(
      years_to_birth = c(30, 50),
      vital_status = 1
    ),
    data = miniACC
  )

  years_to_birth_filter <- maefs$queue_get(1L)[[1]]
  vital_status_filter <- maefs$queue_get(1L)[[2]]
  shiny::isolate(testthat::expect_equal(
    maefs$format(),
    paste(
      "Subject filters:",
      years_to_birth_filter$format(indent = 2),
      vital_status_filter$format(indent = 2),
      sep = "\n"
    )
  ))
})
