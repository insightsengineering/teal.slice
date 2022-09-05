get_cdisc_filtered_data <- function() {
  adsl <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
  adsl$sex <- "F"
  adae <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADAE"))))
  formatters::var_labels(adsl) <- colnames(adsl)

  data <- teal.data::cdisc_data(
    teal.data::cdisc_dataset("ADSL", adsl),
    teal.data::cdisc_dataset("ADAE", adae)
  )

  list(ds = init_filtered_data(data), adsl = adsl, adae = adae)
}

testthat::test_that("load and set_datasets", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  testthat::expect_silent({
    testthat::expect_equal(ds$get_data("ADSL", filtered = FALSE), setup_objects$adsl)
    testthat::expect_equal(ds$get_data("ADAE", filtered = FALSE), setup_objects$adae)
  })
  testthat::expect_setequal(ds$datanames(), c("ADSL", "ADAE"))
})

testthat::test_that("set filter state", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  adsl <- setup_objects$adsl

  filter_state_adsl <- ChoicesFilterState$new(adsl$sex, varname = "sex")
  filter_state_adsl$set_selected("F")

  queue <- ds$get_filtered_dataset("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")

  testthat::expect_null(
    isolate(queue$get_call()),
  )
})

testthat::test_that("get_call for child dataset includes filter call for parent dataset", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds

  fs <- list(
    ADSL = list(
      sex = list(selected = "F")
    ),
    ADAE = list(
      AESEQ = list(selected = "AESEQ")
    )
  )

  ds$set_filter_state(fs)
  adae_call <- isolate(ds$get_call("ADAE"))

  # no filtering as AESEQ filter does not filter any calls
  testthat::expect_equal(
    deparse1(adae_call[[1]]),
    paste0(
      "ADAE <- dplyr::inner_join(x = ADAE, y = ADSL[, ",
      "c(\"STUDYID\", \"USUBJID\"), drop = FALSE], by = c(\"STUDYID\", ",
      "\"USUBJID\"))"
    )
  )
})

testthat::test_that("get_parentname returns the parent name of the datasets", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  testthat::expect_identical(
    ds$get_parentname("ADSL"), character(0)
  )
  testthat::expect_equal(
    ds$get_parentname("ADAE"), "ADSL"
  )
})

testthat::test_that("get_varlabels returns the column labels of the passed dataset", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  adsl <- setup_objects$adsl


  testthat::expect_equal(
    ds$get_varlabels("ADSL"),
    formatters::var_labels(adsl, fill = FALSE)
  )
  # only some variables
  testthat::expect_equal(
    ds$get_varlabels("ADSL", variables = c("sex")),
    formatters::var_labels(adsl, fill = FALSE)[c("sex")]
  )
})

testthat::test_that("get_filterable_varnames does not return child duplicates", {
  adsl <- teal.data::cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- teal.data::cdisc_dataset(
    dataname = "ADTTE",
    parent = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- teal.data::cdisc_data(adsl, child)

  fd <- init_filtered_data(data)
  testthat::expect_identical(
    fd$get_filterable_varnames("ADTTE"),
    c("PARAMCD", "c")
  )
})


testthat::test_that("get_filterable_varnames does not return child non-filterable variables", {
  adsl <- teal.data::cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- teal.data::cdisc_dataset(
    dataname = "ADTTE",
    parent = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- teal.data::cdisc_data(adsl, child)

  fd <- init_filtered_data(data)

  fd$set_filterable_varnames("ADTTE", c("PARAMCD", "STUDYID"))
  testthat::expect_identical(
    fd$get_filterable_varnames("ADTTE"),
    "PARAMCD"
  )
})

testthat::test_that("get_filterable_varnames return all filterable variables from parent dataset", {
  adsl <- teal.data::cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- teal.data::cdisc_dataset(
    dataname = "ADTTE",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- teal.data::cdisc_data(adsl, child)

  fd <- init_filtered_data(data)

  testthat::expect_identical(
    fd$get_filterable_varnames("ADSL"),
    c("USUBJID", "STUDYID", "a", "b")
  )

  fd$set_filterable_varnames("ADSL", c("a", "b"))
  testthat::expect_identical(
    fd$get_filterable_varnames("ADSL"),
    c("a", "b")
  )
})


testthat::test_that("get_filterable_varnames does not return duplicates from parent even if they are not filterable", {
  adsl <- teal.data::cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- teal.data::cdisc_dataset(
    dataname = "ADTTE",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- teal.data::cdisc_data(adsl, child)

  fd <- init_filtered_data(data)

  fd$set_filterable_varnames("ADSL", c("a", "b"))

  testthat::expect_identical(
    fd$get_filterable_varnames("ADTTE"),
    c("PARAMCD", "c")
  )
})


testthat::test_that(
  "set_filter_state returns warning when setting a filter on a column which belongs to parent dataset",
  code = {
    teal.logger::suppress_logs()
    adsl <- teal.data::cdisc_dataset(
      dataname = "ADSL",
      x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
    )
    child <- teal.data::cdisc_dataset(
      dataname = "ADTTE",
      parent = "ADSL",
      x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
    )
    data <- teal.data::cdisc_data(adsl, child)

    fd <- init_filtered_data(data)
    testthat::expect_warning(
      fd$set_filter_state(list(ADTTE = list(USUBJID = "1"))),
      "These columns filters were excluded: USUBJID from dataset ADTTE"
    )
  }
)
