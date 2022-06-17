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

test_that("load and set_datasets", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  expect_silent({
    testthat::expect_equal(ds$get_data("ADSL", filtered = FALSE), setup_objects$adsl)
    testthat::expect_equal(ds$get_data("ADAE", filtered = FALSE), setup_objects$adae)
  })
  expect_setequal(ds$datanames(), c("ADSL", "ADAE"))
})

test_that("set filter state", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  adsl <- setup_objects$adsl

  filter_state_adsl <- ChoicesFilterState$new(adsl$sex, varname = "sex")
  filter_state_adsl$set_selected("F")

  queue <- ds$get_filtered_dataset("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")

  expect_identical(
    isolate(queue$get_call()),
    quote(ADSL_FILTERED <- ADSL) # nolint
  )
})

test_that("get_varlabels returns the column labels of the passed dataset", {
  setup_objects <- get_cdisc_filtered_data()
  ds <- setup_objects$ds
  adsl <- setup_objects$adsl


  expect_equal(
    ds$get_varlabels("ADSL"),
    formatters::var_labels(adsl, fill = FALSE)
  )
  # only some variables
  expect_equal(
    ds$get_varlabels("ADSL", variables = c("sex")),
    formatters::var_labels(adsl, fill = FALSE)[c("sex")]
  )
})

test_that("get_filterable_varnames does not return child duplicates", {
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
  expect_identical(
    fd$get_filterable_varnames("ADTTE"),
    c("PARAMCD", "c")
  )
})

test_that("get_filterable_varnames return all from parent dataset", {
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

  expect_identical(
    fd$get_filterable_varnames("ADSL"),
    c("USUBJID", "STUDYID", "a", "b")
  )
})

test_that("set_filter_state returns warning when setting a filter on a column which belongs to parent dataset", {
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
})
