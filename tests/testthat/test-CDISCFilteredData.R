get_cdisc_filtered_data <- function() {
  adsl <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
  adae <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADAE"))))
  adsl$sex <- "F"
  adae$sex <- "F"
  adae$aval <- 1L
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

  shiny::isolate(ds$set_filter_state(fs))
  adae_call <- shiny::isolate(ds$get_call("ADAE"))

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

testthat::test_that(
  "set_filter_state sets don't throw if column exists and isn't duplicated with parent",
  code = {
    setup_objects <- get_cdisc_filtered_data()
    ds <- setup_objects$ds
    testthat::expect_no_error(
      ds$set_filter_state(list(ADAE = list(aval = 1L)))
    )
  }
)

testthat::test_that(
  "set_filter_state returns warning when setting a filter on a column which belongs to parent dataset",
  code = {
    teal.logger::suppress_logs()
    setup_objects <- get_cdisc_filtered_data()
    ds <- setup_objects$ds
    testthat::expect_warning(
      ds$set_filter_state(list(ADAE = list(USUBJID = "USUBJID"))),
      "These columns filters were excluded: USUBJID from dataset ADAE"
    )
  }
)

testthat::test_that(
  "set_filter_state returns warning when setting a filter on a column which doesn't exist",
  code = {
    teal.logger::suppress_logs()
    setup_objects <- get_cdisc_filtered_data()
    ds <- setup_objects$ds
    testthat::expect_warning(
      ds$set_filter_state(list(ADAE = list(idontexist = "1"))),
      "These columns filters were excluded: idontexist from dataset ADAE"
    )
  }
)
