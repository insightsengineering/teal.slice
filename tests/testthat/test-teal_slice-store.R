test_that("teal_slice store/restore supports saving `POSIXct` timestamps in selected", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.time() + c(-10 * 60 * 60 * 24, -30, 0)

  # ISO8601 does not keep milliseconds
  time_stamps <- as.POSIXct(
    ceiling(as.double(time_stamps)),
    tz = "UTC"
  )

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDTM",
      selected = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  expect_s3_class(tss_restored_list$selected, "POSIXct")

  expect_identical_slice(tss[[1]], tss_restored[[1]])
})

test_that("teal_slice store/restore supports saving `Date` dates in selected", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.Date() + c(-10 * 600, -30, 0)

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDT",
      selected = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  expect_s3_class(tss_restored_list$selected, "Date")

  expect_identical_slice(tss[[1]], tss_restored[[1]])
})

test_that("teal_slice store/restore supports saving `POSIXct` timestamps in choices", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.time() + c(-10 * 60 * 60 * 24, -30, 0)

  # ISO8601 does not keep milliseconds
  time_stamps <- as.POSIXct(
    ceiling(as.double(time_stamps)),
    tz = "UTC"
  )

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDTM",
      selected = sample(time_stamps, 2),
      choices = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  expect_s3_class(tss_restored_list$choices, "POSIXct")

  expect_identical_slice(tss[[1]], tss_restored[[1]])
})

test_that("teal_slice store/restore supports saving `Date` timestamps in choices", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.Date() + c(-10 * 600, -30, 0)

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDT",
      selected = sample(time_stamps, 2),
      choices = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  expect_s3_class(tss_restored_list$choices, "Date")

  expect_identical_slice(tss[[1]], tss_restored[[1]])
})
