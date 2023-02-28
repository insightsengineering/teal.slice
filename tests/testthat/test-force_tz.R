test_that("assertions work", {
  testthat::expect_error(force_tz(1651), "Assertion.+failed")
  testthat::expect_error(force_tz(as.Date("2020-01-02")), "Assertion.+failed")
  testthat::expect_error(force_tz(Sys.time(), tz = 394875), "Assertion.+failed")
  testthat::expect_error(force_tz(Sys.time(), tz = c("one", "two")), "Assertion.+failed")
})

test_that("time zone is altered", {
  testthat::expect_identical(
    force_tz(as.POSIXct("2000-01-01 12:00:00", tz = "GMT"), tz = "CET"),
    as.POSIXct("2000-01-01 12:00:00", tz = "CET")
  )
  testthat::expect_identical(
    force_tz(as.POSIXct("2000-01-01 12:00:00", tz = "CET"), tz = "PST"),
    as.POSIXct("2000-01-01 12:00:00", tz = "PST")
  )
})
