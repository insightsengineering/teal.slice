test_that("expect_identical_slice works for POSIXct selections", {
  choices <- Sys.time() + seq_len(100)
  tss <- teal_slice(
    dataname = "ADSL",
    varname = "EOSDT",
    selected = sample(choices, 1),
    choices = choices,
    fixed = TRUE
  )

  tss_2 <- tss

  expect_identical_slice(tss, tss_2)
})

test_that("expect_identical_slice works for character selections", {
  choices <- c("JPN", "USA", "CHE", "CAN")
  tss <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = sample(choices, 1),
    choices = choices,
    fixed = TRUE
  )

  tss_2 <- tss

  expect_identical_slice(tss, tss_2)
})

test_that("expect_identical_slice works for numeric selections", {
  choices <- sample(seq_len(1000), 500)
  tss <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = choices[1],
    choices = choices,
    fixed = TRUE
  )

  tss_2 <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = choices[1],
    choices = choices,
    fixed = TRUE
  )

  expect_identical_slice(tss, tss_2)
})

test_that("expect_identical_slice detects missing choices", {
  choices <- c("JPN", "USA", "CHE", "CAN")
  tss <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = sample(choices, 1),
    choices = choices,
    fixed = TRUE
  )

  tss_2 <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = sample(choices, 1),
    choices = sample(choices, length(choices) - 1),
    fixed = TRUE
  )

  expect_failure(
    expect_identical_slice(tss, tss_2)
  )
})

test_that("expect_identical_slice detects missing fields", {
  choices <- c("JPN", "USA", "CHE", "CAN")
  tss <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = sample(choices, 1),
    choices = choices,
    fixed = TRUE
  )

  tss_2 <- teal_slice(
    dataname = "ADSL",
    varname = "COUNTRY",
    selected = sample(choices, 1),
    fixed = TRUE
  )

  expect_failure(
    expect_identical_slice(tss, tss_2)
  )
})
