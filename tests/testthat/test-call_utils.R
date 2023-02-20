# call_condition_choice ----
testthat::test_that("call_condition_choice accept all type of choices - character", {
  testthat::expect_identical(
    call_condition_choice("var", choices = character(0)),
    "var %in% c()"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = "F"),
    "var == \"F\""
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c("A", "B")),
    "var %in% c(\"A\", \"B\")"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c("A", "B", NA)),
    "var %in% c(\"A\", \"B\", \"NA\")"
  )
})

testthat::test_that("call_condition_choice accept all type of choices - integer", {
  testthat::expect_identical(
    call_condition_choice("var", choices = integer(0)),
    "var %in% c()"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = 1L),
    "var == 1"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1L, 2L)),
    "var %in% c(1, 2)"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1L, 2L, NA)),
    "var %in% c(1, 2, NA)"
  )
})

testthat::test_that("call_condition_choice accept all type of choices - numeric", {
  testthat::expect_identical(
    call_condition_choice("var", choices = numeric(0)),
    "var %in% c()"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = 1.1),
    "var == 1.1"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1.1, 2.1)),
    "var %in% c(1.1, 2.1)"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1.1, 2.1, NA, Inf)),
    "var %in% c(1.1, 2.1, NA, Inf)"
  )
})

testthat::test_that("call_condition_choice accept all type of choices - factor", {
  testthat::expect_identical(
    call_condition_choice("var", choices = as.factor(character(0))),
    "var %in% c()"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = factor("F")),
    "var == \"F\""
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = factor(c("A", "B"))),
    "var %in% c(\"A\", \"B\")"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = factor(c("A", "B", NA_character_))),
    "var %in% c(\"A\", \"B\", \"NA\")"
  )
})

testthat::test_that("call_condition_choice accept all type of choices - Date", {
  date <- as.Date("2021-09-01")
  testthat::expect_identical(
    call_condition_choice("var", choices = as.Date(integer(0), origin = "1900-01-01")),
    "var %in% as.Date(c())"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + 1L),
    "var == as.Date(\"2021-09-02\")"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L)),
    "var %in% as.Date(c(\"2021-09-02\", \"2021-09-03\"))"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L, NA_integer_)),
    "var %in% as.Date(c(\"2021-09-02\", \"2021-09-03\", \"NA\"))"
  )
})

testthat::test_that("call_condition_choice accept all type of choices - datetime", {
  Sys.setenv("TZ" = "PST")
  testthat::expect_identical(
    call_condition_choice("var", choices = as.POSIXct(integer(0), origin = "1900-01-01")),
    "var %in% as.POSIXct(c(), tz = \"PST\")"
  )
  date <- as.POSIXct("2021-09-01 12:00:00", tz = "GMT")
  testthat::expect_identical(
    call_condition_choice("var", choices = date + 1L),
    "var == as.POSIXct(\"2021-09-01 12:00:01\", tz = \"GMT\")"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L)),
    "var %in% as.POSIXct(c(\"2021-09-01 12:00:01\", \"2021-09-01 12:00:02\"), tz = \"GMT\")"
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L, NA_integer_)),
    "var %in% as.POSIXct(c(\"2021-09-01 12:00:01\", \"2021-09-01 12:00:02\", \"NA\"), tz = \"GMT\")"
  )
  Sys.unsetenv("TZ")
})

# call_condition_range ----
testthat::test_that("call_condition_range works only with sorted numeric of length 2", {
  testthat::expect_identical(
    call_condition_range("var", range = c(1, 2)),
    "var >= 1.0000000000 & var <= 2.0000000000"
  )
  testthat::expect_equal(
    call_condition_range("var", range = c(-1.2, 2.1)),
    "var >= -1.2000000000 & var <= 2.1000000000"
  )
  testthat::expect_error(
    call_condition_range("var", range = c(2.1, -1.2)),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_range("var", range = c("a", "b")),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_range("var", range = 1),
    "Assertion.+failed"
  )
})

# call_condition_logical ----
testthat::test_that("call_condition_logical works only with logical(1)", {
  testthat::expect_identical(
    call_condition_logical("var", choice = TRUE),
    "var == TRUE"
  )
  testthat::expect_identical(
    call_condition_logical("var", choice = FALSE),
    "var == FALSE"
  )
  testthat::expect_error(
    call_condition_logical("var", choice = c(TRUE, FALSE)),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_logical("var", choice = 1),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_logical("var", choice = "TRUE"),
    "Assertion.+failed"
  )
})

# call_condition_range_posixct ----
testthat::test_that("call_condition_range_posixct works with POXIXct range only", {
  datetime <- as.POSIXct("2021-09-01 12:00:00", tz = "GMT")
  testthat::expect_identical(
    call_condition_range_posixct(
      varname = "var",
      range = datetime + c(0, 1)
    ),
    paste(
      "var >= as.POSIXct(\"2021-09-01 12:00:00\", tz = \"GMT\")",
      "var < as.POSIXct(\"2021-09-01 12:00:02\", tz = \"GMT\")",
      sep = " & "
    )
  )
  testthat::expect_error(
    call_condition_range_posixct(
      varname = "var",
      range = datetime + c(1, 0)
    ),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_range_posixct(
      varname = "var",
      range = Sys.Date() + c(0, 1)
    ),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_range_posixct(
      varname = "var",
      range = Sys.time()
    ),
    "Assertion.+failed"
  )
})

testthat::test_that("call_condition_range_posixct returns expected timezone", {
  datetime <- as.POSIXct("2021-09-01 12:00:00", tz = "GMT")
  testthat::expect_identical(
    call_condition_range_posixct(
      varname = "var",
      range = datetime + c(0, 1)
    ),
    paste(
      "var >= as.POSIXct(\"2021-09-01 12:00:00\", tz = \"GMT\")",
      "var < as.POSIXct(\"2021-09-01 12:00:02\", tz = \"GMT\")",
      sep = " & "
    )
  )
})

# call_condition_range_date ----
testthat::test_that("call_condition_range_date works with date range only", {
  date <- as.Date("2021-09-01")
  testthat::expect_identical(
    call_condition_range_date(
      "date",
      range = date + c(0, 1)
    ),
    "date >= as.Date(\"2021-09-01\") & date <= as.Date(\"2021-09-02\")"
  )
  testthat::expect_error(
    call_condition_range_date(
      "date",
      range = date + c(1, 0)
    ),
    "must be sorted"
  )
  testthat::expect_error(
    call_condition_range_date(
      "date",
      range = date
    ),
    "Assertion.+failed"
  )
  testthat::expect_error(
    call_condition_range_date(
      "date",
      range = 1
    ),
    "Assertion.+failed"
  )
})


# calls_combine_by ----
testthat::test_that("calls_combine_by - different operators", {
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b)), operator = "&"),
    quote(a & b)
  )
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b)), operator = "||"),
    quote(a || b)
  )
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b()), quote(c())), operator = "%>%"),
    quote(a %>% b() %>% c())
  )
  testthat::expect_error(
    calls_combine_by(calls = list(quote(a), quote(b), quote(c)), operator = as.symbol("&"))
  )
  testthat::expect_error(
    calls_combine_by(calls = list(quote(a), quote(b), quote(c)), operator = c("&", "|"))
  )
  testthat::expect_identical(
    calls_combine_by(calls = list(quote(a), quote(b), quote(c)), operator = "whatever"),
    quote(whatever(whatever(a, b), c))
  )
})

testthat::test_that("calls_combine_by - different forms of calls", {
  testthat::expect_null(calls_combine_by(calls = list(), operator = "&"))
  testthat::expect_identical(
    calls_combine_by(calls = list(as.name("a"), quote(b$b)), operator = "||"),
    quote(a || b$b)
  )
  testthat::expect_error(
    calls_combine_by(calls = list("a", quote(a)), operator = "%>%"),
    "Assertion.+ failed"
  )
})
