testthat::test_that("call_condition_choice varname as character", {
  testthat::expect_identical(
    call_check_parse_varname("var"),
    quote(var)
  )
  testthat::expect_identical(
    call_check_parse_varname("x$var"),
    quote(x$var)
  )
  testthat::expect_identical(
    call_check_parse_varname("x$var;"),
    quote(x$var)
  )
  testthat::expect_identical(
    call_check_parse_varname('trunc(x$var, units = "secs")'),
    quote(trunc(x$var, units = "secs"))
  )
  testthat::expect_identical(
    call_check_parse_varname('c(trunc(x$var, units = "secs"))'),
    quote(c(trunc(x$var, units = "secs")))
  )
  testthat::expect_error(
    call_check_parse_varname("x$var; x"),
    "Problem with parsing"
  )
})

testthat::test_that("call_condition_choice varname as name or call", {
  testthat::expect_identical(
    call_check_parse_varname(quote(var)),
    quote(var)
  )
  testthat::expect_identical(
    call_check_parse_varname(quote(x$var)),
    quote(x$var)
  )
  testthat::expect_identical(
    call_check_parse_varname(quote(trunc(x$var, units = "secs"))),
    quote(trunc(x$var, units = "secs"))
  )
  testthat::expect_identical(
    call_check_parse_varname(quote(c(trunc(x$var, units = "secs")))),
    quote(c(trunc(x$var, units = "secs")))
  )
  testthat::expect_error(
    call_check_parse_varname(parse(text = "x$var; x")),
    "Assertion failed.+varname"
  )
})

testthat::test_that("call_condition_choice varname inputs - character", {
  testthat::expect_identical(
    call_condition_choice("var", choices = character(0)),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = "F"),
    quote(var == "F")
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c("A", "B")),
    quote(var %in% c("A", "B"))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c("A", "B", NA_character_)),
    quote(var %in% c("A", "B", NA_character_))
  )
})

testthat::test_that("call_codition_choice accept all type of choices - integer", {
  testthat::expect_identical(
    call_condition_choice("var", choices = integer(0)),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = 1L),
    quote(var == 1L)
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1L, 2L)),
    quote(var %in% c(1L, 2L))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1L, 2L, NA_integer_)),
    quote(var %in% c(1L, 2L, NA_integer_))
  )
})

testthat::test_that("call_codition_choice accept all type of choices - numeric", {
  testthat::expect_identical(
    call_condition_choice("var", choices = numeric(0)),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = 1.1),
    quote(var == 1.1)
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1.1, 2.1)),
    quote(var %in% c(1.1, 2.1))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1.1, 2.1, NA_real_, Inf)),
    quote(var %in% c(1.1, 2.1, NA_real_, Inf))
  )
})

testthat::test_that("call_condition_choice varname inputs - character", {
  testthat::expect_identical(
    call_condition_choice("var", choices = character(0)),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = "F"),
    quote(var == "F")
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c("A", "B")),
    quote(var %in% c("A", "B"))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c("A", "B", NA_character_)),
    quote(var %in% c("A", "B", NA_character_))
  )
})

testthat::test_that("call_condition_choice varname inputs - factor", {
  testthat::expect_identical(
    call_condition_choice("var", choices = as.factor(character(0))),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = factor("F")),
    quote(var == "F")
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = factor(c("A", "B"))),
    quote(var %in% c("A", "B"))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = factor(c("A", "B", NA_character_))),
    quote(var %in% c("A", "B", NA_character_))
  )
})

testthat::test_that("call_codition_choice accept all type of choices - Date", {
  date <- as.Date("2021-09-01")
  testthat::expect_identical(
    call_condition_choice("var", choices = as.Date(integer(0), origin = "1900-01-01")),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + 1L),
    quote(var == "2021-09-02")
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L)),
    quote(var %in% c("2021-09-02", "2021-09-03"))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L, NA_integer_)),
    quote(var %in% c("2021-09-02", "2021-09-03", NA_character_))
  )
})

testthat::test_that("call_codition_choice accept all type of choices - datetime", {
  date <- as.POSIXct("2021-09-01 12:00:00", tz = "UTC")
  testthat::expect_identical(
    call_condition_choice("var", choices = as.POSIXct(integer(0), origin = "1900-01-01")),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + 1L),
    quote(var == "2021-09-01 12:00:01")
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L)),
    quote(var %in% c("2021-09-01 12:00:01", "2021-09-01 12:00:02"))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = date + c(1L, 2L, NA_integer_)),
    quote(var %in% c("2021-09-01 12:00:01", "2021-09-01 12:00:02", NA_character_))
  )
})

testthat::test_that("call_codition_choice accept all type of choices - integer", {
  testthat::expect_identical(
    call_condition_choice("var", choices = integer(0)),
    quote(var %in% c())
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = 1L),
    quote(var == 1L)
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1L, 2L)),
    quote(var %in% c(1L, 2L))
  )
  testthat::expect_identical(
    call_condition_choice("var", choices = c(1L, 2L, NA_integer_)),
    quote(var %in% c(1L, 2L, NA_integer_))
  )
})

testthat::test_that("call_condition_range works only with numeric(2)", {
  testthat::expect_identical(
    call_condition_range("var", range = c(1, 2)),
    quote(var >= 1 & var <= 2)
  )

  testthat::expect_equal(
    call_condition_range("var", range = c(-1.2, 2.1)),
    quote(var >= -1.2 & var <= 2.1)
  )

  testthat::expect_error(
    call_condition_range("var", range = c(2.1, -1.2)),
    "Assertion on 'range' failed.+sorted"
  )

  testthat::expect_error(
    call_condition_range("var", range = c("a", "b")),
    "Assertion on 'range' failed.+type"
  )

  testthat::expect_error(
    call_condition_range("var", range = 1),
    "Assertion on 'range' failed.+length"
  )
})

testthat::test_that("call_condition_logical works only with logical(1)", {
  testthat::expect_identical(
    call_condition_logical("var", choice = TRUE),
    quote(var)
  )
  testthat::expect_identical(
    call_condition_logical("var", choice = FALSE),
    quote(!var)
  )
  testthat::expect_identical(
    call_condition_logical("var == 2", choice = FALSE),
    quote(!var == 2)
  )

  testthat::expect_error(
    call_condition_logical("var", choice = c(TRUE, FALSE)),
    "Assertion on 'choice'"
  )

  testthat::expect_error(
    call_condition_logical("var", choice = 1),
    "Assertion on 'choice'"
  )
  testthat::expect_error(
    call_condition_logical("var", choice = "TRUE"),
    "Assertion on 'choice'"
  )
})

testthat::test_that("call_condition_posixct works with POXIXct range only", {
  datetime <- as.POSIXct("2021-09-01 12:00:00", tz = "UTC")
  testthat::expect_identical(
    call_condition_range_posixct(
      varname = as.name("var"),
      range = datetime + c(0, 1),
      timezone = "UTC"
    ),
    quote(
      var >= as.POSIXct("2021-09-01 12:00:00", tz = "UTC") &
        var < as.POSIXct("2021-09-01 12:00:02", tz = "UTC")
    )
  )


  testthat::expect_error(
    call_condition_range_posixct(
      varname = as.name("var"),
      range = datetime + c(1, 0),
      timezone = "UTC"
    )
  )
  testthat::expect_error(
    call_condition_range_posixct(
      varname = as.name("var"),
      range = Sys.Date() + c(0, 1),
      timezone = "UTC"
    )
  )
  testthat::expect_error(
    call_condition_range_posixct(
      varname = as.name("var"),
      range = Sys.time(),
      timezone = "UTC"
    )
  )
})

testthat::test_that("call_condition_posixct returns expected timezone", {
  datetime <- as.POSIXct("2021-09-01 12:00:00", tz = "Europe/Stockholm")
  testthat::expect_identical(
    call_condition_range_posixct(
      varname = as.name("var"),
      range = datetime + c(0, 1),
      timezone = "Europe/Stockholm"
    ),
    quote(
      var >= as.POSIXct("2021-09-01 12:00:00", tz = "Europe/Stockholm") &
        var < as.POSIXct("2021-09-01 12:00:02", tz = "Europe/Stockholm")
    )
  )

  datetime <- as.POSIXct("2021-09-01 12:00:00")
  testthat::expect_identical(
    call_condition_range_posixct(
      varname = as.name("var"),
      range = datetime + c(0, 1)
    ),
    bquote(
      var >= as.POSIXct("2021-09-01 12:00:00", tz = .(Sys.timezone())) &
        var < as.POSIXct("2021-09-01 12:00:02", tz = .(Sys.timezone()))
    )
  )
})

testthat::test_that("call_condition_date works with date range only", {
  date <- as.Date("2021-09-01")
  testthat::expect_identical(
    call_condition_range_date(
      as.name("date"),
      range = date + c(0, 1)
    ),
    quote(date >= as.Date("2021-09-01") & date <= as.Date("2021-09-02"))
  )
  testthat::expect_error(
    call_condition_range_date(
      as.name("date"),
      range = date + c(1, 0)
    )
  )
  testthat::expect_error(
    call_condition_range_date(
      as.name("date"),
      range = date
    )
  )
  testthat::expect_error(
    call_condition_range_date(
      as.name("date"),
      range = 1
    )
  )
})

testthat::test_that("call_extract_array - dataname type", {
  testthat::expect_identical(
    call_extract_array(),
    quote(.[, , ])
  )
  testthat::expect_identical(
    call_extract_array(dataname = "data"),
    quote(data[, , ])
  )
  testthat::expect_identical(
    call_extract_array(dataname = as.name("data")),
    quote(data[, , ])
  )
  testthat::expect_identical(
    call_extract_array(dataname = quote(data$element$element@slot)),
    quote(data$element$element@slot[, , ])
  )
  testthat::expect_error(
    call_extract_array(dataname = as.expression(quote(data$element))),
    "Assertion failed.+dataname"
  )
  testthat::expect_error(
    call_extract_array(dataname = c("a", "b")),
    "Assertion failed.+dataname"
  )
})

testthat::test_that("call_extract_array - row type", {
  testthat::expect_identical(
    call_extract_array(row = "var"),
    quote(.["var", , ])
  )
  testthat::expect_identical(
    call_extract_array(row = as.name("var")),
    quote(.[var, , ])
  )
  testthat::expect_identical(
    call_extract_array(row = quote(data$var)),
    quote(.[data$var, , ])
  )
  testthat::expect_identical(
    call_extract_array(row = quote(data$var == data$var2)),
    quote(.[data$var == data$var2, , ])
  )
  testthat::expect_identical(
    call_extract_array(row = c(TRUE, FALSE, TRUE)),
    quote(.[c(TRUE, FALSE, TRUE), , ])
  )
  testthat::expect_identical(
    call_extract_array(row = c(1L, 3L, 5L)),
    quote(.[c(1L, 3L, 5L), , ])
  )
  testthat::expect_identical(
    call_extract_array(row = c("a", "b", "c")),
    quote(.[c("a", "b", "c"), , ])
  )
  testthat::expect_error(
    call_extract_array(row = as.factor("a"))
  )
})

testthat::test_that("call_extract_array - col type", {
  testthat::expect_identical(
    call_extract_array(col = "var"),
    quote(.[, "var", ])
  )
  testthat::expect_identical(
    call_extract_array(col = as.name("var")),
    quote(.[, var, ])
  )
  testthat::expect_identical(
    call_extract_array(col = quote(data$var)),
    quote(.[, data$var, ])
  )
  testthat::expect_identical(
    call_extract_array(col = quote(data$var == data$var2)),
    quote(.[, data$var == data$var2, ])
  )
  testthat::expect_identical(
    call_extract_array(col = c(TRUE, FALSE, TRUE)),
    quote(.[, c(TRUE, FALSE, TRUE), ])
  )
  testthat::expect_identical(
    call_extract_array(col = c(1L, 3L, 5L)),
    quote(.[, c(1L, 3L, 5L), ])
  )
  testthat::expect_identical(
    call_extract_array(col = c("a", "b", "c")),
    quote(.[, c("a", "b", "c"), ])
  )
  testthat::expect_error(
    call_extract_array(col = as.factor("a"))
  )
})

testthat::test_that("call_extract_array - aisle type", {
  testthat::expect_identical(
    call_extract_array(aisle = "var"),
    quote(.[, , "var"])
  )
  testthat::expect_identical(
    call_extract_array(aisle = as.name("var")),
    quote(.[, , var])
  )
  testthat::expect_identical(
    call_extract_array(aisle = quote(data$var)),
    quote(.[, , data$var])
  )
  testthat::expect_identical(
    call_extract_array(aisle = quote(data$var == data$var2)),
    quote(.[, , data$var == data$var2])
  )
  testthat::expect_identical(
    call_extract_array(aisle = c(TRUE, FALSE, TRUE)),
    quote(.[, , c(TRUE, FALSE, TRUE)])
  )
  testthat::expect_identical(
    call_extract_array(aisle = c(1L, 3L, 5L)),
    quote(.[, , c(1L, 3L, 5L)])
  )
  testthat::expect_identical(
    call_extract_array(aisle = c("a", "b", "c")),
    quote(.[, , c("a", "b", "c")])
  )
  testthat::expect_error(
    call_extract_array(aisle = as.factor("a"))
  )
})

testthat::test_that("call_extract_matrix - dataname type", {
  testthat::expect_identical(
    call_extract_matrix(),
    quote(.[, ])
  )
  testthat::expect_identical(
    call_extract_matrix(dataname = "data"),
    quote(data[, ])
  )
  testthat::expect_identical(
    call_extract_matrix(dataname = as.name("data")),
    quote(data[, ])
  )
  testthat::expect_identical(
    call_extract_matrix(dataname = quote(data$element$element@slot)),
    quote(data$element$element@slot[, ])
  )
  testthat::expect_error(
    call_extract_matrix(dataname = as.expression(quote(data$element))),
    "Assertion failed.+dataname"
  )
  testthat::expect_error(
    call_extract_matrix(dataname = c("a", "b")),
    "Assertion failed.+dataname"
  )
})

testthat::test_that("call_extract_matrix - row type", {
  testthat::expect_identical(
    call_extract_matrix(row = "var"),
    quote(.["var", ])
  )
  testthat::expect_identical(
    call_extract_matrix(row = as.name("var")),
    quote(.[var, ])
  )
  testthat::expect_identical(
    call_extract_matrix(row = quote(data$var)),
    quote(.[data$var, ])
  )
  testthat::expect_identical(
    call_extract_matrix(row = quote(data$var == data$var2)),
    quote(.[data$var == data$var2, ])
  )
  testthat::expect_identical(
    call_extract_matrix(row = c(TRUE, FALSE, TRUE)),
    quote(.[c(TRUE, FALSE, TRUE), ])
  )
  testthat::expect_identical(
    call_extract_matrix(row = c(1L, 3L, 5L)),
    quote(.[c(1L, 3L, 5L), ])
  )
  testthat::expect_identical(
    call_extract_matrix(row = c("a", "b", "c")),
    quote(.[c("a", "b", "c"), ])
  )
  testthat::expect_error(
    call_extract_matrix(row = as.factor("a"))
  )
})

testthat::test_that("call_extract_matrix - col type", {
  testthat::expect_identical(
    call_extract_matrix(col = "var"),
    quote(.[, "var"])
  )
  testthat::expect_identical(
    call_extract_matrix(col = as.name("var")),
    quote(.[, var])
  )
  testthat::expect_identical(
    call_extract_matrix(col = quote(data$var)),
    quote(.[, data$var])
  )
  testthat::expect_identical(
    call_extract_matrix(col = quote(data$var == data$var2)),
    quote(.[, data$var == data$var2])
  )
  testthat::expect_identical(
    call_extract_matrix(col = c(TRUE, FALSE, TRUE)),
    quote(.[, c(TRUE, FALSE, TRUE)])
  )
  testthat::expect_identical(
    call_extract_matrix(col = c(1L, 3L, 5L)),
    quote(.[, c(1L, 3L, 5L)])
  )
  testthat::expect_identical(
    call_extract_matrix(col = c("a", "b", "c")),
    quote(.[, c("a", "b", "c")])
  )
  testthat::expect_error(
    call_extract_matrix(col = as.factor("a"))
  )
})

testthat::test_that("call_extract_list - dataname argument", {
  testthat::expect_identical(
    call_extract_list("ADSL", "SEX"),
    quote(ADSL$"SEX")
  )
  testthat::expect_identical(
    call_extract_list(as.name("ADSL"), "SEX"),
    quote(ADSL$"SEX")
  )
  testthat::expect_identical(
    call_extract_list(quote(ADSL$SEX[[1]]), "SEX"),
    quote(ADSL$SEX[[1]]$"SEX")
  )
  testthat::expect_error(
    call_extract_list(NULL, "SEX")
  )
})

testthat::test_that("call_extract_list - varname argument", {
  testthat::expect_identical(
    call_extract_list("data", as.name("SEX")),
    quote(data$SEX)
  )
  testthat::expect_identical(
    call_extract_list("data", quote(data$var), dollar = FALSE),
    quote(data[[data$var]])
  )
  testthat::expect_error(
    call_extract_list("data", quote(data$var)),
    "Assertion on 'dollar'"
  )
})

testthat::test_that("calls_combine_by - operator", {
  testthat::expect_identical(
    calls_combine_by(operator = "&", calls = list(quote(a), quote(b))),
    quote(a & b)
  )
  testthat::expect_identical(
    calls_combine_by(operator = "||", calls = list(quote(a), quote(b))),
    quote(a || b)
  )
  testthat::expect_identical(
    calls_combine_by(operator = "%>%", calls = list(quote(a), quote(b()), quote(c()))),
    quote(a %>% b() %>% c())
  )
  testthat::expect_error(
    calls_combine_by(operator = as.symbol("&"), calls = list(quote(a), quote(b), quote(c)))
  )
  testthat::expect_error(
    calls_combine_by(operator = c("&", "|"), calls = list(quote(a), quote(b), quote(c)))
  )
  testthat::expect_identical(
    calls_combine_by(operator = "whatever", calls = list(quote(a), quote(b), quote(c))),
    quote(whatever(whatever(a, b), c))
  )
})

testthat::test_that("calls_combine_by - calls", {
  testthat::expect_identical(
    calls_combine_by(operator = "&", calls = as.expression(list(quote(a), quote(b)))),
    quote(a & b)
  )
  testthat::expect_identical(
    calls_combine_by(operator = "||", calls = list(as.name("a"), quote(b$b))),
    quote(a || b$b)
  )
  testthat::expect_error(
    calls_combine_by(operator = "%>%", calls = list("a", quote(a)))
  )
})

testthat::test_that("call_with_colon works", {
  a_call_without_colon <- call("base::sum", 1:10)
  a_call_with_colon <- call_with_colon("base::sum", 1:10)

  # evaluate these calls
  testthat::expect_error(eval(a_call_without_colon))
  testthat::expect_equal(eval(a_call_with_colon), 55)
})

testthat::test_that("call_extract_matrix does not throw when passed a long >500 chars call", {
  long_char <- paste("test %in% c(", paste(rep("test_val", 100), collapse = ", "), ")")
  test_call <- str2lang(long_char)
  testthat::expect_no_error(call_extract_matrix("iris", row = test_call))
})
