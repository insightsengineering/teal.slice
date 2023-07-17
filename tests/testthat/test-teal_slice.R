# teal_slice ----
testthat::test_that("teal_slice checks arguments", {
  testthat::expect_no_error(teal_slice(dataname = "data", varname = "var"))
  testthat::expect_no_error(teal_slice(dataname = "data", varname = "var", extra = "extra"))
  testthat::expect_no_error(
    teal_slice(
      dataname = "data",
      varname = "var",
      choices = NULL,
      selected = NULL,
      keep_na = NULL,
      keep_inf = NULL,
      fixed = FALSE,
      anchored = FALSE,
      id = "filter",
      extra = "extra"
    )
  )

  testthat::expect_error(teal_slice(dataname = "data"), "Must provide either `expr` or `varname`")

  testthat::expect_error(teal_slice(varname = "var"), "argument \"dataname\" is missing, with no default")

  testthat::expect_error(
    teal_slice(dataname = NULL, varname = "var"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = 1, varname = "var"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = c("data1", "data2"), varname = "variable"),
    "Assertion on 'dataname' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = NULL),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = "data", varname = 1),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = "data", varname = c("var1", "var2")),
    "Assertion on 'varname' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", choices = list()),
    "Assertion on 'choices' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", selected = list()),
    "Assertion on 'selected' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", keep_na = "TRUE"),
    "Assertion on 'keep_na' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", keep_inf = "TRUE"),
    "Assertion on 'keep_inf' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", fixed = NULL),
    "Assertion on 'fixed' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", fixed = "TRUE"),
    "Assertion on 'fixed' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", anchored = NULL),
    "Assertion on 'anchored' failed"
  )
  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", anchored = "TRUE"),
    "Assertion on 'anchored' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", id = NULL),
    "Assertion on 'id' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", id = c("a", "b")),
    "Assertion on 'id' failed"
  )

  testthat::expect_error(
    teal_slice(dataname = "data", varname = "var", id = 1L),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("teal_slice returns `teal_slice`", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- teal_slice(dataname = "data", varname = "var1")

  testthat::expect_s3_class(fs1, "teal_slice")
  testthat::expect_s3_class(fs1, "reactivevalues")
  testthat::expect_failure(
    testthat::expect_s3_class(fs1, "teal_slices")
  )
  testthat::expect_length(shiny::reactiveValuesToList(fs1), 10L)
})

testthat::test_that("is* functions work", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)

  testthat::expect_no_error(is.teal_slice(fs1))

  testthat::expect_true(is.teal_slice(fs1))
  testthat::expect_false(is.teal_slice(fs))

  testthat::expect_no_error(is.teal_slice(fs))

  testthat::expect_true(is.teal_slices(fs))
  testthat::expect_false(is.teal_slices(fs1))
})

testthat::test_that("format.teal_slice returns a character string", {
  fs <- teal_slice(
    dataname = "dataname2",
    varname = "varname3",
    choices = 1:10 / 10,
    selected = 0.2,
    multiple = TRUE,
    keep_na = TRUE,
    extra1 = "extraone",
    extra2 = "extratwo"
  )
  testthat::expect_true(checkmate::check_string(format(fs)))
  testthat::expect_true(checkmate::check_string(format(fs, show_all = TRUE)))
})

testthat::test_that("format.teal_slice skips empty mandatory fields show_all is FALSE", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  fs <- teal_slice(
    dataname = "dataname2",
    varname = "varname3",
    choices = 1:10 / 10,
    selected = 0.2,
    keep_na = TRUE,
    extra1 = "extraone",
    extra2 = "extratwo"
  )
  ffs <- strsplit(format(fs, show_all = FALSE), "\n")[[1]]
  empty <- names(Filter(is.null, shiny::reactiveValuesToList(fs)))
  lapply(empty, function(x) {
    testthat::expect_false(any(grepl(sprintf(" \\$ %s.*:", x), ffs)))
  })
})

testthat::test_that("format.teal_slice prints optional fields", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  fs <- teal_slice(
    dataname = "dataname2",
    varname = "varname3",
    chices = 1:10 / 10,
    selected = 0.2,
    multiple = TRUE,
    keep_na = FALSE,
    extra1 = "extraone",
    extra2 = "extratwo"
  )
  ffs <- strsplit(format(fs), "\n")[[1]]
  optional <- setdiff(names(shiny::reactiveValuesToList(fs)), names(formals(teal_slice)))
  lapply(optional, function(x) {
    testthat::expect_true(any(grepl(x, ffs)))
  })
})

testthat::test_that("teal_slice - expr has to be a string", {
  testthat::expect_no_error(teal_slice(dataname = "x", id = "x", title = "x", expr = "x == FALSE"))
  testthat::expect_no_error(teal_slice(dataname = "x", id = "x", title = "x", expr = "x <- 1")) # Ouch!
  testthat::expect_error(
    teal_slice(dataname = "x", id = "x", title = "x", expr = TRUE),
    "Assertion on 'expr' failed"
  )
})

testthat::test_that("teal_slice id has to be a string when expr is specified", {
  testthat::expect_no_error(teal_slice(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(teal_slice(dataname = "x", id = 1, title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(teal_slice(dataname = "x", id = NULL, title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(
    teal_slice(dataname = "x", id = character(0), title = "x", expr = "x == 'x'"), "length"
  )
})

testthat::test_that("teal_slice title has to be a string when expr is specified", {
  testthat::expect_no_error(teal_slice(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(teal_slice(dataname = "x", id = "x", title = 1, expr = "x == 'x'"), "string")
  testthat::expect_error(teal_slice(dataname = "x", id = "x", title = NULL, expr = "x == 'x'"), "string")
  testthat::expect_error(
    teal_slice(dataname = "x", id = "x", title = character(0), expr = "x == 'x'"), "length"
  )
})

testthat::test_that("teal_slice dataname has to be a string when expr is specified", {
  testthat::expect_no_error(teal_slice(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(teal_slice(dataname = 1, id = "x", title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(teal_slice(dataname = NULL, id = "x", title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(
    teal_slice(dataname = character(0), id = "x", title = "x", expr = "x == 'x'"), "length"
  )
})
