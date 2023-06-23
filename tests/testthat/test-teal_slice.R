# filter_var ----
testthat::test_that("filter_var checks arguments", {
  testthat::expect_no_error(filter_var(dataname = "data", varname = "var"))
  testthat::expect_no_error(filter_var(dataname = "data", varname = "var", extra = "extra"))
  testthat::expect_no_error(
    filter_var(
      dataname = "data",
      varname = "var",
      choices = NULL,
      selected = NULL,
      keep_na = NULL,
      keep_inf = NULL,
      fixed = FALSE,
      locked = FALSE,
      id = "filter",
      extra = "extra"
    )
  )

  testthat::expect_error(filter_var(dataname = "data"), "argument \"varname\" is missing, with no default")

  testthat::expect_error(filter_var(varname = "var"), "argument \"dataname\" is missing, with no default")

  testthat::expect_error(
    filter_var(dataname = NULL, varname = "var"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    filter_var(dataname = 1, varname = "var"),
    "Assertion on 'dataname' failed"
  )
  testthat::expect_error(
    filter_var(dataname = c("data1", "data2"), varname = "variable"),
    "Assertion on 'dataname' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = NULL),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = 1),
    "Assertion on 'varname' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = c("var1", "var2")),
    "Assertion on 'varname' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", choices = list()),
    "Assertion on 'choices' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", selected = list()),
    "Assertion on 'selected' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", keep_na = "TRUE"),
    "Assertion on 'keep_na' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", keep_inf = "TRUE"),
    "Assertion on 'keep_inf' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", fixed = NULL),
    "Assertion on 'fixed' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", fixed = "TRUE"),
    "Assertion on 'fixed' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", locked = NULL),
    "Assertion on 'locked' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", locked = "TRUE"),
    "Assertion on 'locked' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", id = NULL),
    "Assertion on 'id' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", id = c("a", "b")),
    "Assertion on 'id' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", id = 1L),
    "Assertion on 'id' failed"
  )
})


testthat::test_that("filter_var returns `teal_slice`", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var("data", "var1")

  testthat::expect_s3_class(fs1, "teal_slice")
  testthat::expect_s3_class(fs1, "reactivevalues")
  testthat::expect_failure(
    testthat::expect_s3_class(fs1, "teal_slices")
  )
  testthat::expect_length(shiny::reactiveValuesToList(fs1), 10L)
})


# filter_settings ----
testthat::test_that("filter_settings checks arguments", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")

  testthat::expect_no_error(filter_settings(fs1, fs2))
  testthat::expect_no_error(filter_settings(fs1, fs2,
    exclude_varnames = list(data = "var1"),
    count_type = "all",
    module_add = FALSE
  ))

  testthat::expect_error(filter_settings(fs1, fs2, "fs1"), "Assertion on 'slices' failed")

  testthat::expect_error(filter_settings(fs1, fs2, exclude_varnames = "fs1"), "Assertion on 'exclude_varnames' failed")
  testthat::expect_error(filter_settings(fs1, fs2, include_varnames = "fs1"), "Assertion on 'include_varnames' failed")

  testthat::expect_error(filter_settings(fs1, fs2, count_type = "fs1"), "Must be a subset")

  testthat::expect_error(filter_settings(fs1, fs2, count_type = c("a", "b")), "Must have length 1")

  testthat::expect_error(filter_settings(fs1, fs2, count_type = c("all", "none")))

  testthat::expect_error(filter_settings(fs1, fs2, module_add = NULL), "Assertion on 'module_add' failed")

  testthat::expect_error(filter_settings(fs1, fs1, fs2), "Some teal_slice objects have the same id")
})

testthat::test_that("filter_settings returns `teal_slices`", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  testthat::expect_s3_class(fs, "teal_slices")
  testthat::expect_s3_class(fs, "list")
  testthat::expect_failure(
    testthat::expect_s3_class(fs, "teal_slice")
  )

  testthat::expect_null(attr(fs, "include"))
  testthat::expect_null(attr(fs, "exclude"))

  testthat::expect_null(attr(fs, "count_type"))
  testthat::expect_true(attr(fs, "module_add"))

  testthat::expect_length(filter_settings(fs1, fs2), 2L)
})


# is.teal_slice(s) ----
testthat::test_that("is* functions work", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  testthat::expect_no_error(is.teal_slice(fs1))

  testthat::expect_true(is.teal_slice(fs1))
  testthat::expect_false(is.teal_slice(fs))

  testthat::expect_no_error(is.teal_slice(fs))

  testthat::expect_true(is.teal_slices(fs))
  testthat::expect_false(is.teal_slices(fs1))
})

# [.teal_slices ----
testthat::test_that("[.teal_slices accepts various types of indices", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  names(fs) <- c("one", "two")

  testthat::expect_no_error(fs[])
  testthat::expect_no_error(fs[integer(0)])
  testthat::expect_no_error(fs[character(0)])
  testthat::expect_no_error(fs[logical(0)])
  testthat::expect_no_error(fs[1])
  testthat::expect_no_error(fs["one"])
  testthat::expect_no_error(fs[c(TRUE, FALSE)])

  testthat::expect_error(fs[c(TRUE, TRUE, FALSE)], "subscript out of bounds")
  testthat::expect_error(fs[3], "subscript out of bounds")
  testthat::expect_error(fs["three"], "subscript out of bounds")

  testthat::expect_error(fs[[integer(0)]], "attempt to select less than one element in get1index")
  testthat::expect_error(fs[[character(0)]], "attempt to select less than one element in get1index")
  testthat::expect_error(fs[[logical(0)]], "attempt to select less than one element in get1index")
  testthat::expect_no_error(fs[[1]])
  testthat::expect_no_error(fs[["one"]])
  testthat::expect_error(fs[[c(TRUE, FALSE)]], "attempt to select less than one element in integerOneIndex")
})


testthat::test_that("[.teal_slices subsets properly", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  names(fs) <- c("one", "two")

  fss1 <- filter_settings(fs1)
  names(fss1) <- "one"

  # return class
  testthat::expect_s3_class(fs[1], "teal_slices")
  testthat::expect_s3_class(fs[[1]], "teal_slice")

  # different indices return the same subset
  testthat::expect_identical(fs[1], fs["one"])
  testthat::expect_identical(fs[1], fs[c(TRUE, FALSE)])
  testthat::expect_identical(fs[[1]], fs[["one"]])

  # a subset of teal_slices is the same as teal_slices created from a subset of teal_slice's
  testthat::expect_identical(fs[1], fss1)
  testthat::expect_identical(fs[[1]], fss1[[1]])
  testthat::expect_identical(fs[["one"]], fss1[["one"]])

  # also for unnamed objects
  fs <- unname(fs)
  fss1 <- unname(fss1)
  testthat::expect_identical(fs[1], fss1)
  testthat::expect_identical(fs[[1]], fss1[[1]])
})


testthat::test_that("[.teal_slices also subsets the exclude_varnames attribute", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var("data1", "var1")
  fs2 <- filter_var("data1", "var2")
  fs3 <- filter_var("data2", "var1")
  fs4 <- filter_var("data2", "var2")
  fs <- filter_settings(fs1, fs2, fs3, fs4, exclude_varnames = list(data1 = "var1", data2 = "var1"))

  testthat::expect_identical(
    attr(fs[1], "exclude_varnames"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fs[2], "exclude_varnames"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fs[1:2], "exclude_varnames"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3], "exclude_varnames"),
    list(data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[4], "exclude_varnames"),
    list(data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3:4], "exclude_varnames"),
    list(data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
})


testthat::test_that("[.teal_slices preserves count_type", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  testthat::expect_identical(
    attr(fs, "count_type"),
    attr(fs[1], "count_type")
  )
})


testthat::test_that("c.teal_slices concatenates `teal_slices` objects", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var("data1", "var1")
  fs2 <- filter_var("data1", "var2")
  fs3 <- filter_var("data2", "var1")
  fs4 <- filter_var("data2", "var2")
  fss1 <- filter_settings(fs1, fs2)
  fss2 <- filter_settings(fs3, fs4)

  testthat::expect_no_error(c(fss1, fss2))
  testthat::expect_error(c(fss1, fs1), "Assertion on 'all arguments are teal_slices' failed")

  testthat::expect_s3_class(c(fss1, fss2), "teal_slices")
  testthat::expect_length(c(fss1, fss2), length(fss1) + length(fss2))
})


testthat::test_that("c.teal_slices handles attributes", {
  fs1 <- filter_var("data1", "var1")
  fs2 <- filter_var("data1", "var2")
  fs3 <- filter_var("data2", "var1")
  fs4 <- filter_var("data2", "var2")
  fss1 <- filter_settings(fs1, fs2, exclude_varnames = list(data1 = "var1"))
  fss2 <- filter_settings(fs3, fs4, exclude_varnames = list(data2 = "var1"))
  fss3 <- filter_settings(fs3, fs4, exclude_varnames = list(data2 = "var1"), count_type = "none")

  # teal_slices with different exclude attributes
  testthat::expect_no_error(c(fss1, fss2))

  fsss <- c(fss1, fss2)

  # exclude attributes are combined
  testthat::expect_identical(
    attr(fsss, "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )

  # count_type attribute is preserved
  testthat::expect_identical(
    attr(fsss, "count_type"),
    attr(fss1, "count_type")
  )
})


# format.teal_slice ----
testthat::test_that("format.teal_slice returns a character string", {
  fs <- filter_var(
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
  fs <- filter_var(
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
  fs <- filter_var(
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
  optional <- setdiff(names(shiny::reactiveValuesToList(fs)), names(formals(filter_var)))
  lapply(optional, function(x) {
    testthat::expect_true(any(grepl(x, ffs)))
  })
})

# format.teal_slices ----
testthat::test_that("format.teal_slices returns a character string", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  names(fs) <- c("one", "two")
  testthat::expect_true(checkmate::check_string(format(fs, show_all = TRUE)))
})

testthat::test_that("format.teal_slices contains literal formatted representations of all included `teal_slice`s", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  slices <- lapply(fs, format, show_all = TRUE)
  lapply(slices, function(x) {
    testthat::expect_true(any(grepl(gsub(" ", "", x), gsub(" ", "", ffs), fixed = TRUE)))
  })
})

testthat::test_that("format.teal_slices prints include_varnames attribute if not empty", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(!grepl("include_varnames", ffs))
  fs <- filter_settings(fs1, fs2, include_varnames = list(data = "var2"))
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(grepl("include_varnames", ffs))
})

testthat::test_that("format.teal_slices prints exclude_varnames attribute if not empty", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(!all(grepl("exclude_varnames", ffs)))
  fs <- filter_settings(fs1, fs2, exclude_varnames = list(data = "var2"))
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(grepl("exclude_varnames", ffs))
})

testthat::test_that("format.teal_slices prints count_type attribute if not empty", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(!grepl("count_type", ffs))
})


# helpers ----
testthat::test_that("slices_field works", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- filter_var(dataname = "data", varname = "var1")
  fs2 <- filter_var(dataname = "data", varname = "var2")
  fs <- filter_settings(fs1, fs2)

  # argument checks
  testthat::expect_no_error(slices_field(fs, "dataname"))
  testthat::expect_error(slices_field(fs), "argument \"field\" is missing")

  # vector is returned when non-empty fields are queried
  testthat::expect_vector(slices_field(fs, "dataname"))
  testthat::expect_vector(slices_field(fs, "varname"))

  # proper content is returned
  testthat::expect_identical(slices_field(fs, "dataname"), "data")
  testthat::expect_identical(slices_field(fs, "varname"), c("var1", "var2"))
  testthat::expect_identical(slices_field(fs, "choices"), NULL)
  testthat::expect_identical(slices_field(fs, "fixed"), FALSE)
  testthat::expect_identical(slices_field(fs, "locked"), FALSE)
})

testthat::test_that("filter_expr id has to be a string", {
  testthat::expect_no_error(filter_expr(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(filter_expr(dataname = "x", id = 1, title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(filter_expr(dataname = "x", id = NULL, title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(
    filter_expr(dataname = "x", id = character(0), title = "x", expr = "x == 'x'"), "length"
  )
})

testthat::test_that("filter_expr title has to be a string", {
  testthat::expect_no_error(filter_expr(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(filter_expr(dataname = "x", id = "x", title = 1, expr = "x == 'x'"), "string")
  testthat::expect_error(filter_expr(dataname = "x", id = "x", title = NULL, expr = "x == 'x'"), "string")
  testthat::expect_error(
    filter_expr(dataname = "x", id = "x", title = character(0), expr = "x == 'x'"), "length"
  )
})

testthat::test_that("filter_expr dataname has to be a string", {
  testthat::expect_no_error(filter_expr(dataname = "x", id = "x", title = "x", expr = "x == 'x'"))
  testthat::expect_error(filter_expr(dataname = 1, id = "x", title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(filter_expr(dataname = NULL, id = "x", title = "x", expr = "x == 'x'"), "string")
  testthat::expect_error(
    filter_expr(dataname = character(0), id = "x", title = "x", expr = "x == 'x'"), "length"
  )
})

testthat::test_that("filter_expr expr has to be a string", {
  testthat::expect_no_error(filter_expr(dataname = "x", id = "x", title = "x", expr = "x == FALSE"))
  testthat::expect_no_error(filter_expr(dataname = "x", id = "x", title = "x", expr = "x <- 1")) # Ouch!
  testthat::expect_error(
    filter_expr(dataname = "x", id = "x", title = "x", expr = TRUE),
    "Assertion on 'expr' failed"
  )
})
