testthat::test_that("teal_slices checks arguments", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")

  testthat::expect_no_error(teal_slices(fs1, fs2))
  testthat::expect_error(teal_slices(fs1, fs2,
    exclude_varnames = list(data = "var1"),
    count_type = "all",
    allow_add = FALSE
  ))

  testthat::expect_error(teal_slices(fs1, fs2, "fs1"), "Assertion on 'slices' failed")

  testthat::expect_error(teal_slices(fs1, fs2, exclude_varnames = "fs1"), "Assertion on 'exclude_varnames' failed")
  testthat::expect_error(teal_slices(fs1, fs2, include_varnames = "fs1"), "Assertion on 'include_varnames' failed")

  testthat::expect_error(teal_slices(fs1, fs2, count_type = "fs1"), "Must be a subset")

  testthat::expect_error(teal_slices(fs1, fs2, count_type = c("a", "b")), "Must have length 1")

  testthat::expect_error(teal_slices(fs1, fs2, count_type = c("all", "none")))

  testthat::expect_error(teal_slices(fs1, fs2, allow_add = NULL), "Assertion on 'allow_add' failed")

  testthat::expect_error(teal_slices(fs1, fs1, fs2), "Some teal_slice objects have the same id")
})

testthat::test_that("teal_slices returns `teal_slices`", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)

  testthat::expect_s3_class(fs, "teal_slices")
  testthat::expect_s3_class(fs, "list")
  testthat::expect_failure(
    testthat::expect_s3_class(fs, "teal_slice")
  )

  testthat::expect_null(attr(fs, "include"))
  testthat::expect_null(attr(fs, "exclude"))

  testthat::expect_null(attr(fs, "count_type"))
  testthat::expect_true(attr(fs, "allow_add"))

  testthat::expect_length(teal_slices(fs1, fs2), 2L)
})

testthat::test_that("[.teal_slices accepts various types of indices", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
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

  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
  names(fs) <- c("one", "two")

  fss1 <- teal_slices(fs1)
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


testthat::test_that("[.teal_slices doesn't subset the exclude_varnames attribute according to available teal_slice", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- teal_slice(dataname = "data1", varname = "var1")
  fs2 <- teal_slice(dataname = "data1", varname = "var2")
  fs3 <- teal_slice(dataname = "data2", varname = "var1")
  fs4 <- teal_slice(dataname = "data2", varname = "var2")
  fs <- teal_slices(fs1, fs2, fs3, fs4, exclude_varnames = list(data1 = "var3", data2 = "var3"))

  testthat::expect_identical(
    attr(fs[1], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
  testthat::expect_identical(
    attr(fs[2], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
  testthat::expect_identical(
    attr(fs[1:2], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
  testthat::expect_identical(
    attr(fs[3], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
  testthat::expect_identical(
    attr(fs[4], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
  testthat::expect_identical(
    attr(fs[3:4], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
  testthat::expect_identical(
    attr(fs[], "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )
})

testthat::test_that("[.teal_slices doesn't subset the include_varnames attribute according to available teal_slice", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- teal_slice(dataname = "data1", varname = "var1")
  fs2 <- teal_slice(dataname = "data1", varname = "var2")
  fs3 <- teal_slice(dataname = "data2", varname = "var1")
  fs4 <- teal_slice(dataname = "data2", varname = "var2")
  fs <- teal_slices(fs1, fs2, fs3, fs4, include_varnames = list(data1 = "var1", data2 = "var1"))

  testthat::expect_identical(
    attr(fs[1], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[2], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[1:2], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[4], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3:4], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[], "include_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
})

testthat::test_that("[.teal_slices preserves count_type", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)

  testthat::expect_identical(
    attr(fs, "count_type"),
    attr(fs[1], "count_type")
  )
})


testthat::test_that("c.teal_slices concatenates `teal_slices` objects", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  fs1 <- teal_slice("data1", "var1")
  fs2 <- teal_slice("data1", "var2")
  fs3 <- teal_slice("data2", "var1")
  fs4 <- teal_slice("data2", "var2")
  fss1 <- teal_slices(fs1, fs2)
  fss2 <- teal_slices(fs3, fs4)

  testthat::expect_no_error(c(fss1, fss2))
  testthat::expect_error(c(fss1, fs1), "Assertion on 'all arguments are teal_slices' failed")

  testthat::expect_s3_class(c(fss1, fss2), "teal_slices")
  testthat::expect_length(c(fss1, fss2), length(fss1) + length(fss2))
})


testthat::test_that("c.teal_slices handles attributes", {
  fs1 <- teal_slice("data1", "var1")
  fs2 <- teal_slice("data1", "var2")
  fs3 <- teal_slice("data2", "var1")
  fs4 <- teal_slice("data2", "var2")
  fss1 <- teal_slices(fs1, fs2, exclude_varnames = list(data1 = "var3"))
  fss2 <- teal_slices(fs3, fs4, exclude_varnames = list(data2 = "var3"))
  fss3 <- teal_slices(fs3, fs4, exclude_varnames = list(data2 = "var4"), count_type = "none")

  # teal_slices with different exclude attributes
  testthat::expect_no_error(c(fss1, fss2))

  fsss <- c(fss1, fss2)

  # exclude attributes are combined
  testthat::expect_identical(
    attr(fsss, "exclude_varnames"),
    list(data1 = "var3", data2 = "var3")
  )

  # count_type attribute is preserved
  testthat::expect_identical(
    attr(fsss, "count_type"),
    attr(fss1, "count_type")
  )
})

testthat::test_that("format.teal_slices returns a character string", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
  names(fs) <- c("one", "two")
  testthat::expect_true(checkmate::check_string(format(fs, show_all = TRUE)))
})

testthat::test_that("format.teal_slices contains literal formatted representations of all included `teal_slice`s", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  slices <- lapply(fs, format, show_all = TRUE)
  lapply(slices, function(x) {
    testthat::expect_true(any(grepl(gsub(" ", "", x), gsub(" ", "", ffs), fixed = TRUE)))
  })
})

testthat::test_that("format.teal_slices prints include_varnames attribute if not empty", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(!grepl("include_varnames", ffs))
  fs <- teal_slices(fs1, fs2, include_varnames = list(data = "var2"))
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(grepl("include_varnames", ffs))
})

testthat::test_that("format.teal_slices prints exclude_varnames attribute if not empty", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(!all(grepl("exclude_varnames", ffs)))
  fs <- teal_slices(fs1, fs2, exclude_varnames = list(data = "var3"))
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(grepl("exclude_varnames", ffs))
})

testthat::test_that("format.teal_slices prints count_type attribute if not empty", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)
  ffs <- format(fs, show_all = TRUE)
  testthat::expect_true(!grepl("count_type", ffs))
})
