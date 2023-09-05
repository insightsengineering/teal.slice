testthat::test_that("teal_slices checks arguments", {
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")

  testthat::expect_no_error(teal_slices(fs1, fs2))
  testthat::expect_no_error(teal_slices(fs1, fs2,
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

testthat::test_that("teal_slices raises error when include_varnames and exclude_varnames specified same dataset", {
  testthat::expect_error(
    teal_slices(
      include_varnames = list(data1 = "var1"),
      exclude_varnames = list(data1 = "var2")
    ),
    "Some datasets are specified in both, include_varnames and exclude_varnames"
  )
})


testthat::test_that("[.teal_slices accepts various types of indices", {
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
  fs1 <- teal_slice(dataname = "data1", varname = "var1")
  fs2 <- teal_slice(dataname = "data1", varname = "var2")
  fs3 <- teal_slice(dataname = "data2", varname = "var1")
  fs4 <- teal_slice(dataname = "data2", varname = "var2")
  fs <- teal_slices(fs1, fs2, fs3, fs4, exclude_varnames = list(data1 = "var1", data2 = "var1"))

  testthat::expect_identical(
    attr(fs[1], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[2], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[1:2], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[4], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3:4], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[], "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )
})

testthat::test_that("[.teal_slices doesn't subset the include_varnames attribute according to available teal_slice", {
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
  fs1 <- teal_slice("data", "var1")
  fs2 <- teal_slice("data", "var2")
  fs <- teal_slices(fs1, fs2)

  testthat::expect_identical(
    attr(fs, "count_type"),
    attr(fs[1], "count_type")
  )
})


testthat::test_that("coalesce_r accepts list of atomics or list of lists", {
  testthat::expect_no_error(coalesce_r(list("a", "b")))
  testthat::expect_no_error(coalesce_r(list(list(a = "A", b = "B"), list(c = "C", d = "D"))))
  testthat::expect_error(coalesce_r(c("a", "b")), "Assertion on.+failed")
  testthat::expect_error(coalesce_r(list("a", list("b"))), "Assertion on.+failed")
})

testthat::test_that("coalesce_r returns first non-null element of list of atomics", {
  testthat::expect_identical(coalesce_r(list("a", "b")), "a")
  testthat::expect_identical(coalesce_r(list("a", NULL)), "a")
  testthat::expect_identical(coalesce_r(list(NULL, "b")), "b")
  testthat::expect_identical(coalesce_r(list(2L, "b")), 2L)
})

testthat::test_that("coalesce_r combines non-overlapping lists", {
  testthat::expect_identical(
    coalesce_r(
      list(
        list(first = c("a", "b")),
        list(second = c("a", "b"))
      )
    ),
    list(first = c("a", "b"), second = c("a", "b"))
  )
})

testthat::test_that("coalesce_r combines drops duplicated list elements", {
  testthat::expect_identical(
    coalesce_r(
      list(
        list(first = c("a", "b"), second = c("a", "b")),
        list(first = c("A", "B"))
      )
    ),
    list(first = c("a", "b"), second = c("a", "b"))
  )
})

testthat::test_that("coalesce_r ignores NULL elements", {
  testthat::expect_identical(
    coalesce_r(
      list(
        list(first = NULL, second = c("a", "b")),
        list(first = c("A", "B"))
      )
    ),
    list(first = c("A", "B"), second = c("a", "b"))
  )
})


testthat::test_that("c.teal_slices concatenates `teal_slices` objects", {
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

testthat::test_that("c.teal_slices coalesces attributes", {
  fs1 <- teal_slice("data1", "var1")
  fs2 <- teal_slice("data1", "var2")
  fs3 <- teal_slice("data2", "var1")
  fs4 <- teal_slice("data2", "var2")

  fss1 <- teal_slices(fs1, fs2, include_varnames = list(data1 = "var1"))
  fss2 <- teal_slices(fs3, fs4, exclude_varnames = list(data2 = "var1"))
  # teal_slices with include and exclude attributes
  testthat::expect_no_error(fsss <- c(fss1, fss2))
  # separate attributes are collated
  testthat::expect_identical(
    attr(fsss, "include_varnames"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fsss, "exclude_varnames"),
    list(data2 = "var1")
  )

  fss3 <- teal_slices(fs1, fs2, exclude_varnames = list(data1 = "var1"))
  fss4 <- teal_slices(fs3, fs4, exclude_varnames = list(data2 = "var1"))
  # teal_slices with different exclude attributes
  testthat::expect_no_error(fsss <- c(fss3, fss4))
  # list attribute is combined
  testthat::expect_identical(
    attr(fsss, "exclude_varnames"),
    list(data1 = "var1", data2 = "var1")
  )

  fss5 <- teal_slices(fs1, fs2, exclude_varnames = list(data1 = "var1"))
  fss6 <- teal_slices(fs3, fs4, exclude_varnames = list(data1 = "var2"))
  # teal_slices with conflicting exclude attributes
  testthat::expect_no_error(fsss <- c(fss5, fss6))
  # list attribute is coalesced
  testthat::expect_identical(
    attr(fsss, "exclude_varnames"),
    list(data1 = "var1")
  )

  fss7 <- teal_slices(fs1, fs2, count_type = NULL, allow_add = TRUE)
  fss8 <- teal_slices(fs3, fs4, count_type = "none", allow_add = FALSE)
  # teal_slices with conflicting count_type and allow_add attributes
  testthat::expect_no_error(fsss <- c(fss7, fss8))
  # atomic attributes are coalesced
  testthat::expect_identical(
    attributes(fsss)[c("count_type", "allow_add")],
    list(count_type = "none", allow_add = TRUE)
  )

  # teal_slices with conflicting exclude attributes - reversed order
  testthat::expect_no_error(fsss <- c(fss8, fss7))
  # list attribute is coalesced
  testthat::expect_identical(
    attributes(fsss)[c("count_type", "allow_add")],
    list(count_type = "none", allow_add = FALSE)
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
  fs <- teal_slices(fs1, fs2, exclude_varnames = list(data = "var2"))
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
