
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
      disabled = FALSE)
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
    "Assertion on 'keep_na' failed")

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", keep_inf = "TRUE"),
    "Assertion on 'keep_inf' failed")

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", fixed = NULL),
    "Assertion on 'fixed' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", fixed = "TRUE"),
    "Assertion on 'fixed' failed"
  )

  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", disabled = NULL),
    "Assertion on 'disabled' failed"
  )
  testthat::expect_error(
    filter_var(dataname = "data", varname = "var", disabled = "TRUE"),
    "Assertion on 'disabled' failed"
  )
})


testthat::test_that("filter_var returns `teal_slice`", {
  fs1 <- filter_var("data", "var1")

  testthat::expect_s3_class(fs1, "teal_slice")
  testthat::expect_s3_class(fs1, "list")
  testthat::expect_failure(
    testthat::expect_s3_class(fs1, "teal_slices")
  )
  testthat::expect_length(fs1, 8L)
})


testthat::test_that("filter_settings checks arguments", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")

  testthat::expect_no_error(filter_settings(fs1, fs2))
  testthat::expect_no_error(filter_settings(fs1, fs2, exclude = list(data = "var1"), count_type = "all"))

  testthat::expect_error(filter_settings(fs1, fs2, "fs1"), "Assertion on 'slices' failed")

  testthat::expect_error(filter_settings(fs1, fs2, exclude = "fs1"), "Assertion on 'exclude' failed")

  testthat::expect_error(filter_settings(fs1, fs2, count_type = "fs1"), "'arg' should be")
  testthat::expect_error(filter_settings(fs1, fs2, count_type = c("all", "none")), "'arg' must be of length 1")
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

  testthat::expect_false(is.null(attr(fs, "exclude")))
  testthat::expect_false(is.null(attr(fs, "count_type")))
  testthat::expect_length(filter_settings(fs1, fs2), 2L)
})


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


testthat::test_that("as.teal_slice checks arguments", {
  fl1 <- list(
    dataname = "data",
    varname = "var1"
  )

  testthat::expect_no_error(as.teal_slice(fl1))
  testthat::expect_error(as.teal_slice(unlist(fl1)), "Assertion on 'x' failed")
  testthat::expect_error(as.teal_slice(unname(fl1)), "Assertion on 'x' failed")
  testthat::expect_error(as.teal_slice(list()), "argument \"dataname\" is missing, with no default")
})


testthat::test_that("as.teal_slice converts list to `teal_slice`", {
  fl1 <- list(
    dataname = "data",
    varname = "var1"
  )
  fs1 <- filter_var("data", "var1")

  testthat::expect_s3_class(as.teal_slice(fl1), "teal_slice")
  testthat::expect_identical(as.teal_slice(fl1), fs1)
})


testthat::test_that("as.teal_slices checks arguments", {
  fl2 <- list(
    data1 = list(
      var1 = list(
        selected = "a",
        keep_na = TRUE
      )
    ),
    data1 = list(
      var2 = list(
        selected = 2,
        keep_na = TRUE,
        keep_inf = FALSE
      )
    )
  )

  testthat::expect_no_error(as.teal_slices(fl2))
  testthat::expect_no_error(as.teal_slices(fl2[1]))
  testthat::expect_error(as.teal_slices(fl2[[1]]), "conversion to filter_slices failed")
  testthat::expect_error(as.teal_slices(list("a", 1L)), "Assertion on 'x' failed")
  testthat::expect_error(as.teal_slices(list(a = "a", b = 1L)), "conversion to filter_slices failed")
})


testthat::test_that("as.teal_slices converts list to `teal_slices`", {
  fl3 <- list(
    data1 = list(
      var1 = list(
        selected = "a",
        keep_na = TRUE
      )
    ),
    data1 = list(
      subjects = list(
        var31 = list(
          selected = 31,
          keep_na = TRUE,
          keep_inf = FALSE
        )
      ),
      exp1 = list(
        subset = list(
          var32 = list(
            selected = 32,
            keep_na = TRUE,
            keep_inf = FALSE
          )
        )
      )
    )
  )
  fs3 <- filter_settings(
    filter_var("data1", "var1", selected = "a", keep_na = TRUE),
    filter_var("data1", "var31", selected = 31, keep_na = TRUE, keep_inf = FALSE, datalabel = "subjects", target = "y"),
    filter_var("data1", "var32", selected = 32, keep_na = TRUE, keep_inf = FALSE, datalabel = "exp1", target = "subset")
  )

  testthat::expect_s3_class(as.teal_slices(fl3), "teal_slices")
  testthat::expect_identical(as.teal_slices(fl3), fs3)

  testthat::expect_identical(as.teal_slices(list()), filter_settings())
})


testthat::test_that("[.teal_slices accepts various types of indices", {
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

  testthat::expect_no_error(fs[[]])
  testthat::expect_null(fs[[]])
  testthat::expect_error(fs[[integer(0)]], "attempt to select less than one element in get1index")
  testthat::expect_error(fs[[character(0)]], "attempt to select less than one element in get1index")
  testthat::expect_error(fs[[logical(0)]], "attempt to select less than one element in get1index")
  testthat::expect_no_error(fs[[1]])
  testthat::expect_no_error(fs[["one"]])
  testthat::expect_error(fs[[c(TRUE, FALSE)]], "attempt to select less than one element in integerOneIndex")
})


testthat::test_that("[.teal_slices subsets properly", {
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


testthat::test_that("[.teal_slices also subsets the exclude attribute", {
  fs1 <- filter_var("data1", "var1")
  fs2 <- filter_var("data1", "var2")
  fs3 <- filter_var("data2", "var1")
  fs4 <- filter_var("data2", "var2")
  fs <- filter_settings(fs1, fs2, fs3, fs4, exclude = list(data1 = "var1", data2 = "var1"))

  testthat::expect_identical(
    attr(fs[1], "exclude"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fs[2], "exclude"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fs[1:2], "exclude"),
    list(data1 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3], "exclude"),
    list(data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[4], "exclude"),
    list(data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[3:4], "exclude"),
    list(data2 = "var1")
  )
  testthat::expect_identical(
    attr(fs[], "exclude"),
    list(data1 = "var1", data2 = "var1")
  )
})


testthat::test_that("[.teal_slices preserves count_type", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  testthat::expect_identical(
    attr(fs, "count_type"),
    attr(fs[1], "count_type")
  )
})


testthat::test_that("c.teal_slice adds fields to `teal_slice`", {
  fs1 <- filter_var("data1", "var1")
  extra <- list(name = "value")

  testthat::expect_no_error(c(fs1, extra))
  testthat::expect_s3_class(c(fs1, extra), "teal_slice")
  testthat::expect_warning(c(fs1, fs1), "duplicate field names were discarded")

  testthat::expect_identical(suppressWarnings(c(fs1, fs1)), fs1)
  testthat::expect_identical(
    filter_var("data1", "var1", "name" = "value"),
    c(fs1, extra)
  )
  testthat::expect_identical(c(fs1, list()), fs1)
})


testthat::test_that("c.teal_slices concatenates `teal_slices` objects", {
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
  fss1 <- filter_settings(fs1, fs2, exclude = list(data1 = "var1"))
  fss2 <- filter_settings(fs3, fs4, exclude = list(data2 = "var1"))
  fss3 <- filter_settings(fs3, fs4, exclude = list(data2 = "var1"), count_type = "all")

  # teal_slices with different exclude attributes
  testthat::expect_no_error(c(fss1, fss2))

  fsss <- c(fss1, fss2)

  # exclude attributes are combined
  testthat::expect_identical(
    attr(fsss, "exclude"),
    list(data1 = "var1", data2 = "var1")
  )

  # count_type attribute is preserved
  testthat::expect_identical(
    attr(fsss, "count_type"),
    attr(fss1, "count_type")
  )

  # different count_type attributes raise error
  testthat::expect_error(c(fss1, fss3), "Assertion on 'count_types' failed")
})


testthat::test_that("slices_field works", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
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
})


testthat::test_that("slices_which works", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  # argument checks
  testthat::expect_no_error(slices_which(fs, "dataname == \"data\""))
  testthat::expect_error(slices_which(fs, str2lang("dataname == \"data\"")), "Assertion on 'expr' failed")

  # return type
  testthat::expect_s3_class(slices_which(fs, "dataname == \"data\""), "teal_slices")
  testthat::expect_s3_class(slices_which(fs, "varname == \"var1\""), "teal_slices")

  # proper content is returned
  testthat::expect_identical(slices_which(fs, "dataname == \"data\""), fs)
  testthat::expect_identical(slices_which(fs, "varname == \"var1\""), fs[1])
  testthat::expect_identical(slices_which(fs, "dataname == \"dataa\""), filter_settings())
})
