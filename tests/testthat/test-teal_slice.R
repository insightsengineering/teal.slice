
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


testthat::test_that("filter_var returns a teal_slice", {
  fs1 <- filter_var("data", "var1")

  testthat::expect_s3_class(fs1, "teal_slice")
  testthat::expect_s3_class(fs1, "list")
  testthat::expect_failure(
    testthat::expect_s3_class(fs1, "teal_slices")
  )
  testthat::expect_length(fs1, 8L)
})


testthat::test_that("filter_var checks arguments", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")

  testthat::expect_no_error(filter_settings(fs1, fs2))
  testthat::expect_no_error(filter_settings(fs1, fs2, exclude = list(data = c("var1")), count_type = "all"))

  testthat::expect_error(filter_settings(fs1, fs2, "fs1"), "Assertion on 'slices' failed")

  testthat::expect_error(filter_settings(fs1, fs2, exclude = "fs1"), "Assertion on 'exclude' failed")

  testthat::expect_error(filter_settings(fs1, fs2, count_type = "fs1"), "'arg' should be")
  testthat::expect_error(filter_settings(fs1, fs2, count_type = c("all", "none")), "'arg' must be of length 1")
})


testthat::test_that("filter_settings returns a teal_slices", {
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


testthat::test_that("as* functions work", {
  fl1 <- list(
    dataname = "data",
    varname = "var1"
  )
  fs1 <- filter_var("data", "var1")

  fl2 <- list(
    "data1" = list(
      "var1" = list(
        selected = "a",
        keep_na = TRUE
      )
    ),
    "data1" = list(
      "var2" = list(
        selected = 2,
        keep_na = TRUE,
        keep_inf = FALSE
      )
    )
  )
  fs2 <- filter_settings(
    filter_var("data1", "var1", selected = "a", keep_na = TRUE),
    filter_var("data1", "var2", selected = 2, keep_na = TRUE, keep_inf = FALSE)
  )

  testthat::expect_no_error(as.teal_slice(fl1))
  testthat::expect_error(as.teal_slice(fl2))

  testthat::expect_s3_class(as.teal_slice(fl1), "teal_slice")
  testthat::expect_identical(as.teal_slice(fl1), fs1)


  testthat::expect_no_error(as.teal_slices(fl2))
  testthat::expect_error(as.teal_slices(fl1), "conversion to filter_slices failed")

  testthat::expect_s3_class(as.teal_slices(fl2), "teal_slices")
  testthat::expect_identical(as.teal_slices(fl2), fs2)

  testthat::expect_identical(as.teal_slices(list()), filter_settings())
})


testthat::test_that("[.teal_slices works", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)
  names(fs) <- c("one", "two")

  fss1 <- filter_settings(fs1)
  names(fss1) <- "one"

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

  testthat::expect_s3_class(fs[1], "teal_slices")
  testthat::expect_s3_class(fs[[1]], "teal_slice")

  testthat::expect_identical(fs[1], fs["one"])
  testthat::expect_identical(fs[[1]], fs[["one"]])

  testthat::expect_identical(fs[1], fss1)
  testthat::expect_identical(fs[[1]], fss1[[1]])
  testthat::expect_identical(fs[["one"]], fss1[["one"]])

  fs <- unname(fs)
  fss1 <- unname(fss1)
  testthat::expect_identical(fs[1], fss1)
  testthat::expect_identical(fs[[1]], fss1[[1]])

  # TODO exclude and count_type no covered
})


testthat::test_that("c.teal_slices works", {
  fs1 <- filter_var("data1", "var1")
  fs2 <- filter_var("data1", "var2")
  fs3 <- filter_var("data2", "var1")
  fs4 <- filter_var("data2", "var2")
  fss1 <- filter_settings(fs1, fs2, exclude = list(data1 = "var1"))
  fss2 <- filter_settings(fs3, fs4, exclude = list(data2 = "var1"))
  fss3 <- filter_settings(fs3, fs4, exclude = list(data2 = "var1"), count_type = "all")

  testthat::expect_no_error(c(fss1, fss2))

  fsss <- c(fss1, fss2)

  testthat::expect_s3_class(fsss, "teal_slices")
  testthat::expect_length(fsss, length(fss1) + length(fss2))

  testthat::expect_identical(
    attr(fsss, "exclude"),
    list(data1 = "var1", data2 = "var1")
  )

  testthat::expect_identical(
    attr(fsss, "count_type"),
    attr(fss1, "count_type")
  )

  testthat::expect_error(c(fss1, fss3), "Assertion on 'count_types' failed")
})


testthat::test_that("slices_field works", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  testthat::expect_no_error(slices_field(fs, "dataname"))
  testthat::expect_error(slices_field(fs), "argument \"field\" is missing")

  testthat::expect_vector(slices_field(fs, "dataname"))
})


testthat::test_that("slices_which works", {
  fs1 <- filter_var("data", "var1")
  fs2 <- filter_var("data", "var2")
  fs <- filter_settings(fs1, fs2)

  testthat::expect_no_error(slices_which(fs, "dataname == \"data\""))
  testthat::expect_error(slices_which(fs, str2lang("dataname == \"data\"")), "Assertion on 'expr' failed")

  fssub1 <- slices_which(fs, "dataname == \"data\"")
  fssub2 <- slices_which(fs, "varname == \"var1\"")
  fssub3 <- slices_which(fs, "dataname == \"dataa\"")

  testthat::expect_s3_class(fssub1, "teal_slices")
  testthat::expect_s3_class(fssub1, "teal_slices")

  testthat::expect_identical(fssub1, fs)
  testthat::expect_identical(fssub2, fs[1])
  testthat::expect_identical(fssub3, filter_settings())
})
