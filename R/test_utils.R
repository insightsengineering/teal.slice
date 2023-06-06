# This file contains helper functions used in unit tests.

# compares specified fields between two `teal_slice` objects
compare_slices <- function(ts1, ts2, fields) {
  all(vapply(fields, function(x) identical(ts1[[x]], ts2[[x]]), logical(1L)))
}


# compare two reactiveValues
expect_identical_slice <- function(x, y) {
  shiny::isolate(
    testthat::expect_identical(
      reactiveValuesToList(x),
      reactiveValuesToList(y))
  )
}

expect_identical_slices <- function(x, y) {
  shiny::isolate({
    x2 <- lapply(x, reactiveValuesToList)
    y2 <- lapply(y, reactiveValuesToList)
    attributes(x2) <- attributes(x)
    attributes(y2) <- attributes(y)
    testthat::expect_identical(x2, y2)
  })
}
