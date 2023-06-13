# This file contains helper functions used in unit tests.

# compares specified fields between two `teal_slice` objects
compare_slices <- function(ts1, ts2, fields) {
  shiny::isolate(
    all(vapply(fields, function(x) identical(ts1[[x]], ts2[[x]]), logical(1L)))
  )
}


# compare two teal_slice
expect_identical_slice <- function(x, y) {
  shiny::isolate({
    testthat::expect_true(
      setequal(
        reactiveValuesToList(x),
        reactiveValuesToList(y)
      )
    )
  })
}

# compare two teal_slices
expect_identical_slices <- function(x, y) {
  shiny::isolate({
    mapply(
      function(x, y) {
        expect_identical_slice(x, y)
      },
      x = x,
      y = y
    )
    testthat::expect_identical(attributes(x), attributes(y))
  })
}
