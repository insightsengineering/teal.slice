# This file contains helper functions used in unit tests.

# compares specified fields between two `teal_slice` objects
#' @noRd
#' @keywords internal
compare_slices <- function(ts1, ts2, fields) {
  isolate(
    all(vapply(fields, function(x) identical(ts1[[x]], ts2[[x]]), logical(1L)))
  )
}


# compare two teal_slice
#' @noRd
#' @keywords internal
expect_identical_slice <- function(x, y) {
  isolate({
    testthat::expect_true(
      setequal(
        reactiveValuesToList(x),
        reactiveValuesToList(y)
      )
    )
  })
}

# compare two teal_slices
#' @noRd
#' @keywords internal
expect_identical_slices <- function(x, y) {
  isolate({
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
