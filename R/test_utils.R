# This file contains helper functions used in unit tests.

# sets `choices` field to NULL in all `teal_slice`s in a `teal_slices`
adjust_states <- function(tss) {
  ans <- lapply(tss, function(x) {
    x <- unclass(x)
    x$choices <- NULL
    x
  })
  unname(do.call(filter_settings, lapply(ans, as.teal_slice)))
}

# compares specified fields between two `teal_slice` objects
compare_slices <- function(ts1, ts2, fields) {
  all(vapply(fields, function(x) identical(ts1[[x]], ts2[[x]]), logical(1L)))
}
