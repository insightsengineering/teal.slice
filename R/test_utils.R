# This file contains helper functions used in unit tests.

# compares specified fields between two `teal_slice` objects
compare_slices <- function(ts1, ts2, fields) {
  all(vapply(fields, function(x) identical(ts1[[x]], ts2[[x]]), logical(1L)))
}
