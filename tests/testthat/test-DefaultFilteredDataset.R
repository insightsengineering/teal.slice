# initialize ----
testthat::test_that("constructor accepts all types of datasets", {
  testthat::expect_no_error(FilteredData$new(list("logical" = c(TRUE, FALSE))))
  testthat::expect_no_error(FilteredData$new(list("integer" = 1:10)))
  testthat::expect_no_error(FilteredData$new(list("numeric" = 1:10 * 1)))
  testthat::expect_no_error(FilteredData$new(list("character" = letters)))
  testthat::expect_no_error(FilteredData$new(list("factor" = as.factor(letters))))
  testthat::expect_no_error(FilteredData$new(list("list" = as.list(letters))))
  testthat::expect_no_error(FilteredData$new(list("function" = function() letters)))
  testthat::expect_no_error(FilteredData$new(list("array" = array(1:27, dim = c(3, 3, 3)))))
})

# format ----
testthat::test_that("format dispalys object name and class", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_identical(
    fds$format(),
    "DefaultFilteredDataset:\n\"character\": character"
  )
})

# get_call ----
testthat::test_that("get_call returns NULL with a warning", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(
    testthat::expect_warning(fds$get_call(), "does not create filter calls")
  )
})

# get_filter_state ----
testthat::test_that("get_filter_state returns NULL with a warning", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(
    testthat::expect_warning(fds$get_filter_state(), "does not have state")
  )
})
# set_filter_state ----
testthat::test_that("set_filter_state returns NULL with a warning", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  tss <- teal_slices(teal_slice("letters", "letter"))
  testthat::expect_null(
    testthat::expect_warning(fds$set_filter_state(tss), "cannot set state")
  )
})
# clear_filter_states ----
testthat::test_that("clear_filter_state returns NULL with a warning", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(
    testthat::expect_warning(fds$clear_filter_states(), "does not have filter states")
  )
})
# get_filter_overview ----
testthat::test_that("get_filter_overview returns NULL with a warning", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(
    fds$get_filter_overview()
  )
})
