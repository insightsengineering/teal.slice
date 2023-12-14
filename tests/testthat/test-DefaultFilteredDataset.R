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
    " - unfiltered dataset:\t\"character\":   character"
  )
})

testthat::test_that("format trims very long class names to 40 characters if trim_lines = TRUE", {
  classes <- c("someclass1", "someclass2", "someclass3", "someclass4", "somanyclasses")
  fds <- DefaultFilteredDataset$new(structure(letters, class = classes), "character")
  testthat::expect_identical(
    nchar(fds$format(trim_lines = FALSE)),
    nchar(paste0(" - unfiltered dataset:\t\"character\":   ")) + nchar(toString(classes))
  )

  testthat::expect_identical(
    nchar(fds$format(trim_lines = TRUE)),
    nchar(paste0(" - unfiltered dataset:\t\"character\":   ")) + 37L
  )
})

# get_call ----
testthat::test_that("get_call returns NULL", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(fds$get_call())
})
# get_filter_state ----
testthat::test_that("get_filter_state returns NULL", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(fds$get_filter_state())
})
# set_filter_state ----
testthat::test_that("set_filter_state returns NULL, raises warning if `state` is not empty", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  tss0 <- teal_slices()
  tss1 <- teal_slices(teal_slice("letters", "letter"))
  testthat::expect_null(fds$set_filter_state(tss0))
  testthat::expect_null(
    testthat::expect_warning(fds$set_filter_state(tss1), "cannot set state")
  )
})
# clear_filter_states ----
testthat::test_that("clear_filter_state returns NULL", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_null(fds$clear_filter_states())
})
# get_filter_overview ----
testthat::test_that("get_filter_overview returns NULL", {
  fds <- DefaultFilteredDataset$new(letters, "character")
  testthat::expect_identical(
    fds$get_filter_overview(),
    data.frame(dataname = "character", obs = NA, obs_filtered = NA)
  )
})
