# initialize ----
testthat::test_that("constructor accepts all types of datasets", {
  testthat::skip_if_not_installed("MultiAssayExperiment")
  utils::data(miniACC, package = "MultiAssayExperiment")

  testthat::expect_no_error(FilteredData$new(list("logical" = c(TRUE, FALSE))))
  testthat::expect_no_error(FilteredData$new(list("integer" = 1:10)))
  testthat::expect_no_error(FilteredData$new(list("numeric" = 1:10 * 1)))
  testthat::expect_no_error(FilteredData$new(list("character" = letters)))
  testthat::expect_no_error(FilteredData$new(list("factor" = as.factor(letters))))
  testthat::expect_no_error(FilteredData$new(list("list" = as.list(letters))))
  testthat::expect_no_error(FilteredData$new(list("function" = function() letters)))

  testthat::expect_no_error(FilteredData$new(list("array" = array(1:27, dim = c(3, 3, 3)))))
  testthat::expect_no_error(FilteredData$new(list("dataframe" = iris)))
  testthat::expect_no_error(FilteredData$new(list("mae" = miniACC)))
})

testthat::test_that("constructor accepts join_keys to be join_keys or NULL", {
  testthat::expect_no_error(FilteredData$new(list(iris = iris), join_keys = teal.data::join_keys()))
  testthat::expect_no_error(FilteredData$new(list(iris = iris)))
  testthat::expect_error(
    FilteredData$new(list(iris = iris), join_keys = list()),
    "Assertion on 'join_keys' failed"
  )
})

# datanames ----
testthat::test_that("filtered_data$datanames returns character vector of datasets names", {
  filtered_data <- FilteredData$new(list(df1 = iris, df2 = iris))
  testthat::expect_identical(filtered_data$datanames(), c("df1", "df2"))
})

testthat::test_that("datanames are ordered topologically from parent to child", {
  jk <- teal.data::join_keys(teal.data::join_key("parent", "child", c("id" = "id")))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(list(child = head(iris2), parent = head(iris2)), join_keys = jk)
  testthat::expect_identical(filtered_data$datanames(), c("parent", "child"))
  filtered_data <- FilteredData$new(list(parent = head(iris2), child = head(iris2)), join_keys = jk)
  testthat::expect_identical(filtered_data$datanames(), c("parent", "child"))
})

# set_dataset ----
testthat::test_that("set_dataset accepts data being `data.frame`", {
  filtered_data <- FilteredData$new(data_objects = list())
  testthat::expect_no_error(filtered_data$set_dataset(data = iris, dataname = "iris"))
})

testthat::test_that("set_dataset returns self", {
  filtered_data <- FilteredData$new(data_objects = list())
  testthat::expect_identical(
    filtered_data$set_dataset(data = iris, dataname = "iris"),
    filtered_data
  )
})

testthat::test_that("set_dataset creates FilteredDataset object", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = FilteredData,
    public = list(
      get_filtered_datasets = function() private$filtered_datasets
    )
  )
  filtered_data <- test_class$new(data_objects = list())
  filtered_data$set_dataset(data = iris, dataname = "iris")
  checkmate::expect_list(
    filtered_data$get_filtered_datasets(),
    types = "FilteredDataset"
  )
})

testthat::test_that("set_datasets creates FilteredDataset object linked with parent", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = FilteredData,
    public = list(
      get_filtered_datasets = function() private$filtered_datasets
    )
  )
  jk <- teal.data::join_keys(teal.data::join_key("parent", "child", c("id" = "id")))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- test_class$new(data_objects = list(), join_keys = jk)
  filtered_data$set_dataset(data = head(iris), dataname = "parent")
  filtered_data$set_dataset(data = head(iris), dataname = "child")
  testthat::expect_identical(
    shiny::isolate(filtered_data$get_call("child"))[[1]],
    quote(child <- dplyr::inner_join(x = child, y = parent[, c("id"), drop = FALSE], by = "id"))
  )
})


# get_keys ----
testthat::test_that("get_join_keys returns empty join_keys object", {
  filtered_data <- FilteredData$new(list(iris = head(iris)))
  testthat::expect_s3_class(filtered_data$get_join_keys(), "join_keys")
})

testthat::test_that("get_keys returns keys of the dataset specified via join_keys", {
  jk <- teal.data::join_keys(teal.data::join_key("iris", "iris", "test"))
  filtered_data <- FilteredData$new(list(iris = head(iris)), join_keys = jk)
  testthat::expect_identical(filtered_data$get_keys("iris"), setNames("test", "test"))
})

testthat::test_that("get_join_keys returns join_keys object if it exists", {
  filtered_data <- FilteredData$new(
    list(iris = head(iris), iris2 = head(iris)),
    join_keys = teal.data::join_keys(teal.data::join_key("iris", "iris2", c("Species" = "Species")))
  )
  testthat::expect_equal(
    filtered_data$get_join_keys(),
    teal.data::join_keys(
      teal.data::join_key("iris", "iris2", c("Species" = "Species"))
    )
  )
})


# get_datalabel ----
testthat::test_that("get_datalabel returns character(0) for dataset with no label", {
  this_iris <- iris
  attr(this_iris, "label") <- "Super iris"
  filtered_data <- FilteredData$new(list(iris = this_iris))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "Super iris")
})

# get_call ----
testthat::test_that("get_call returns a NULL if no filters applied", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  testthat::expect_null(shiny::isolate(datasets$get_call("iris")))
  testthat::expect_null(shiny::isolate(datasets$get_call("mtcars")))
})

testthat::test_that("get_call return a list of calls when filter applied", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  testthat::expect_identical(
    shiny::isolate(datasets$get_call("iris")),
    list(
      filter = quote(iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4))
    )
  )
  testthat::expect_identical(
    shiny::isolate(datasets$get_call("mtcars")),
    list(
      filter = quote(mtcars <- dplyr::filter(mtcars, cyl %in% c(4, 6)))
    )
  )
})

testthat::test_that("get_call raises error if dataname doesn't match available datasets", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  testthat::expect_error(shiny::isolate(datasets$get_call(dataname = "idontexist")))
})


# get_filter_expr ----
testthat::test_that("get_filter_expr returns empty string when no filters applied", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  testthat::expect_identical(shiny::isolate(get_filter_expr(datasets)), "")
})

testthat::test_that("get_filter_expr returns all filter calls as character", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE)
  )

  datasets$set_filter_state(state = fs)
  testthat::expect_identical(
    shiny::isolate(get_filter_expr(datasets)),
    paste(
      "iris <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4)",
      "mtcars <- dplyr::filter(mtcars, cyl %in% c(4, 6))",
      sep = "\n"
    )
  )
})


# get_data ----
testthat::test_that("get_data argument `filtered` must be a flag", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = iris))
  testthat::expect_no_error(filtered_data$get_data("iris", filtered = FALSE))
  testthat::expect_error(
    filtered_data$get_data("iris", filtered = "Wrong type"), "Assertion on 'filtered' failed"
  )
})

testthat::test_that("get_data requires that dataname be subset of datanames", {
  filtered_data <- FilteredData$new(list(iris = head(iris)))
  testthat::expect_no_error(filtered_data$get_data("iris", filtered = FALSE))
  testthat::expect_error(filtered_data$get_data("mtcars", filtered = FALSE), "Assertion on 'dataname' failed")
})

testthat::test_that("get_data filtered = FALSE returns the same object as passed to the constructor", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = iris))
  testthat::expect_equal(filtered_data$get_data("iris", filtered = FALSE), iris)
})

testthat::test_that("get_data returns the same object as passed to the constructor if no filter applied", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = iris))
  testthat::expect_equal(shiny::isolate(filtered_data$get_data("iris")), iris)
})

testthat::test_that("get_data returns an object filtered by set filters", {
  datasets <- FilteredData$new(list(iris = iris))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE)
  )

  datasets$set_filter_state(state = fs)
  testthat::expect_identical(
    shiny::isolate(datasets$get_data("iris")),
    dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4)
  )
})

testthat::test_that("get_data of the child is dependent on the ancestor filter", {
  jk <- teal.data::join_keys(
    teal.data::join_key("parent", "child", c("id" = "id")),
    teal.data::join_key("child", "grandchild", c("id" = "id"))
  )
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(
    list(grandchild = head(iris2), child = head(iris2), parent = head(iris2)),
    join_keys = jk
  )
  filtered_data$set_filter_state(teal_slices(
    teal_slice(dataname = "parent", varname = "id", selected = c(1, 1), keep_na = FALSE, keep_inf = FALSE)
  ))

  testthat::expect_identical(
    shiny::isolate(filtered_data$get_data("grandchild", filtered = TRUE)),
    dplyr::filter(iris2, id == 1)
  )
})

testthat::test_that("get_data of the child is dependent on the ancestor filter (mismatched columns)", {
  jk <- teal.data::join_keys(
    teal.data::join_key("parent", "child", c("pk" = "id")),
    teal.data::join_key("child", "grandchild", c("id" = "id"))
  )
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  iris_parent <- dplyr::rename(iris2, pk = "id")
  filtered_data <- FilteredData$new(
    list(grandchild = head(iris2), child = head(iris2), parent = head(iris_parent)),
    join_keys = jk
  )
  filtered_data$set_filter_state(teal_slices(
    teal_slice(dataname = "parent", varname = "pk", selected = c(1, 1), keep_na = FALSE, keep_inf = FALSE)
  ))

  testthat::expect_identical(
    shiny::isolate(filtered_data$get_data("grandchild", filtered = TRUE)),
    dplyr::filter(iris2, id == 1)
  )
})

# get_filter_state ----
testthat::test_that("get_filter_state returns `teal_slices` with features identical to those in input", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))

  fs <- teal_slices(
    teal_slice(
      dataname = "iris", varname = "Sepal.Length",
      choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    teal_slice(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), selected = c("setosa", "versicolor"),
      keep_na = FALSE
    ),
    teal_slice(
      dataname = "mtcars", varname = "cyl",
      choices = c("4", "6", "8"), selected = c("4", "6"),
      keep_na = FALSE, keep_inf = FALSE
    ),
    count_type = "none",
    include_varnames = list(mtcars = "cyl"),
    exclude_varnames = list(iris = c("Petal.Length", "Petal.Width"))
  )

  datasets$set_filter_state(state = fs)

  fs_out <- unname(shiny::isolate(datasets$get_filter_state()))

  expect_identical_slices(fs, fs_out)
})

# format ----
testthat::test_that("format returns properly formatted string representing `teal_slices` with class header", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))

  fs <- teal_slices(
    teal_slice(
      dataname = "iris", varname = "Sepal.Length",
      choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    teal_slice(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), multiple = TRUE, selected = c("setosa", "versicolor"),
      keep_na = FALSE
    ),
    teal_slice(
      dataname = "mtcars", varname = "cyl",
      choices = c("4", "6", "8"), multiple = TRUE, selected = c("4", "6"),
      keep_na = FALSE, keep_inf = FALSE
    ),
    count_type = "none",
    include_varnames = list(mtcars = "cyl"),
    exclude_varnames = list(iris = c("Petal.Length", "Petal.Width"))
  )

  datasets$set_filter_state(state = fs)

  testthat::expect_identical(
    shiny::isolate(datasets$format()),
    paste0("FilteredData:\n", format(fs))
  )
  testthat::expect_identical(
    shiny::isolate(datasets$format(show_all = TRUE)),
    paste0("FilteredData:\n", format(fs, show_all = TRUE))
  )
  testthat::expect_identical(
    shiny::isolate(datasets$format(trim_lines = FALSE)),
    paste0("FilteredData:\n", format(fs, trim_lines = FALSE))
  )
  testthat::expect_identical(
    shiny::isolate(datasets$format(show_all = TRUE, trim_lines = FALSE)),
    paste0("FilteredData:\n", format(fs, show_all = TRUE, trim_lines = FALSE))
  )
})

testthat::test_that("format lists unfiltered datasets at the end of the output", {
  datasets <- FilteredData$new(list(iris = iris, letters = letters, mtcars = mtcars))

  fs <- teal_slices(
    teal_slice(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), multiple = TRUE, selected = c("setosa", "versicolor"),
      keep_na = FALSE
    ),
    teal_slice(
      dataname = "mtcars", varname = "cyl",
      choices = c("4", "6", "8"), multiple = TRUE, selected = c("4", "6"),
      keep_na = FALSE, keep_inf = FALSE
    ),
    count_type = "none",
    include_varnames = list(mtcars = "cyl"),
    exclude_varnames = list(iris = c("Petal.Length", "Petal.Width"))
  )

  datasets$set_filter_state(fs)

  state_fmt <- shiny::isolate(format(datasets$get_filter_state()))

  testthat::expect_identical(
    shiny::isolate(datasets$format()),
    paste0("FilteredData:\n", state_fmt, "\n - unfiltered dataset:\t\"letters\":   character")
  )
})

# remove_filter_state ----
testthat::test_that("remove_filter_state removes states specified by `teal_slices", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  datasets$remove_filter_state(
    teal_slices(
      teal_slice(dataname = "iris", varname = "Sepal.Length"),
      teal_slice(dataname = "mtcars", varname = "cyl"),
      teal_slice(dataname = "mtcars", varname = "disp")
    )
  )
  testthat::expect_identical(
    shiny::isolate(unique(unlist(lapply(datasets$get_filter_state(), "[[", "varname")))),
    "Species"
  )
})

testthat::test_that("remove_filter_state does not remove anchored filters", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(
      dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    teal_slice(
      dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"),
      keep_na = FALSE, anchored = TRUE
    ),
    teal_slice(
      dataname = "iris", varname = "Sepal.Width", selected = c(2.5, 3.3),
      keep_na = FALSE, keep_inf = FALSE, anchored = TRUE
    )
  )
  datasets$set_filter_state(state = fs)
  datasets$remove_filter_state(fs)

  testthat::expect_length(shiny::isolate(datasets$get_filter_state()), 2)
  testthat::expect_true(
    shiny::isolate(unique(unlist(lapply(datasets$get_filter_state(), "[[", "anchored"))))
  )
})


# clear_filter_states ----
testthat::test_that("clear_filter_states removes all filters of all datasets in FilteredData", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  datasets$clear_filter_states()

  testthat::expect_s3_class(shiny::isolate(datasets$get_filter_state()), "teal_slices")
  testthat::expect_length(shiny::isolate(datasets$get_filter_state()), 0)
})

testthat::test_that("clear_filter_states removes filters of desired dataset only", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  datasets$clear_filter_states(datanames = "iris")

  testthat::expect_identical(
    shiny::isolate(unique(vapply(datasets$get_filter_state(), "[[", character(1L), "dataname"))),
    "mtcars"
  )
})

testthat::test_that("clear_filter_states does not remove anchored filters", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(
      dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    teal_slice(
      dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"),
      keep_na = FALSE, anchored = TRUE
    ),
    teal_slice(
      dataname = "iris", varname = "Sepal.Width", selected = c(2.5, 3.3),
      keep_na = FALSE, keep_inf = FALSE, anchored = TRUE
    )
  )
  datasets$set_filter_state(state = fs)
  datasets$clear_filter_states()

  testthat::expect_length(shiny::isolate(datasets$get_filter_state()), 2)
  testthat::expect_true(
    shiny::isolate(unique(unlist(lapply(datasets$get_filter_state(), "[[", "anchored"))))
  )
})

# get_filter_overview ----
testthat::test_that("get_filter_overview checks arguments", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("iris")))
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("mtcars")))
  testthat::expect_error(
    shiny::isolate(datasets$get_filter_overview()), "argument \"datanames\" is missing, with no default"
  )
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview("AA")))
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview("")))
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview(23)))
})

testthat::test_that("get_filter_overview returns overview data.frame with obs counts if the keys are not specified", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  testthat::expect_equal(
    shiny::isolate(datasets$get_filter_overview(datasets$datanames())),
    data.frame(
      dataname = c("iris", "mtcars"),
      obs = c(150, 32),
      obs_filtered = c(150, 32)
    )
  )
})

testthat::test_that("get_filter_overview returns overview data.frame with filtered counts", {
  datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  datasets$set_filter_state(
    teal_slices(
      teal_slice(
        dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 5.1),
        keep_na = TRUE, keep_inf = FALSE
      ),
      teal_slice(
        dataname = "mtcars", varname = "cyl", selected = 6,
        keep_na = FALSE, keep_inf = FALSE
      )
    )
  )

  testthat::expect_equal(
    shiny::isolate(datasets$get_filter_overview(datasets$datanames())),
    data.frame(
      dataname = c("iris", "mtcars"),
      obs = c(150, 32),
      obs_filtered = c(9, 7)
    )
  )
})

testthat::test_that("get_filter_overview return counts based on reactive filtering by ancestors", {
  jk <- teal.data::join_keys(
    teal.data::join_key("parent", "child", c("id" = "id")),
    teal.data::join_key("child", "grandchild", c("id" = "id"))
  )
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(
    list(grandchild = head(iris2), child = head(iris2), parent = head(iris2)),
    join_keys = jk
  )
  filtered_data$set_filter_state(
    teal_slices(teal_slice(dataname = "parent", varname = "id", selected = c(1, 2)))
  )
  testthat::expect_equal(
    shiny::isolate(filtered_data$get_filter_overview(c("child", "parent"))),
    data.frame(
      dataname = c("child", "parent"),
      obs = c(6, 6),
      obs_filtered = c(2, 2),
      subjects = c(6, NA),
      subjects_filtered = c(2, NA)
    )
  )
})

# active_datanames ----
testthat::test_that("active_datanames fails if returns dataname which isn't a subset of available datanames", {
  filtered_data <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  shiny::testServer(
    filtered_data$srv_filter_panel,
    args = list(active_datanames = function() c("iris", "idontexist")),
    expr = {
      testthat::expect_error(active_datanames_resolved())
    }
  )
})

# srv_active ----
testthat::test_that("srv_active - output$teal_filters_count returns (reactive) number of current filters applied", {
  filtered_data <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_active,
    expr = {
      testthat::expect_identical(output$teal_filters_count, "4 filters applied across datasets")
    }
  )
})

testthat::test_that("srv_active - clicking remove_all button clears filters", {
  filtered_data <- FilteredData$new(list(iris = iris, mtcars = mtcars))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_active,
    expr = {
      session$setInputs(remove_all_filters = TRUE)
      testthat::expect_s3_class(filtered_data$get_filter_state(), "teal_slices")
      testthat::expect_length(filtered_data$get_filter_state(), 0)
    }
  )
})

# get_filter_count
testthat::test_that("get_filter_count properly tallies active filter states", {
  testthat::skip_if_not_installed("MultiAssayExperiment")
  utils::data(miniACC, package = "MultiAssayExperiment")
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = FilteredData,
    public = list(
      get_filter_count = function() private$get_filter_count()
    )
  )
  datasets <- test_class$new(list(iris = iris, mtcars = mtcars, mae = miniACC))
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    teal_slice(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  shiny::isolate(testthat::expect_equal(datasets$get_filter_count(), 0L))
  datasets$set_filter_state(state = fs)
  shiny::isolate(testthat::expect_equal(datasets$get_filter_count(), 4L))
})

testthat::test_that("get_filter_count properly tallies active filter states for MAE objects", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = FilteredData,
    public = list(
      get_filter_count = function() {
        length(self$get_filter_state())
      }
    )
  )
  testthat::skip_if_not_installed("MultiAssayExperiment")
  utils::data(miniACC, package = "MultiAssayExperiment")
  datasets <- test_class$new(list(iris = iris, mtcars = mtcars, mae = miniACC))
  fs <- teal_slices(
    teal_slice(dataname = "mae", varname = "years_to_birth", selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
    teal_slice(dataname = "mae", varname = "vital_status", selected = "1", keep_na = FALSE),
    teal_slice(dataname = "mae", varname = "gender", selected = "female", keep_na = TRUE),
    teal_slice(
      dataname = "mae", varname = "ARRAY_TYPE",
      selected = "", keep_na = TRUE, experiment = "RPPAArray", arg = "subset"
    )
  )
  shiny::isolate(testthat::expect_equal(datasets$get_filter_count(), 0L))
  datasets$set_filter_state(state = fs)
  shiny::isolate(testthat::expect_equal(datasets$get_filter_count(), 4L))
})



test_class <- R6::R6Class(
  classname = "test_class",
  inherit = FilteredData,
  public = list(
    srv_available_filters = function(id) {
      private$srv_available_filters(id)
    },
    ui_available_filters = function(id) {
      private$ui_available_filters(id)
    }
  )
)
datasets <- test_class$new(list(iris = iris))
fs <- teal_slices(
  teal_slice(dataname = "iris", varname = "Sepal.Length", anchored = TRUE),
  teal_slice(dataname = "iris", varname = "Sepal.Width", fixed = TRUE),
  teal_slice(dataname = "iris", varname = "Petal.Length"),
  teal_slice(dataname = "iris", varname = "Petal.Width"),
  teal_slice(dataname = "iris", varname = "Petal.Width", id = "duplicated"),
  teal_slice(dataname = "iris", title = "test", id = "test", expr = "!is.na(Species)")
)
fs_rv <- shiny::reactiveVal(fs)
datasets$set_available_teal_slices(shiny::reactive(fs_rv()))
datasets$set_filter_state(fs[1:2])
shiny::testServer(
  datasets$srv_available_filters,
  expr = {
    testthat::test_that("slices_interactive() reactive returns interactive filters", {
      expect_identical_slices(slices_interactive(), fs[c(1, 3, 4, 5)])
    })
    testthat::test_that("slices_fixed() reactive returns fixed filters and teal_slice_expr", {
      expect_identical_slices(slices_fixed(), fs[c(2, 6)])
    })

    testthat::test_that("FilteredData$srv_available_slices new state in external list reflected in available slices", {
      species_slice <- teal_slice(dataname = "iris", varname = "Species")
      fs_rv(c(fs_rv(), teal_slices(species_slice)))
      testthat::expect_identical(
        available_slices_id(),
        c(
          "iris Sepal.Length", "iris Sepal.Width", "iris Petal.Length",
          "iris Petal.Width", "duplicated", "test", "iris Species"
        )
      )
    })

    testthat::test_that("active_slices_id returns list of currently active filters", {
      testthat::expect_identical(active_slices_id(), c("iris Sepal.Length", "iris Sepal.Width"))
    })

    testthat::test_that("FilteredData$srv_available_slices changing input values de/activate states", {
      session$setInputs(available_slices_id = c("iris Sepal.Length", "iris Sepal.Width"))
      session$setInputs(available_slices_id = c("iris Sepal.Length"))
      testthat::expect_identical(active_slices_id(), c("iris Sepal.Length"))

      session$setInputs(available_slices_id = c("iris Sepal.Length", "iris Sepal.Width", "iris Species"))
      testthat::expect_identical(active_slices_id(), c("iris Sepal.Length", "iris Sepal.Width", "iris Species"))
    })

    testthat::test_that("FilteredData$srv_available_slices deactivating all keeps anchored states", {
      session$setInputs(available_slices_id = NULL)
      testthat::expect_identical(active_slices_id(), "iris Sepal.Length")
    })

    testthat::test_that("duplicated_slice_references() returns character(0) if none of duplicated filters is active", {
      session$setInputs(available_slices_id = "Sepal.Length")
      testthat::expect_identical(duplicated_slice_references(), character(0))
    })

    testthat::test_that(
      "duplicated_slice_references() returns variable reference when any of duplicated filters is on",
      {
        session$setInputs(available_slices_id = "duplicated")
        testthat::expect_identical(duplicated_slice_references(), "iris Petal.Width")
      }
    )
  }
)
