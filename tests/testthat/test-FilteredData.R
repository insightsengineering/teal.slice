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
  testthat::expect_identical(filtered_data$get_keys("iris"), stats::setNames("test", "test"))
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

testthat::describe("srv_overview", {
  it("srv_overview produces table with filtered counts of non-relational datasets", {
    filtered_data <- init_filtered_data(x = list(iris = iris, mtcars = mtcars))
    filtered_data$set_filter_state(
      teal_slices(
        teal_slice(dataname = "iris", varname = "Species", selected = "setosa"),
        teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6))
      )
    )
    shiny::testServer(
      filtered_data$srv_overview,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        testthat::expect_identical(
          as.data.frame(
            rvest::html_table(rvest::read_html(as.character(output$table$html)),
              header = TRUE
            ),
            check.names = FALSE
          ),
          data.frame(
            `Data Name` = c("iris", "mtcars"),
            `Obs` = c("50/150", "18/32"),
            check.names = FALSE
          )
        )
      }
    )
  })

  it("srv_overview produces table with filtered counts of relational datasets", {
    parent <- data.frame(id = seq_len(5), a = letters[seq_len(5)])
    child <- data.frame(id = seq_len(10), parent_id = rep(seq_len(5), each = 2), b = LETTERS[seq_len(10)])

    filtered_data <- init_filtered_data(
      x = list(parent = parent, child = child),
      join_keys = teal.data::join_keys(
        teal.data::join_key("parent", "parent", "id"),
        teal.data::join_key("child", "child", "id"),
        teal.data::join_key("parent", "child", c(id = "parent_id"))
      )
    )
    filtered_data$set_filter_state(
      teal_slices(
        teal_slice(dataname = "parent", varname = "a", selected = c("a", "b", "c"))
      )
    )
    shiny::testServer(
      filtered_data$srv_overview,
      args = list(id = "test", active_datanames = reactive(c("parent", "child"))),
      expr = {
        testthat::expect_identical(
          as.data.frame(
            rvest::html_table(rvest::read_html(as.character(output$table$html)),
              header = TRUE
            ),
            check.names = FALSE
          ),
          data.frame(
            `Data Name` = c("parent", "child"),
            Obs = c("3/5", "6/10"),
            Subjects = c("3/5", "3/5"),
            check.names = FALSE
          )
        )
      }
    )
  })

  it("srv_overview produces table with popover information for unsupported dataset class", {
    filtered_data <- init_filtered_data(x = list(iris = iris, test = structure(1L, class = "testing-class")))
    shiny::testServer(
      filtered_data$srv_overview,
      args = list(id = "test", active_datanames = reactive(c("iris", "test"))),
      expr = {
        testthat::expect_identical(
          trimws(rvest::html_text(
            rvest::html_nodes(
              rvest::read_html(as.character(output$table$html)),
              xpath = "//td[i[@title='Unsupported dataset']]"
            )
          )),
          "test"
        )
      }
    )
  })

  it("srv_overview returns NULL when active_dataset returns nothing", {
    filtered_data <- init_filtered_data(x = list(iris = iris, mtcars = mtcars))
    filtered_data$set_filter_state(
      teal_slices(
        teal_slice(dataname = "iris", varname = "Species", selected = "setosa"),
        teal_slice(dataname = "mtcars", varname = "cyl", selected = c(4, 6))
      )
    )
    shiny::testServer(
      filtered_data$srv_overview,
      args = list(id = "test", active_datanames = reactive(character(0))),
      expr = {
        testthat::expect_null(output$table)
      }
    )
  })
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


testthat::describe("test FilterState server", {
  it("input$back restores previous filter selection", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "versicolor"))
        )
        session$flushReact()
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "setosa"))
        )
        session$flushReact()
        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          "setosa"
        )
        session$setInputs(`iris-filter-iris_Species-back` = 1L)
        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          "versicolor"
        )
      }
    )
  })

  it("input$reset restores initial filter selection", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "versicolor"))
        )
        session$flushReact()
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "setosa"))
        )
        session$flushReact()
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "virginica"))
        )
        session$flushReact()

        session$setInputs(`iris-filter-iris_Species-reset` = 1L)
        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          "versicolor"
        )
      }
    )
  })

  it("input$remove removes filter from states", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species"))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        session$setInputs(`iris-filter-iris_Species-remove` = 1L)
        testthat::expect_length(get_filter_state(filtered_data), 0)
      }
    )
  })

  it("setting input$keep_na-value reflects in filter-state", {
    filtered_data <- init_filtered_data(x = list(iris = within(iris, Species[1] <- NA)))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species", keep_na = FALSE))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        session$setInputs(`iris-filter-iris_Species-inputs-keep_na-value` = FALSE)
        testthat::expect_false(get_filter_state(filtered_data)[[1]]$keep_na)
      }
    )
  })
})


testthat::describe("test ChoicesFilterState server", {
  it("summary displays selected choices as text", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor")))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        testthat::expect_identical(
          trimws(rvest::html_text(
            rvest::read_html(as.character(output$`iris-filter-iris_Species-summary-summary`$html))
          )),
          "setosa, versicolor"
        )
      }
    )
  })

  it("summary displays 'n levels selected' when selected exceeds 40 characters", {
    filtered_data <- init_filtered_data(x = list(mtcars = within(mtcars, model <- rownames(mtcars))))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "mtcars", varname = "model"))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        testthat::expect_identical(
          trimws(rvest::html_text(
            rvest::read_html(as.character(output$`mtcars-filter-mtcars_model-summary-summary`$html))
          )),
          "32 levels selected"
        )
      }
    )
  })

  it("summary displays 'no selection' when nothing is selected", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = character(0), multiple = TRUE))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        testthat::expect_identical(
          trimws(rvest::html_text(
            rvest::read_html(as.character(output$`iris-filter-iris_Species-summary-summary`$html))
          )),
          "no selection"
        )
      }
    )
  })

  it("setting input$selection changes filter state (radioButton)", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species"))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        session$setInputs(`iris-filter-iris_Species-inputs-selection` = "setosa")
        session$setInputs(`iris-filter-iris_Species-inputs-selection_open` = FALSE)

        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          "setosa"
        )
      }
    )
  })

  it("setting input$selection changes filter state (pickerInput)", {
    withr::with_options(
      list("teal.threshold_slider_vs_checkboxgroup" = 1L), # when length(choices) < 1 then radio-button
      {
        filtered_data <- init_filtered_data(x = list(iris = iris))
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species"))
        )
        shiny::testServer(
          filtered_data$srv_active,
          args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
          expr = {
            session$flushReact()
            session$setInputs(`iris-filter-iris_Species-inputs-selection` = c("setosa", "versicolor"))
            session$setInputs(`iris-filter-iris_Species-inputs-selection_open` = FALSE)

            testthat::expect_identical(
              get_filter_state(filtered_data)[[1]]$selected,
              c("setosa", "versicolor")
            )
          }
        )
      }
    )
  })

  it("setting input$selection has no effect on filter state when fixed = TRUE", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species", fixed = TRUE))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        session$setInputs(`iris-filter-iris_Species-inputs-selection` = c("setosa", "versicolor", "virginica"))
        session$setInputs(`iris-filter-iris_Species-inputs-selection_open` = FALSE)

        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          c("setosa", "versicolor", "virginica")
        )
      }
    )
  })

  it("setting many input$selection when multiple = FALSE is ignored with warning (radioInput)", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "setosa", multiple = FALSE))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        testthat::expect_warning(
          session$setInputs(`iris-filter-iris_Species-inputs-selection` = c("setosa", "versicolor", "virginica")),
          "is not a vector of length one. Maintaining previous selection."
        )
        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          "setosa"
        )
      }
    )
  })

  it("setting many input$selection when multiple = FALSE is ignored with warning (pickerInput)", {
    withr::with_options(
      list("teal.threshold_slider_vs_checkboxgroup" = 1L), # when length(choices) < 1 then radio-button
      {
        filtered_data <- init_filtered_data(x = list(iris = iris))
        set_filter_state(
          filtered_data,
          teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "setosa", multiple = FALSE))
        )
        shiny::testServer(
          filtered_data$srv_active,
          args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
          expr = {
            session$flushReact()
            session$setInputs(`iris-filter-iris_Species-inputs-selection` = c("setosa", "versicolor", "virginica"))
            testthat::expect_warning(
              session$setInputs(`iris-filter-iris_Species-inputs-selection_open` = FALSE),
              "is not a vector of length one. Maintaining previous selection."
            )
            testthat::expect_identical(
              get_filter_state(filtered_data)[[1]]$selected,
              "setosa"
            )
          }
        )
      }
    )
  })
})


testthat::describe("test DateFilterState server", {
  it("summary displays selected date-range as text", {
    filtered_data <- init_filtered_data(x = list(iris = within(iris, date <- as.Date(seq_len(150)))))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "date", selected = c("1970-01-02", "1970-01-30")))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()
        testthat::expect_match(
          trimws(rvest::html_text(
            rvest::read_html(as.character(output$`iris-filter-iris_date-summary-summary`$html))
          )),
          "1970-01-02.+1970-01-30"
        )
      }
    )
  })

  it("setting input changes filter state $selected", {
    filtered_data <- init_filtered_data(x = list(iris = within(iris, date <- as.Date(seq_len(150)))))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "date"))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive(c("iris", "mtcars"))),
      expr = {
        session$flushReact()

        session$setInputs(`iris-filter-iris_date-inputs-selection` = c("1970-01-02", "1970-01-30"))
        testthat::expect_identical(
          get_filter_state(filtered_data)[[1]]$selected,
          as.Date(c("1970-01-02", "1970-01-30"))
        )
      }
    )
  })
})


testthat::describe("test RangeFilterState", {
  it("summary displays selected numeric-range as text", {
    filtered_data <- init_filtered_data(x = list(iris = iris))
    set_filter_state(
      filtered_data,
      teal_slices(teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.0, 6.0)))
    )
    shiny::testServer(
      filtered_data$srv_active,
      args = list(id = "test", active_datanames = reactive("iris")),
      expr = {
        session$flushReact()
        testthat::expect_match(
          trimws(rvest::html_text(
            rvest::read_html(as.character(output$`iris-filter-iris_Sepal_Length-summary-summary`$html))
          )),
          "^5.+6$"
        )
      }
    )
  })
})
