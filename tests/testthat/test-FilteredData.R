# initialize ----
testthat::test_that("constructor accepts call with only dataset specified", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  df_dataset <- list(dataset = iris)
  mae_dataset <- list(dataset = miniACC)
  testthat::expect_no_error(FilteredData$new(list(iris = df_dataset)))
  testthat::expect_no_error(FilteredData$new(list(iris = df_dataset, mae = mae_dataset)))

  dataset <- list(dataset = structure(list(), class = "customclass"))
  testthat::expect_error(FilteredData$new(list(iris = dataset)), "Must inherit")
})

testthat::test_that("constructor accepts join_keys to be JoinKeys or NULL", {
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), join_keys = teal.data::join_keys())
  )
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)))
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), join_keys = list())
  )
})

testthat::test_that("constructor accepts code to be CodeClass or NULL", {
  mockcodeclass <- R6::R6Class(classname = "CodeClass")
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), code = mockcodeclass$new())
  )
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), code = NULL)
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), code = list(), "Assertion on 'code' failed")
  )
})

testthat::test_that("constructor accepts check to be a flag", {
  testthat::expect_no_error(
    FilteredData$new(list(iris = list(dataset = iris)), check = TRUE)
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), check = NULL, "Assertion on 'check' failed")
  )
  testthat::expect_error(
    FilteredData$new(list(iris = list(dataset = iris)), check = logical(0), "Assertion on 'check' failed")
  )
})

testthat::test_that("FilteredData preserves the check field when check is TRUE", {
  code <- teal.data:::CodeClass$new()$set_code("df_1 <- data.frame(x = 1:10)")

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    code = code,
    check = FALSE
  )
  testthat::expect_false(filtered_data$get_check())

  filtered_data <- FilteredData$new(
    list("df_1" = list(dataset = data.frame(x = 1:10))),
    code = code,
    check = TRUE
  )
  testthat::expect_true(filtered_data$get_check())
})

testthat::test_that("FilteredData forbids cyclic graphs of datasets relationship", {
  jk <- teal.data::join_keys(
    teal.data::join_key("child", "parent", c("id" = "id")),
    teal.data::join_key("grandchild", "child", c("id" = "id")),
    teal.data::join_key("grandchild", "parent", c("id" = "id"))
  )
  jk$set_parents(list(child = "parent"))
  jk$set_parents(list(grandchild = "child"))
  jk$set_parents(list(parent = "grandchild"))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  testthat::expect_error(
    FilteredData$new(
      list(
        grandchild = list(dataset = head(iris2)),
        child = list(dataset = head(iris2)),
        parent = list(dataset = head(iris2))
      ),
      join_keys = jk
    ),
    "Graph is not a directed acyclic graph"
  )
})


# datanames ----
testthat::test_that("filtered_data$datanames returns character vector of datasets names", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(df1 = dataset, df2 = dataset))
  testthat::expect_identical(filtered_data$datanames(), c("df1", "df2"))
})

testthat::test_that("datanames are ordered topologically from parent to child", {
  jk <- teal.data::join_keys(teal.data::join_key("parent", "child", c("id" = "id")))
  jk$set_parents(list(child = "parent"))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(
    list(
      child = list(dataset = head(iris2)),
      parent = list(dataset = head(iris2))
    ),
    join_keys = jk
  )
  testthat::expect_identical(filtered_data$datanames(), c("parent", "child"))
  filtered_data <- FilteredData$new(
    list(
      parent = list(dataset = head(iris2)),
      child = list(dataset = head(iris2))
    ),
    join_keys = jk
  )
  testthat::expect_identical(filtered_data$datanames(), c("parent", "child"))
})


# get_filterable_dataname ----
testthat::test_that("get_filterable_dataname returns all datasets by default", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(iris = dataset, iris2 = dataset))
  testthat::expect_identical(filtered_data$get_filterable_datanames(), c("iris", "iris2"))
})

testthat::test_that("get_filterable_dataname('all') returns all datasets", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(iris = dataset, iris2 = dataset))
  testthat::expect_identical(filtered_data$get_filterable_datanames("all"), c("iris", "iris2"))
})

testthat::test_that("get_filterable_dataname raises error when dataname not subset of current datanames", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(iris = dataset))
  testthat::expect_error(filtered_data$get_filterable_datanames("idontexist"))
})

testthat::test_that("get_filterable_dataname returns dataname same as input", {
  dataset <- list(dataset = iris)
  filtered_data <- FilteredData$new(list(iris = dataset))
  testthat::expect_identical(filtered_data$get_filterable_datanames("iris"), "iris")
})

testthat::test_that("get_filterable_datanames returns all ancestors if parents are specified in join_keys", {
  jk <- teal.data::join_keys(
    teal.data::join_key("child", "parent", c("Species" = "Species")),
    teal.data::join_key("grandchild", "child", c("Species" = "Species"))
  )
  jk$set_parents(list(child = "parent"))
  jk$set_parents(list(grandchild = "child"))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(
    list(
      grandchild = list(dataset = head(iris2)),
      child = list(dataset = head(iris2)),
      parent = list(dataset = head(iris2))
    ),
    join_keys = jk
  )

  testthat::expect_identical(filtered_data$get_filterable_datanames("parent"), "parent")
  testthat::expect_identical(filtered_data$get_filterable_datanames("child"), c("parent", "child"))
  testthat::expect_identical(filtered_data$get_filterable_datanames("grandchild"), c("parent", "child", "grandchild"))
})


# set_dataset ----
testthat::test_that("set_dataset accepts data being `data.frame`", {
  filtered_data <- FilteredData$new(data_objects = list())
  testthat::expect_no_error(filtered_data$set_dataset(data = iris, dataname = "iris", label = NULL, metadata = NULL))
})

testthat::test_that("set_dataset returns self", {
  filtered_data <- FilteredData$new(data_objects = list())
  testthat::expect_identical(
    filtered_data$set_dataset(data = iris, dataname = "iris", label = NULL, metadata = NULL),
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
  filtered_data$set_dataset(data = iris, dataname = "iris", label = NULL, metadata = NULL)
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
  jk$set_parents(list(child = "parent"))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- test_class$new(data_objects = list(), join_keys = jk)
  filtered_data$set_dataset(data = head(iris), dataname = "parent", label = NULL, metadata = NULL)
  filtered_data$set_dataset(data = head(iris), dataname = "child", label = NULL, metadata = NULL)
  testthat::expect_identical(
    shiny::isolate(filtered_data$get_call("child"))[[1]],
    quote(child <- dplyr::inner_join(x = child, y = parent[, c("id"), drop = FALSE], by = "id"))
  )
})


# get_keys ----
testthat::test_that("get_join_keys returns empty JoinKeys object", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_s3_class(filtered_data$get_join_keys(), "JoinKeys")
})

testthat::test_that("get_keys returns keys of the dataset specified via join_keys", {
  jk <- teal.data::join_keys(teal.data::join_key("iris", "iris", "test"))
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))), join_keys = jk)
  testthat::expect_identical(filtered_data$get_keys("iris"), setNames("test", "test"))
})

testthat::test_that("get_join_keys returns join_keys object if it exists", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris)),
      iris2 = list(dataset = head(iris))
    ),
    join_keys = teal.data::join_keys(
      teal.data::join_key("iris", "iris2", c("Species" = "Species"))
    )
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
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), character(0))
})

testthat::test_that("get_datalabel returns the label of a passed dataset", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris), label = "test")))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "test")
})


# get_metadata ----
testthat::test_that("get_metadata raises error if dataset does not exist", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_error(filtered_data$get_metadata("mtcars"), "Assertion on 'dataname' failed")
})

testthat::test_that("get_metadata returns metadata if dataset exists", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = head(iris), metadata = list(E = TRUE)),
      iris2 = list(dataset = head(iris))
    )
  )
  testthat::expect_equal(filtered_data$get_metadata("iris"), list(E = TRUE))
  testthat::expect_null(filtered_data$get_metadata("iris2"))
})


# get_code ----
testthat::test_that("get_code returns the code passed to CodeClass$set_code", {
  code <- teal.data:::CodeClass$new()
  code$set_code("'preprocessing code'", "iris")
  filtered_data <- FilteredData$new(
    list(iris = list(dataset = head(iris))),
    code = code
  )
  testthat::expect_equal(filtered_data$get_code(), "\"preprocessing code\"")
})

testthat::test_that("get_code returns a string when FilteredData has no code", {
  filtered_data <- FilteredData$new(data_objects = list())
  testthat::expect_identical(filtered_data$get_code(), "# No pre-processing code provided")
})


# get_call ----
testthat::test_that("get_call returns a NULL if no filters applied", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  testthat::expect_null(shiny::isolate(datasets$get_call("iris")))
  testthat::expect_null(shiny::isolate(datasets$get_call("mtcars")))
})

testthat::test_that("get_call return a list of calls when filter applied", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE)
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
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )

  testthat::expect_error(shiny::isolate(datasets$get_call(dataname = "idontexist")))
})


# get_filter_expr ----
testthat::test_that("get_filter_expr returns empty string when no filters applied", {
  datasets <- FilteredData$new(list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)))
  testthat::expect_identical(shiny::isolate(get_filter_expr(datasets)), "")
})

testthat::test_that("get_filter_expr returns all filter calls as character", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE)
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
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_no_error(filtered_data$get_data("iris", filtered = FALSE))
  testthat::expect_error(
    filtered_data$get_data("iris", filtered = "Wrong type"), "Assertion on 'filtered' failed"
  )
})

testthat::test_that("get_data requires that dataname be subset of datanames", {
  filtered_data <- FilteredData$new(list(iris = list(dataset = head(iris))))
  testthat::expect_no_error(filtered_data$get_data("iris", filtered = FALSE))
  testthat::expect_error(filtered_data$get_data("mtcars", filtered = FALSE), "Assertion on 'dataname' failed")
})

testthat::test_that("get_data filtered = FALSE returns the same object as passed to the constructor", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_equal(filtered_data$get_data("iris", filtered = FALSE), iris)
})

testthat::test_that("get_data returns the same object as passed to the constructor if no filter applied", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_equal(shiny::isolate(filtered_data$get_data("iris")), iris)
})

testthat::test_that("get_data returns an object filtered by set filters", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE)
  )

  datasets$set_filter_state(state = fs)
  testthat::expect_identical(
    shiny::isolate(datasets$get_data("iris")),
    dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4)
  )
})

testthat::test_that("get_data of the child is dependent on the ancestor filter", {
  jk <- teal.data::join_keys(
    teal.data::join_key("child", "parent", c("id" = "id")),
    teal.data::join_key("grandchild", "child", c("id" = "id"))
  )
  jk$set_parents(list(child = "parent"))
  jk$set_parents(list(grandchild = "child"))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(
    list(
      grandchild = list(dataset = head(iris2)),
      child = list(dataset = head(iris2)),
      parent = list(dataset = head(iris2))
    ),
    join_keys = jk
  )
  filtered_data$set_filter_state(filter_settings(
    filter_var(dataname = "parent", varname = "id", selected = c(1, 1), keep_na = FALSE, keep_inf = FALSE)
  ))

  testthat::expect_identical(
    shiny::isolate(filtered_data$get_data("grandchild", filtered = TRUE)),
    dplyr::filter(iris2, id == 1)
  )
})

# supporting previous api ----
testthat::test_that("set_filter_state accepts `teal_slices` and nested list and both set identical settings", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  x <- list(iris = list(dataset = iris), mae = list(dataset = miniACC))
  datasets1 <- init_filtered_data(x)
  datasets2 <- init_filtered_data(x)
  fs1 <- structure(
    list(
      iris = list(
        Species = list(selected = c("setosa", "versicolor")),
        Sepal.Length = c(5.1, 6.4),
        Petal.Length = list()
      ),
      mae = list(
        subjects = list(
          years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
          vital_status = list(selected = "1", keep_na = FALSE),
          gender = list(selected = "female", keep_na = TRUE)
        ),
        RPPAArray = list(
          subset = list(
            ARRAY_TYPE = list(selected = "", keep_na = TRUE)
          )
        )
      )
    ),
    filterable = list(
      iris = c("Species", "Sepal.Length", "Petal.Length"),
      mae = c("years_to_birth", "vital_status", "gender")
    )
  )

  fs2 <- filter_settings(
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor")),
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
    filter_var(dataname = "iris", varname = "Petal.Length"),
    filter_var(
      dataname = "mae", varname = "years_to_birth", datalabel = "subjects",
      selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE
    ),
    filter_var(
      dataname = "mae", varname = "vital_status", datalabel = "subjects",
      selected = "1", keep_na = FALSE
    ),
    filter_var(
      dataname = "mae", varname = "gender", datalabel = "subjects",
      selected = "female", keep_na = TRUE
    ),
    filter_var(
      dataname = "mae", varname = "ARRAY_TYPE", datalabel = "RPPAArray", arg = "subset",
      selected = "", keep_na = TRUE
    ),
    include_varnames = list(
      iris = c("Species", "Sepal.Length", "Petal.Length"),
      mae = c("years_to_birth", "vital_status", "gender")
    )
  )

  testthat::expect_warning(datasets1$set_filter_state(fs1), "deprecated")
  datasets2$set_filter_state(fs2)

  expect_identical_slices(datasets1$get_filter_state(), datasets2$get_filter_state())
})


# get_filter_state / format ----
testthat::test_that("get_filter_state returns `teal_slices` with features identical to those in input, adds format", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )

  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Sepal.Length",
      choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    filter_var(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), selected = c("setosa", "versicolor"),
      keep_na = FALSE
    ),
    filter_var(
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

  testthat::expect_identical(
    shiny::isolate(datasets$format()),
    paste0("FilteredData:\n", format(fs_out))
  )
  testthat::expect_identical(
    shiny::isolate(datasets$format(show_all = TRUE)),
    paste0("FilteredData:\n", format(fs_out, show_all = TRUE))
  )
})

# print ---
testthat::test_that("print returns properly formatted string representing `teal_slices`", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )

  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Sepal.Length",
      choices = c(4.3, 7.9), selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    filter_var(
      dataname = "iris", varname = "Species",
      choices = c("setosa", "versicolor", "virginica"), multiple = TRUE, selected = c("setosa", "versicolor"),
      keep_na = FALSE
    ),
    filter_var(
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
    utils::capture.output(shiny::isolate(datasets$print())),
    c(
      "FilteredData:",
      utils::capture.output(print(fs)),
      " "
    )
  )
  testthat::expect_identical(
    utils::capture.output(shiny::isolate(datasets$print(show_all = TRUE))),
    c(
      "FilteredData:",
      utils::capture.output(print(fs, show_all = TRUE)),
      " "
    )
  )
})

# remove_filter_state ----
testthat::test_that("remove_filter_state removes states specified by `teal_slices", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  datasets$remove_filter_state(
    filter_settings(
      filter_var(dataname = "iris", varname = "Sepal.Length"),
      filter_var(dataname = "mtcars", varname = "cyl"),
      filter_var(dataname = "mtcars", varname = "disp")
    )
  )
  testthat::expect_identical(
    shiny::isolate(slices_field(datasets$get_filter_state(), "varname")),
    "Species"
  )
})

testthat::test_that("remove_filter_state does not remove locked filters", {
  datasets <- teal.slice:::FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars, metadata = list(type = "training"))
    )
  )
  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    filter_var(
      dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"),
      keep_na = FALSE, locked = TRUE
    ),
    filter_var(
      dataname = "iris", varname = "Sepal.Width", selected = c(2.5, 3.3),
      keep_na = FALSE, keep_inf = FALSE, locked = TRUE
    )
  )
  datasets$set_filter_state(state = fs)
  datasets$remove_filter_state(fs)

  testthat::expect_length(shiny::isolate(datasets$get_filter_state()), 2)
  testthat::expect_true(
    shiny::isolate(teal.slice:::slices_field(datasets$get_filter_state(), "locked"))
  )
})


# clear_filter_states ----
testthat::test_that("clear_filter_states removes all filters of all datasets in FilteredData", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  datasets$clear_filter_states()

  testthat::expect_null(shiny::isolate(datasets$get_filter_state()))
})

testthat::test_that("clear_filter_states removes filters of desired dataset only", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  datasets$set_filter_state(state = fs)
  datasets$clear_filter_states(datanames = "iris")

  testthat::expect_identical(shiny::isolate(slices_field(datasets$get_filter_state(), "dataname")), "mtcars")
})

testthat::test_that("clear_filter_states does not remove locked filters", {
  datasets <- teal.slice:::FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars, metadata = list(type = "training"))
    )
  )
  fs <- filter_settings(
    filter_var(
      dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
      keep_na = FALSE, keep_inf = FALSE
    ),
    filter_var(
      dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"),
      keep_na = FALSE, locked = TRUE
    ),
    filter_var(
      dataname = "iris", varname = "Sepal.Width", selected = c(2.5, 3.3),
      keep_na = FALSE, keep_inf = FALSE, locked = TRUE
    )
  )
  datasets$set_filter_state(state = fs)

  datasets$clear_filter_states()

  testthat::expect_length(shiny::isolate(datasets$get_filter_state()), 2)
  testthat::expect_true(
    shiny::isolate(teal.slice:::slices_field(datasets$get_filter_state(), "locked"))
  )
})

# get_filter_overview ----
testthat::test_that("get_filter_overview checks arguments", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("iris")))
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("mtcars")))
  testthat::expect_no_error(shiny::isolate(datasets$get_filter_overview("all")))
  testthat::expect_error(
    shiny::isolate(datasets$get_filter_overview()), "argument \"datanames\" is missing, with no default"
  )
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview("AA")), "Must be a subset of")
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview("")), "Must be a subset of")
  testthat::expect_error(shiny::isolate(datasets$get_filter_overview(23)), "Must be a subset of")
})

testthat::test_that("get_filter_overview returns overview data.frame with obs counts if the keys are not specified", {
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
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
  datasets <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  datasets$set_filter_state(
    filter_settings(
      filter_var(
        dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 5.1),
        keep_na = TRUE, keep_inf = FALSE
      ),
      filter_var(
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
    teal.data::join_key("child", "parent", c("id" = "id")),
    teal.data::join_key("grandchild", "child", c("id" = "id"))
  )
  jk$set_parents(list(child = "parent"))
  jk$set_parents(list(grandchild = "child"))
  iris2 <- transform(iris, id = seq_len(nrow(iris)))
  filtered_data <- FilteredData$new(
    list(
      grandchild = list(dataset = head(iris2)),
      child = list(dataset = head(iris2)),
      parent = list(dataset = head(iris2))
    ),
    join_keys = jk
  )
  filtered_data$set_filter_state(
    filter_settings(filter_var(dataname = "parent", varname = "id", selected = c(1, 2)))
  )
  testthat::expect_equal(
    shiny::isolate(filtered_data$get_filter_overview(c("child", "parent"))),
    data.frame(
      dataname = c("parent", "child"),
      obs = c(6, 6),
      obs_filtered = c(2, 2),
      subjects = c(NA, 6),
      subjects_filtered = c(NA, 2)
    )
  )
})

# active_datanames ----
testthat::test_that("active_datanames in srv_filter_panel gets resolved to valid datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  shiny::testServer(
    filtered_data$srv_filter_panel,
    args = list(active_datanames = function() "all"),
    expr = {
      testthat::expect_identical(active_datanames_resolved(), c("iris", "mtcars"))
    }
  )
  shiny::testServer(
    filtered_data$srv_filter_panel,
    args = list(active_datanames = function() "iris"),
    expr = {
      testthat::expect_identical(active_datanames_resolved(), c("iris"))
    }
  )
})

testthat::test_that("active_datanames fails if returns dataname which isn't a subset of available datanames", {
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
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
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
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
  filtered_data <- FilteredData$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
  )
  filtered_data$set_filter_state(fs)
  shiny::testServer(
    filtered_data$srv_active,
    expr = {
      session$setInputs(remove_all_filters = TRUE)
      testthat::expect_null(filtered_data$get_filter_state())
    }
  )
})

# other ----
testthat::test_that("turn filed by default equal to TRUE", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_true(filtered_data$get_filter_panel_active())
})

testthat::test_that("get_filter_panel_ui_id - empty when no shiny session", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  testthat::expect_length(filtered_data$get_filter_panel_ui_id(), 0)
})

testthat::test_that("get_filter_panel_ui_id - non-empty when in shiny session", {
  filtered_data <- FilteredData$new(data_objects = list("iris" = list(dataset = iris)))
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      testthat::expect_length(filtered_data$get_filter_panel_ui_id(), 1)
    }
  )
})


# get_filter_count
testthat::test_that("get_filter_count properly tallies active filter states", {
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = FilteredData,
    public = list(
      get_filter_count = function() private$get_filter_count()
    )
  )
  datasets <- test_class$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars),
      mae = list(dataset = miniACC)
    )
  )
  fs <- filter_settings(
    filter_var(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    filter_var(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"), keep_na = FALSE),
    filter_var(dataname = "mtcars", varname = "cyl", selected = c(4, 6), keep_na = FALSE, keep_inf = FALSE),
    filter_var(dataname = "mtcars", varname = "disp", keep_na = FALSE, keep_inf = FALSE)
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
  datasets <- test_class$new(
    list(
      iris = list(dataset = iris),
      mtcars = list(dataset = mtcars),
      mae = list(dataset = miniACC)
    )
  )
  fs <- filter_settings(
    filter_var(
      dataname = "mae", varname = "years_to_birth",
      selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE, datalabel = "subjects", arg = "y"
    ),
    filter_var(
      dataname = "mae", varname = "vital_status",
      selected = "1", keep_na = FALSE, datalabel = "subjects", arg = "y"
    ),
    filter_var(
      dataname = "mae", varname = "gender",
      selected = "female", keep_na = TRUE, datalabel = "subjects", arg = "y"
    ),
    filter_var(
      dataname = "mae", varname = "ARRAY_TYPE",
      selected = "", keep_na = TRUE, datalabel = "RPPAArray", arg = "subset"
    )
  )
  shiny::isolate(testthat::expect_equal(datasets$get_filter_count(), 0L))
  datasets$set_filter_state(state = fs)
  shiny::isolate(testthat::expect_equal(datasets$get_filter_count(), 4L))
})
