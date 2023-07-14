# initialize ----
testthat::test_that("constructor accepts only a string as dataname", {
  testthat::expect_no_error(FilterStates$new(data = NULL, dataname = "string"))
  testthat::expect_error(FilterStates$new(data = NULL, dataname = quote(name)), "Assertion on 'dataname' failed")
  testthat::expect_error(FilterStates$new(data = NULL, dataname = call("call")), "Assertion on 'dataname' failed")
})

# filter states api -----
testthat::test_that("get_filter_state returns default count_type = 'none'", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    teal_slices(count_type = "none")
  )
})

testthat::test_that("set_filter_state sets include_variables by excluding unsupported cols from inputed list", {
  test <- iris
  test$col <- as.complex(1:150)
  test$col2 <- as.list(1:150)
  filter_states <- FilterStates$new(data = test, dataname = "test")
  teal_slices <- teal_slices(
    include_varnames = list(test = c("Species", "Sepal.Length", "inexisting", "col", "col2"))
  )
  filter_states$set_filter_state(teal_slices)
  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    teal_slices(
      include_varnames = list(test = c("Species", "Sepal.Length")),
      count_type = "none"
    )
  )
})

testthat::test_that("set_filter_state sets count_type", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  filter_states$set_filter_state(teal_slices(count_type = "none"))
  testthat::expect_identical(
    shiny::isolate(filter_states$get_filter_state()),
    teal_slices(count_type = "none")
  )
})

testthat::test_that("set_filter_state ignores teal_slice for inexisting variables with log warning", {
  filter_states <- FilterStates$new(data = data.frame(a = 1), dataname = "test")
  res <- utils::capture.output(
    filter_states$set_filter_state(teal_slices(teal_slice(dataname = "test", varname = "inexisting")))
  )
  testthat::expect_true(grepl("\\[WARN\\].+inexisting excluded from test", res))
})

testthat::test_that("set_filter_state and get_filter_state, sets and returns the same fully specified teal_slices", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:10), dataname = "test")
  fs <- teal_slices(
    teal_slice(
      dataname = "test", varname = "a", choices = c(1, 5), selected = c(1, 4), keep_na = FALSE, keep_inf = FALSE,
      fixed = FALSE, any_attribute = "a", another_attribute = "b"
    ),
    count_type = "none"
  )
  filter_states$set_filter_state(fs)
  expect_identical_slices(filter_states$get_filter_state(), fs)
})

testthat::test_that("set_filter_state updates FilterState when dataname and varname are matched between teal_slice and
existing filter", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:10), dataname = "test")
  fs <- teal_slices(
    teal_slice(
      dataname = "test", varname = "a", choices = c(1, 5), selected = c(1, 4), keep_na = FALSE, keep_inf = FALSE,
      fixed = FALSE, locked = FALSE, any_attribute = "a", another_attribute = "b"
    ),
    count_type = "none"
  )
  filter_states$set_filter_state(fs)
  fs[[1]]$selected <- c(1, 5)
  filter_states$set_filter_state(fs)
  expect_identical_slices(filter_states$get_filter_state(), fs)
})

testthat::test_that("set_filter_state allows to create two filters on the same variable if combination of their
fields (dataname, varname, varlabel, arg, id) differ", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:10), dataname = "a")
  fs <- teal_slices(
    teal_slice(dataname = "a", varname = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a", id = "a"),
    count_type = "none"
  )
  filter_states$set_filter_state(fs)
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 4)
})


testthat::test_that("set_filter_state creates a new FilterStateExpr", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:5, b = 6:10), dataname = "test")
  fs <- teal_slices(
    teal_slice(id = "test", dataname = "test", title = "expression", expr = "a > 1 & b < 10"),
    count_type = "none"
  )
  filter_states$set_filter_state(fs)
  expect_identical_slices(filter_states$get_filter_state(), fs)
})

testthat::test_that("remove_filter_state of inexistent FilterState raiser warning", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:5), dataname = "a")
  testthat::expect_warning(
    filter_states$remove_filter_state(teal_slices(teal_slice(dataname = "a", varname = "a"))),
    "not found in state list"
  )
})

testthat::test_that("remove_filter_state removes FilterState objects identified by 'dataname', 'experiment',
'varname', 'arg' and/or 'id'", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:5), dataname = "a")
  fs <- teal_slices(
    teal_slice(dataname = "a", varname = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a", id = "a"),
    count_type = "none"
  )
  filter_states$set_filter_state(fs)

  filter_states$remove_filter_state(teal_slices(teal_slice(dataname = "a", varname = "a")))
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 3)

  filter_states$remove_filter_state(teal_slices(
    teal_slice(dataname = "a", varname = "a", experiment = "a")
  ))
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 2)

  filter_states$remove_filter_state(teal_slices(
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a")
  ))
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 1)

  filter_states$remove_filter_state(teal_slices(
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a", id = "a")
  ))
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 0)
})

testthat::test_that("clearing empty `FilterStates` does not raise errors", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  testthat::expect_no_error(filter_states$clear_filter_states())
})


testthat::test_that("clear_filter_state empties the state_list", {
  filter_states <- FilterStates$new(data = data.frame(a = 1:5), dataname = "a")
  fs <- teal_slices(
    teal_slice(dataname = "a", varname = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a"),
    teal_slice(dataname = "a", varname = "a", experiment = "a", arg = "a", id = "a"),
    count_type = "none"
  )
  filter_states$set_filter_state(fs)
  filter_states$clear_filter_states()
  testthat::expect_length(shiny::isolate(filter_states$get_filter_state()), 0)
})


# get_call ----
testthat::test_that("get_call returns NULL after initialization if no filter applied", {
  filter_states <- FilterStates$new(data = NULL, dataname = "test")
  testthat::expect_null(shiny::isolate(filter_states$get_call()))
})

testthat::test_that("get_call returns subset call with dataname and logical expressions by default", {
  fs <- FilterStates$new(data = data.frame(a = 1:10), dataname = "test", datalabel = "1")
  fs$set_filter_state(teal_slices(
    teal_slice(dataname = "test", varname = "a", experiment = "1", selected = c(1, 9))
  ))
  testthat::expect_identical(
    shiny::isolate(fs$get_call()),
    quote(test <- subset(test, a >= 1 & a <= 9))
  )
})

testthat::test_that("get_call returns custom fun call", {
  test <- R6::R6Class(
    "test",
    inherit = FilterStates,
    private = list(
      fun = quote(fun)
    )
  )
  fs <- test$new(data = data.frame(a = 1:10), dataname = "test", datalabel = "1")
  fs$set_filter_state(teal_slices(
    teal_slice(dataname = "test", varname = "a", experiment = "1", selected = c(1, 9))
  ))

  testthat::expect_identical(
    shiny::isolate(fs$get_call()),
    quote(test <- fun(test, a >= 1 & a <= 9))
  )
})

testthat::test_that("get_call returns subset call on custom dataname_prefixed", {
  test <- R6::R6Class(
    "test",
    inherit = FilterStates,
    public = list(
      initialize = function(data, dataname) {
        super$initialize(data = data, dataname = dataname)
        private$dataname_prefixed <- 'dataname[["slot"]]'
      }
    )
  )
  fs <- test$new(data = data.frame(a = 1:10), dataname = "test")
  fs$set_filter_state(teal_slices(
    teal_slice(dataname = "test", varname = "a", experiment = "1", selected = c(1, 9))
  ))

  testthat::expect_identical(
    shiny::isolate(fs$get_call()),
    quote(dataname[["slot"]] <- subset(dataname[["slot"]], a >= 1 & a <= 9))
  )
})

testthat::test_that("get_call returns subset with varnames prefixed depending on a extract_type", {
  test <- R6::R6Class(
    "test",
    inherit = FilterStates,
    private = list(
      extract_type = "list"
    )
  )
  fs <- test$new(data = data.frame(a = 1:10), dataname = "test")
  fs$set_filter_state(teal_slices(
    teal_slice(dataname = "test", varname = "a", experiment = "1", selected = c(1, 9))
  ))

  testthat::expect_identical(
    shiny::isolate(fs$get_call()),
    quote(test <- subset(test, test$a >= 1 & test$a <= 9))
  )
})

testthat::test_that("get_call returns subset with multiple filter expressions combined by '&' operator", {
  fs <- FilterStates$new(data = data.frame(a = 1:10, b = 1:10, c = 1:10), dataname = "test")
  fs$set_filter_state(teal_slices(
    teal_slice(dataname = "test", varname = "a", experiment = "1", selected = c(1, 9)),
    teal_slice(id = "a", dataname = "test", title = "a", expr = "b > 5 | a > 5")
  ))

  testthat::expect_equal(
    shiny::isolate(fs$get_call()),
    quote(test <- subset(test, a >= 1 & a <= 9 & (b > 5 | a > 5)))
  )
})


testthat::test_that("get_call skips conditions form FilterState which are identified by sid", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))
  test_class <- R6::R6Class(
    classname = "test_class",
    inherit = FilterStates,
    public = list(
      get_filter_states_sid = function() {
        names(private$state_list())
      }
    )
  )
  filter_states <- test_class$new(data = iris, dataname = "iris")
  filter_states$set_filter_state(
    teal_slices(
      teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
      teal_slice(dataname = "iris", varname = "Petal.Length", selected = c(1.5, 6.9)),
      teal_slice(dataname = "iris", varname = "Species", selected = "setosa")
    )
  )
  sid_attrs <- unname(filter_states$get_filter_states_sid())
  testthat::expect_equal(
    filter_states$get_call(sid = sid_attrs[1]),
    quote(
      iris <- subset(iris, Petal.Length >= 1.5 & Petal.Length <= 6.9 & Species == "setosa")
    )
  )
  testthat::expect_equal(
    filter_states$get_call(sid = sid_attrs[2]),
    quote(
      iris <- subset(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4 & Species == "setosa")
    )
  )
  testthat::expect_equal(
    filter_states$get_call(sid = sid_attrs[3]),
    quote(
      iris <- subset(
        iris,
        Sepal.Length >= 5.1 & Sepal.Length <= 6.4 & (Petal.Length >= 1.5 & Petal.Length <= 6.9)
      )
    )
  )
  testthat::expect_null(
    filter_states$get_call(sid = sid_attrs)
  )
})

# todo: test srv_active - we probably need shinytest2
# todo: format

# module_add ----
testthat::test_that("ui_add returns a message inside a div when data has no columns or no rows", {
  filter_states <- FilterStates$new(data = data.frame(), dataname = "iris")
  testthat::expect_identical(
    filter_states$ui_add("id"),
    div("no sample variables available")
  )
})

testthat::test_that("Selecting a new variable initializes a new filter state with default states", {
  filter_states <- FilterStates$new(data = iris, dataname = "iris")
  shiny::testServer(
    filter_states$srv_add,
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )
  expect_identical_slices(
    filter_states$get_filter_state(),
    teal_slices(
      teal_slice(
        dataname = "iris",
        varname = "Sepal.Length",
        choices = c(4.3, 7.9),
        selected = c(4.3, 7.9)
      ),
      count_type = "none"
    )
  )
})

testthat::test_that("Adding 'var_to_add' adds another filter state", {
  filter_states <- FilterStates$new(data = iris, dataname = "iris")

  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE)
  )
  filter_states$set_filter_state(state = fs)
  shiny::testServer(
    filter_states$srv_add,
    expr = {
      session$setInputs(var_to_add = "Petal.Length")
    }
  )
  shiny::testServer(
    filter_states$srv_add,
    expr = {
      session$setInputs(var_to_add = "Species")
    }
  )

  expect_identical_slices(
    shiny::isolate(filter_states$get_filter_state()),
    teal_slices(
      teal_slice(
        dataname = "iris", varname = "Sepal.Length", choices = c(4.3, 7.9), selected = c(5.1, 6.4),
        keep_na = FALSE, keep_inf = FALSE, fixed = FALSE, locked = FALSE
      ),
      teal_slice(
        dataname = "iris", varname = "Petal.Length", choices = c(1.0, 6.9), selected = c(1.0, 6.9),
        keep_na = NULL, keep_inf = NULL, fixed = FALSE, locked = FALSE
      ),
      teal_slice(
        dataname = "iris", varname = "Species", choices = c("setosa", "versicolor", "virginica"),
        multiple = TRUE, selected = c("setosa", "versicolor", "virginica"), keep_na = NULL, keep_inf = NULL,
        fixed = FALSE, locked = FALSE
      ),
      count_type = "none"
    )
  )
})

testthat::test_that("srv_add determines labels for the choices based on the column attribute", {
  data <- iris[c("Sepal.Length", "Species")]
  colnames(data) <- tolower(colnames(data))
  attr(data[["sepal.length"]], "label") <- "Sepal length"
  attr(data[["species"]], "label") <- "Species"
  filter_states <- FilterStates$new(data = data, dataname = "test")

  shiny::testServer(
    filter_states$srv_add,
    expr = {
      testthat::expect_identical(
        avail_column_choices(),
        structure(
          c(`sepal.length: Sepal length` = "sepal.length", `species: Species` = "species"),
          raw_labels = c("Sepal length", "Species"),
          combined_labels = c("sepal.length: Sepal length", "species: Species"),
          class = c("choices_labeled", "character"),
          types = c(sepal.length = "numeric", species = "factor")
        )
      )
    }
  )
})

testthat::test_that("srv_add limits choices to the include_varnames", {
  data <- iris
  colnames(data) <- tolower(colnames(data))
  filter_states <- FilterStates$new(data = data, dataname = "test")
  filter_states$set_filter_state(state = teal_slices(
    include_varnames = list(test = c("sepal.length", "species"))
  ))

  shiny::testServer(
    filter_states$srv_add,
    expr = {
      testthat::expect_identical(
        avail_column_choices(),
        structure(
          c(`sepal.length: sepal.length` = "sepal.length", `species: species` = "species"),
          raw_labels = c("sepal.length", "species"),
          combined_labels = c("sepal.length: sepal.length", "species: species"),
          class = c("choices_labeled", "character"),
          types = c(sepal.length = "numeric", species = "factor")
        )
      )
    }
  )
})

testthat::test_that("srv_add flags keys as primary_key", {
  data <- iris
  colnames(data) <- tolower(colnames(data))
  testfs <- R6::R6Class(
    "testfs",
    inherit = FilterStates,
    public = list(
      initialize = function(data, dataname, keys) {
        super$initialize(data = data, dataname = dataname)
        private$keys <- keys
      }
    )
  )
  filter_states <- testfs$new(data = data, dataname = "test", keys = "species")
  filter_states$set_filter_state(state = teal_slices(
    include_varnames = list(test = c("sepal.length", "species"))
  ))

  shiny::testServer(
    filter_states$srv_add,
    expr = {
      testthat::expect_identical(
        avail_column_choices(),
        structure(
          c(`sepal.length: sepal.length` = "sepal.length", `species: species` = "species"),
          raw_labels = c("sepal.length", "species"),
          combined_labels = c("sepal.length: sepal.length", "species: species"),
          class = c("choices_labeled", "character"),
          types = c(sepal.length = "numeric", species = "primary_key")
        )
      )
    }
  )
})
