filtered_data <- teal.slice:::init_filtered_data(list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)))

testthat::test_that("FilterPanelAPI constructor accepts a FilteredData object", {
  testthat::expect_no_error(FilterPanelAPI$new(filtered_data))
})

testthat::test_that("FilterPanelAPI constructor throws error with a non FilteredData object", {
  testthat::expect_error(
    FilterPanelAPI$new(list(iris = list(dataset = iris))),
    "Must inherit from class 'FilteredData', but has class 'list'."
  )

  testthat::expect_error(
    FilterPanelAPI$new(iris),
    "Must inherit from class 'FilteredData', but has class 'data.frame'."
  )
})

testthat::test_that("FilterPanelAPI$set_filter_state sets filters specified by the named list",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)

    filter_list <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      )
    )
    isolate(datasets$set_filter_state(filter_list))
    testthat::expect_equal(
      isolate(datasets$get_filter_state()),
      isolate(filtered_data$get_filter_state())
    )
  }
)

testthat::test_that("FilterPanelAPI$get_filter_state returns list identical to input without attribute",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)

    filter_list <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      ),
      mtcars = list(
        hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      )
    )
    isolate(datasets$set_filter_state(filter_list))
    fs_wo_attr <- isolate(datasets$get_filter_state())
    attr(fs_wo_attr, "formatted") <- NULL

    testthat::expect_equal(
      fs_wo_attr,
      filter_list
    )
  }
)

testthat::test_that("FilterPanelAPI$remove_filter_state removes filter states defined in the list", {
  datasets <- FilterPanelAPI$new(filtered_data)
  filter_list <- list(
    iris = list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    ),
    mtcars = list(
      hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
    )
  )
  isolate(datasets$set_filter_state(filter_list))
  isolate(datasets$remove_filter_state(filter = list(iris = "Sepal.Length")))
  fs_wo_attr <- isolate(datasets$get_filter_state())
  attr(fs_wo_attr, "formatted") <- NULL

  testthat::expect_identical(
    fs_wo_attr,
    list(
      iris = list(Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)),
      mtcars = list(hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE))
    )
  )
})

testthat::test_that(
  "FilterPanelAPI$remove_all_filter_states removes all filters of datasets in FilterPanelAPI",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)
    filter_list <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      ),
      mtcars = list(
        hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      )
    )
    isolate(datasets$set_filter_state(filter_list))
    isolate(datasets$remove_all_filter_states())

    testthat::expect_equal(
      length(isolate(datasets$get_filter_state())),
      0
    )
  }
)

testthat::test_that(
  "FilterPanelAPI$remove_all_filter_states remove the filters of the desired dataset only",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)
    filter_list <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      )
    )
    isolate(datasets$set_filter_state(filter_list))
    isolate(datasets$remove_all_filter_states(datanames = "iris"))
    fs_wo_attr <- isolate(datasets$get_filter_state())
    attr(fs_wo_attr, "formatted") <- NULL

    testthat::expect_equal(
      fs_wo_attr,
      list(mtcars = list(
        hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      ))
    )
  }
)


testthat::test_that("filter_panel_api neutral when filter panel is disabled", {
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      filtered_data <- teal.slice:::init_filtered_data(
        list(iris = list(dataset = iris), mtcars = list(dataset = mtcars))
      )
      filtered_data$filter_panel_disable()
      fs <- FilterPanelAPI$new(filtered_data)
      filter_list <- list(
        iris = list(
          Sepal.Length = list(c(5.1, 6.4)),
          Species = c("setosa", "versicolor")
        ),
        mtcars = list(
          hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
        )
      )
      testthat::expect_warning(fs$set_filter_state(filter_list))
      testthat::expect_warning(fs$remove_all_filter_states(datanames = "iris"))
      fs_wo_attr <- isolate(fs$get_filter_state())
      attr(fs_wo_attr, "formatted") <- NULL
      names(fs_wo_attr) <- NULL

      testthat::expect_equal(
        fs_wo_attr,
        list()
      )
    }
  )
})


testthat::test_that("filter_panel_api under disable/enable filter panel", {
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      filtered_data <- teal.slice:::init_filtered_data(
        list(iris = list(dataset = iris), mtcars = list(dataset = mtcars))
      )
      filtered_data$filter_panel_disable()
      fs <- FilterPanelAPI$new(filtered_data)
      filter_list <- list(
        iris = list(
          Sepal.Length = list(c(5.1, 6.4)),
          Species = c("setosa", "versicolor")
        ),
        mtcars = list(
          hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
        )
      )
      testthat::expect_warning(fs$set_filter_state(filter_list))
      testthat::expect_warning(fs$remove_all_filter_states(datanames = "iris"))
      filtered_data$filter_panel_enable()
      fs$set_filter_state(filter_list)
      fs$remove_all_filter_states(datanames = "iris")
      fs_wo_attr <- isolate(fs$get_filter_state())
      attr(fs_wo_attr, "formatted") <- NULL

      testthat::expect_equal(
        fs_wo_attr,
        list(mtcars = list(
          hp = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
        ))
      )
    }
  )
})
