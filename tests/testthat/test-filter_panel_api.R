df1 <- data.frame(
  int = 1:100,
  num = 1:100 / 10,
  fact = sample(rep_len(letters, 100)),
  stringsAsFactors = TRUE
)
df2 <- data.frame(
  int = 1:100,
  num = 1:100 / 100,
  fact = sample(rep_len(letters, 100)),
  stringsAsFactors = TRUE
)

filtered_data <- teal.slice:::init_filtered_data(list(df1 = list(dataset = df1), df2 = list(dataset = df2)))

testthat::test_that("FilterPanelAPI constructor accepts a FilteredData object", {
  testthat::expect_no_error(FilterPanelAPI$new(filtered_data))
})

testthat::test_that("FilterPanelAPI constructor throws error with a non FilteredData object", {
  testthat::expect_error(
    FilterPanelAPI$new(list(df1 = list(dataset = df1))),
    "Must inherit from class 'FilteredData', but has class 'list'."
  )

  testthat::expect_error(
    FilterPanelAPI$new(df1),
    "Must inherit from class 'FilteredData', but has class 'data.frame'."
  )
})

testthat::test_that("FilterPanelAPI$set_filter_state sets filters specified by the named list",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)

    filter_list <- list(
      df1 = list(
        num = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        fact = list(selected = c("a", "b"), keep_na = FALSE)
      )
    )
    shiny::isolate(datasets$set_filter_state(filter_list))
    testthat::expect_equal(
      shiny::isolate(datasets$get_filter_state()),
      shiny::isolate(filtered_data$get_filter_state())
    )
  }
)

testthat::test_that("FilterPanelAPI$get_filter_state returns list identical to input without attribute",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)

    filter_list <- list(
      df1 = list(
        num = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        fact = list(selected = c("a", "b"), keep_na = FALSE)
      ),
      df2 = list(
        int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      )
    )
    shiny::isolate(datasets$set_filter_state(filter_list))
    fs_wo_attr <- shiny::isolate(datasets$get_filter_state())
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
    df1 = list(
      num = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      fact = list(selected = c("a", "b"), keep_na = FALSE)
    ),
    df2 = list(
      int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
    )
  )
  shiny::isolate(datasets$set_filter_state(filter_list))
  shiny::isolate(datasets$remove_filter_state(filter = list(df1 = "num")))
  fs_wo_attr <- shiny::isolate(datasets$get_filter_state())
  attr(fs_wo_attr, "formatted") <- NULL

  testthat::expect_equal(
    fs_wo_attr,
    list(
      df1 = list(fact = list(selected = c("a", "b"), keep_na = FALSE)),
      df2 = list(int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE))
    )
  )
})

testthat::test_that(
  "FilterPanelAPI$remove_all_filter_states removes all filters of datasets in FilterPanelAPI",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)
    filter_list <- list(
      df1 = list(
        num = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
        fact = list(selected = c("a", "b"), keep_na = FALSE)
      ),
      df2 = list(
        int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      )
    )
    shiny::isolate(datasets$set_filter_state(filter_list))
    shiny::isolate(datasets$remove_all_filter_states())

    testthat::expect_equal(
      length(shiny::isolate(datasets$get_filter_state())),
      0
    )
  }
)

testthat::test_that(
  "FilterPanelAPI$remove_all_filter_states remove the filters of the desired dataset only",
  code = {
    datasets <- FilterPanelAPI$new(filtered_data)
    filter_list <- list(
      df1 = list(
        num = list(c(5.1, 6.4)),
        fact = c("a", "b")
      ),
      df2 = list(
        int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      )
    )
    shiny::isolate(datasets$set_filter_state(filter_list))
    shiny::isolate(datasets$remove_all_filter_states(datanames = "df1"))
    fs_wo_attr <- shiny::isolate(datasets$get_filter_state())
    attr(fs_wo_attr, "formatted") <- NULL

    testthat::expect_equal(
      fs_wo_attr,
      list(df2 = list(
        int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
      ))
    )
  }
)


testthat::test_that("filter_panel_api neutral when filter panel is disabled", {
  shiny::testServer(
    filtered_data$srv_filter_panel,
    expr = {
      filtered_data <- teal.slice:::init_filtered_data(
        list(df1 = list(dataset = df1), df2 = list(dataset = df2))
      )
      filtered_data$filter_panel_disable()
      fs <- FilterPanelAPI$new(filtered_data)
      filter_list <- list(
        df1 = list(
          Sepal.Length = list(c(5.1, 6.4)),
          Species = c("setosa", "versicolor")
        ),
        df2 = list(
          int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
        )
      )
      testthat::expect_warning(fs$set_filter_state(filter_list))
      testthat::expect_warning(fs$remove_all_filter_states(datanames = "df1"))
      fs_wo_attr <- shiny::isolate(fs$get_filter_state())
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
        list(df1 = list(dataset = df1), df2 = list(dataset = df2))
      )
      filtered_data$filter_panel_disable()
      fs <- FilterPanelAPI$new(filtered_data)
      filter_list <- list(
        df1 = list(
          num = list(c(5.1, 6.4)),
          fact = c("a", "b")
        ),
        df2 = list(
          int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
        )
      )
      testthat::expect_warning(fs$set_filter_state(filter_list))
      testthat::expect_warning(fs$remove_all_filter_states(datanames = "df1"))
      filtered_data$filter_panel_enable()
      fs$set_filter_state(filter_list)
      fs$remove_all_filter_states(datanames = "df1")
      fs_wo_attr <- shiny::isolate(fs$get_filter_state())
      attr(fs_wo_attr, "formatted") <- NULL

      testthat::expect_equal(
        fs_wo_attr,
        list(df2 = list(
          int = list(selected = c(52, 65), keep_na = FALSE, keep_inf = FALSE)
        ))
      )
    }
  )
})
