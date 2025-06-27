app <- function(name = "filteredData", variant = paste0("app_driver_", name)) {
  testthat::skip_if_not_installed("shinytest2")

  # create a FilteredData object
  datasets <- init_filtered_data(list(iris = iris, mtcars = mtcars))

  # setting initial state
  set_filter_state(
    datasets = datasets,
    filter = teal_slices(
      teal_slice(dataname = "iris", varname = "Species", selected = "virginica", keep_na = FALSE),
      teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
      teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0), keep_na = FALSE, keep_inf = FALSE),
      include_varnames = list(iris = c("Species", "Sepal.Length")),
      exclude_varnames = list(mtcars = "cyl"),
      count_type = "all",
      allow_add = TRUE
    )
  )

  ui <- fluidPage(
    fluidRow(
      column(
        width = 9,
        tabsetPanel(
          tabPanel(title = "iris", dataTableOutput("iris_table")),
          tabPanel(title = "mtcars", dataTableOutput("mtcars_table"))
        )
      ),
      # ui for the filter panel
      column(width = 3,
                    # What we want to test:
                    datasets$ui_filter_panel("filter_panel"))
    )
  )

  server <- function(input, output, session) {
    # this is the shiny server function for the filter panel and the datasets
    # object can now be used inside the application
    datasets$srv_filter_panel("filter_panel")

    # get the filtered datasets and put them inside reactives for analysis
    iris_filtered_data <- reactive(datasets$get_data(dataname = "iris", filtered = TRUE))
    mtcars_filtered_data <- reactive(datasets$get_data(dataname = "mtcars", filtered = TRUE))

    output$iris_table <- renderDataTable(iris_filtered_data())
    output$mtcars_table <- renderDataTable(mtcars_filtered_data())
  }

  app <- shinyApp(ui, server)
  app_driver <- shinytest2::AppDriver$new(
    app,
    name = name,
    variant = variant,
    timeout = default_idle_timeout,
    load_timeout = default_idle_timeout,
    wait = TRUE,
    seed = 20250626
  )
  app_driver
}

testthat::test_that("Initializes visible filters for DF", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-overview-main_filter_accordion"))
  testthat::expect_equal(app_driver$get_text("#filter_panel-overview-table > table > tbody > tr:nth-child(1) > td:nth-child(1)"),
                         "iris")
  testthat::expect_equal(app_driver$get_text("#filter_panel-overview-table > table > tbody > tr:nth-child(2) > td:nth-child(1)"),
                         "mtcars")

  testthat::expect_true(is_visible(app_driver, "#filter_panel-active"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-dataset_filter_accordion"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-dataset_filter_accordion"))
  app_driver$stop()
})

testthat::test_that("Toggle visibility of Active Filter Summary", {
  app_driver <- app()

  selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 4L)
  testthat::expect_true("show" %in% unlist(out[[1L]]))

  selector <- ".filter-panel > .teal-slice:nth-of-type(1) .accordion-button"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)

  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 4L)
  testthat::expect_false("show" %in% unlist(out[[1L]]))
  testthat::expect_equal(app_driver$get_text(
    paste0("#filter_panel-overview-main_filter_accordion > div >",
           " div.accordion-header > button > div.accordion-title")),
    "Active Filter Summary")
  app_driver$stop()
})

testthat::test_that("Toggle visibility of Filter Data", {
  app_driver <- app()

  selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 4L)
  testthat::expect_true("show" %in% unlist(out[[2L]]))

  selector <- ".filter-panel > #filter_panel-active.teal-slice > div > div > div > button"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)

  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 4L)
  testthat::expect_false("show" %in% unlist(out[[2L]]))
  testthat::expect_equal(
    app_driver$get_text(paste0("#filter_panel-active-main_filter_accordion > div > ",
                               "div.accordion-header > button > div.accordion-title")),
    "Filter Data")

  app_driver$stop()
})

testthat::test_that("Toggle visibility of filters for a dataset", {
  app_driver <- app()

  selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 4L)
  testthat::expect_true("show" %in% unlist(out[[3L]]))

  selector <- "#iris * button"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle()

  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 4L)
  testthat::expect_false("show" %in% unlist(out[[3L]]))
  app_driver$stop()
})

testthat::test_that("Remove one filter", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-filter-4_cyl"))
  app_driver$click("filter_panel-active-mtcars-filter-4_cyl-remove")
  app_driver$wait_for_idle(timeout = default_idle_timeout, duration = default_idle_duration)
  testthat::expect_false(is_visible(app_driver, "#filter_panel-active-mtcars-filter-4_cyl"))
  app_driver$stop()
})


testthat::test_that("Remove filters from a dataset", {
  app_driver <- app()
  id_filters <- "#filter_panel-active-iris-filter-iris_Species-summary-summary > span.filter-card-summary-value"
  testthat::expect_true(is_visible(app_driver, id_filters))
  app_driver$click("filter_panel-active-iris-remove_filters")
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)
  testthat::expect_false(is_visible(app_driver, id_filters))
  app_driver$stop()
})

testthat::test_that("Remove filters from all datasets", {
  app_driver <- app()
  id_filters_iris <- "#filter_panel-active-iris-filter-iris_Species-summary-summary > span.filter-card-summary-value"
  id_filters_mtcars <- "#filter_panel-active-mtcars-filter-4_cyl"
  testthat::expect_true(is_visible(app_driver, id_filters_iris))
  testthat::expect_true(is_visible(app_driver, id_filters_mtcars))
  app_driver$click("filter_panel-active-remove_all_filters")
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)
  testthat::expect_false(is_visible(app_driver, id_filters_iris))
  testthat::expect_false(is_visible(app_driver, id_filters_mtcars))
  app_driver$stop()
})
