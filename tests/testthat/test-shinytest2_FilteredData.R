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
    wait = TRUE
  )
  app_driver
}

test_that("filteredData initializes", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-overview-main_filter_accordian"))
  testthat::expect_equal(app_driver$get_text("#filter_panel-overview-table > table > tbody > tr:nth-child(1) > td:nth-child(1)"),
                         "iris")
  testthat::expect_equal(app_driver$get_text("#filter_panel-overview-table > table > tbody > tr:nth-child(2) > td:nth-child(1)"),
                         "mtcars")

  testthat::expect_true(is_visible(app_driver, "#filter_panel-active"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-dataset_filter_accordian"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-dataset_filter_accordian"))

})

test_that("filteredData removing filters", {
  app_driver <- app()
  expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-filter-4_cyl"))
  app_driver$click("filter_panel-active-mtcars-filter-4_cyl-remove")
  app_driver$wait_for_idle(timeout = default_idle_timeout, duration = default_idle_duration)
  expect_false(is_visible(app_driver, "#filter_panel-active-mtcars-filter-4_cyl"))
  app_driver$stop()
})

test_that("filterdData minimize one filters", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-filter-mtcars_mpg"))
  app_driver$click("filter_panel-active-mtcars-filter-mtcars_mpg-back")
  app_driver$wait_for_idle(timeout = default_idle_timeout, duration = default_idle_duration)
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-filter-mtcars_mpg"))
  app_driver$stop()
})

test_that("filterData toggle visibility of Active Filter Summary", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#bslib-accordion-panel-4630"))
  app_driver$set_inputs(`filter_panel-overview-main_filter_accordian` = character(0))
  testthat::expect_false(is_visible(app_driver, "#bslib-accordion-panel-4630"))
  app_driver$set_inputs(`filter_panel-overview-main_filter_accordian` = "Active Filter Summary")
  testthat::expect_true(is_visible(app_driver, "#bslib-accordion-panel-4630"))
  testthat::expect_equal(app_driver$get_text(
    paste0("#filter_panel-overview-main_filter_accordian > div >",
           " div.accordion-header > button > div.accordion-title")),
    "Active Filter Summary")
  app_driver$stop()
})

test_that("filterData toggle visibility of Filter Data", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#bslib-accordion-panel-2640"))
  app_driver$set_inputs(`filter_panel-active-main_filter_accordian` = character(0))
  testthat::expect_false(is_visible(app_driver, "#bslib-accordion-panel-2640"))
  app_driver$set_inputs(`filter_panel-active-main_filter_accordian` = "Filter Data")
  testthat::expect_true(is_visible(app_driver, "#bslib-accordion-panel-2640"))
  testthat::expect_equal(
    app_driver$get_text(paset0("#filter_panel-active-main_filter_accordian > div > ",
                               "div.accordion-header > button > div.accordion-title")),
    "Filter Data")

  app_driver$stop()
})

test_that("filterData toggle visibility of filters for a dataset", {
  app_driver <- app()

  element_id <- "filter_panel-active-iris-filter-iris_Species-body"
  before <- app_driver$get_js(paste0("document.getElementById('", element_id, "').className;"))
  before2 <- app_driver$get_js(element_class_shown("bslib-accordion-panel-2150"))

  testthat::expect_equal(before, "collapse out")
  testthat::expect_true(before2, NULL)

  # Click to the right place
  # FIXME: click on the right id to toggle the visibility.
  # app_driver$click("filter_panel-active-mtcars-dataset_filter_accordian")

  shown <- app_driver$get_js(element_class_shown("bslib-accordion-panel-2150"))
  testthat::expect_false(after)
  app_driver$stop()
})

test_that("Remove filters from a dataset", {
  app_driver <- app()
  id_filters <- "#filter_panel-active-iris-filter-iris_Species-summary-summary > span.filter-card-summary-value"
  testthat::expect_true(is_visible(app_driver, id_filters))
  app_driver$click("filter_panel-active-iris-remove_filters")
  testthat::expect_false(is_visible(app_driver, id_filters))
  app_driver$stop()
})

test_that("Remove filters from all datasets", {
  app_driver <- app()
  id_filters_iris <- "#filter_panel-active-iris-filter-iris_Species-summary-summary > span.filter-card-summary-value"
  id_filters_mtcars <- "#filter_panel-active-mtcars-filter-4_cyl"
  testthat::expect_true(is_visible(app_driver, id_filters_iris))
  testthat::expect_true(is_visible(app_driver, id_filters_mtcars))
  app_driver$click("filter_panel-active-remove_all_filters")
  testthat::expect_false(is_visible(app_driver, id_filters_iris))
  testthat::expect_false(is_visible(app_driver, id_filters_mtcars))
  app_driver$stop()
})

# test_tahat("filterDatatoggle visibility of filters for a dataset", {
#   app_driver <- app()
#   app_driver$click("filter_panel-active-mtcars-filter-mtcars_mpg-reset")
#   app_driver$click("filter_panel-active-mtcars-filter-mtcars_mpg-remove")
#   app_driver$click("filter_panel-active-mtcars-filter-mtcars_mpg-inputs-plotly_info")
  # # Update output value
  # app_driver$set_inputs(`filter_panel-active-iris-filter-iris_Species-shinyjs-delay-b72e323805347c622fab8286f7f318b1` = 100, allow_no_input_binding_ = TRUE)
  # app_driver$set_inputs(`filter_panel-active-iris-filter-iris_Species-shinyjs-delay-c39477bf135d7071b5164ffb266addf8` = 100, allow_no_input_binding_ = TRUE)
  # # Update output value
  # app_driver$set_inputs(`filter_panel-active-mtcars-filter-mtcars_mpg-shinyjs-delay-b4732a9feffeafc9e37c0d5547991c38` = 100, allow_no_input_binding_ = TRUE)
  # app_driver$set_inputs(`filter_panel-active-mtcars-filter-mtcars_mpg-shinyjs-delay-ed744f9988d9bbd1d35226551bd73779` = 100, allow_no_input_binding_ = TRUE)
  # app_driver$expect_values()
  # app_driver$set_inputs(`filter_panel-overview-main_filter_accordian` = character(0))
  # app_driver$set_inputs(`filter_panel-active-main_filter_accordian` = character(0))
  # app_driver$set_inputs(`filter_panel-active-main_filter_accordian` = "Filter Data")
  # # Update output value
  # app_driver$set_inputs(`filter_panel-active-mtcars-mtcars-filter-var_to_add` = character(0))
  # # Update output value
  # app_driver$set_inputs(`filter_panel-overview-main_filter_accordian` = "Active Filter Summary")
  # # Update output value
  # app_driver$set_inputs(mtcars_table_rows_current = 1:5, allow_no_input_binding_ = TRUE)
  # app_driver$set_inputs(mtcars_table_rows_all = 1:5, allow_no_input_binding_ = TRUE)
  # app_driver$set_inputs(
  #   mtcars_table_state = c(
  #     1750239102719, 0, 10, "", TRUE, FALSE, TRUE,
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE),
  #     c(TRUE, "", TRUE, FALSE, TRUE)
  #   ),
  #   allow_no_input_binding_ = TRUE
  # )
#   app_driver$stop()
# })
