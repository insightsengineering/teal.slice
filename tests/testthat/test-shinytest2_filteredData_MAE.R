app <- function(name = "filteredData_MAE", variant = paste0("app_driver_", name)) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("MultiAssayExperiment")

  data(miniACC, package = "MultiAssayExperiment")
  fs <- teal_slices(
    teal_slice(
      dataname = "MAE", varname = "years_to_birth", selected = c(30, 50),
      keep_na = TRUE, keep_inf = FALSE
    ),
    teal_slice(
      dataname = "MAE", varname = "vital_status", selected = "1",
      keep_na = FALSE
    ),
    teal_slice(
      dataname = "MAE", varname = "gender", selected = "female",
      keep_na = TRUE
    ),
    teal_slice(
      dataname = "MAE", varname = "ARRAY_TYPE", selected = "",
      keep_na = TRUE, experiment = "RPPAArray", arg = "subset"
    )
  )

  # create a FilteredData object
  datasets <- init_filtered_data(list(MAE = miniACC))
  datasets$set_filter_state(state = fs)

  ui <- fluidPage(
    fluidRow(
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

testthat::test_that("filteredData initializes", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-overview-main_filter_accordion"))
  testthat::expect_equal(app_driver$get_text("#filter_panel-overview-table > table > tbody > tr:nth-child(1) > td:nth-child(1)"),
                         "MAE")
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-dataset_filter_accordion"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-dataset_filter_accordion"))

})

testthat::test_that("filteredData removing filters", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-filter-4_cyl"))
  app_driver$click("filter_panel-active-mtcars-filter-4_cyl-remove")
  app_driver$wait_for_idle(timeout = default_idle_timeout, duration = default_idle_duration)
  testthat::expect_true(is.null(is_visible(app_driver, "#filter_panel-active-mtcars-filter-4_cyl")))
  app_driver$stop()
})

testthat::test_that("filterdData minimize one filter", {
  app_driver <- app()

  testthat::expect_true(
    app_driver$get_js(
      paste0(
        "document.getElementById('filter_panel-active-mtcars-filter-mtcars_mpg-body')",
        ".classList.contains('collapse')"
      )
    )
  )

  app_driver$click(selector = "#filter_panel-active-mtcars-filter-mtcars_mpg .filter-card-header")
  testthat::expect_false(
    app_driver$get_js(
      paste0(
        "document.getElementById('filter_panel-active-mtcars-filter-mtcars_mpg-body')",
        ".classList.contains('collapse')"
      )
    )
  )

  app_driver$stop()
})

testthat::test_that("filterData toggle visibility of Active Filter Summary", {
  app_driver <- app()
  element <- ".filter-panel > .teal-slice:nth-of-type(1) .accordion-button"

  table_selector <- "thead td:nth-child(1) , tr+ tr td:nth-child(1)"
  testthat::expect_true(is_visible(app_driver, table_selector))

  app_driver$set_inputs(`filter_panel-overview-main_filter_accordion` = character(0))

  testthat::expect_false(is_visible(app_driver, element))

  testthat::expect_equal(is_expanded(app_driver, element), "true")
  app_driver$set_inputs(`filter_panel-overview-main_filter_accordion` = "Active Filter Summary")

  testthat::expect_true(is_visible(app_driver, element_id))
  testthat::expect_equal(app_driver$get_text(
    paste0("#filter_panel-overview-main_filter_accordion > div >",
           " div.accordion-header > button > div.accordion-title")),
    "Active Filter Summary")
  #filter_panel-overview-main_filter_accordion > div > div.accordion-header > button > div.accordion-title
  app_driver$stop()
})

# testthat::test_that("filterData toggle visibility of Filter Data", {
#   app_driver <- app()
#   testthat::expect_true(is_visible(app_driver, "#bslib-accordion-panel-2640"))
#   app_driver$set_inputs(`filter_panel-active-main_filter_accordion` = character(0))
#   testthat::expect_false(is_visible(app_driver, "#bslib-accordion-panel-2640"))
#
#   app_driver$set_inputs(`filter_panel-active-main_filter_accordion` = "Filter Data")
#
#   testthat::expect_true(is_visible(app_driver, "#bslib-accordion-panel-2640"))
#   testthat::expect_equal(
#     app_driver$get_text(paset0("#filter_panel-active-main_filter_accordion > div > ",
#                                "div.accordion-header > button > div.accordion-title")),
#     "Filter Data")
#
#   app_driver$stop()
# })

testthat::test_that("filterData toggle visibility of filters for a dataset", {
  app_driver <- app()

  element_id <- "#filter_panel-active-MAE-filter-MAE_subjects-body"
  before <- app_driver$get_js(paste0("document.querySelector('", element_id, "');"))
  before2 <- app_driver$get_js(element_class_shown("#bslib-accordion-panel-2150"))

  testthat::expect_equal(before, "collapse out")
  testthat::expect_true(before2, NULL)

  # Click to the right place
  app_driver$click(selector = "#MAE .accordion-button:nth-child(1)")

  shown <- app_driver$get_js(element_class_shown("#bslib-accordion-panel-2150"))
  testthat::expect_false(after)
  app_driver$stop()
})

testthat::test_that("Remove filters from a dataset", {
  app_driver <- app()
  id_filters <- ".filter-card-header , .filter-card-varlabel , .filter-card-summary-value"
  testthat::expect_true(is_visible(app_driver, id_filters))
  app_driver$click("filter_panel-active-MAE-remove_filters")
  testthat::expect_false(is_visible(app_driver, id_filters))
  app_driver$stop()
})

testthat::test_that("Remove filters from all datasets", {
  app_driver <- app()
  id_filters <- ".filter-card-header , .filter-card-varlabel , .filter-card-summary-value"
  testthat::expect_true(is_visible(app_driver, id_filters))
  app_driver$click("filter_panel-active-remove_all_filters")
  testthat::expect_false(is_visible(app_driver, id_filters))
  app_driver$stop()
})

