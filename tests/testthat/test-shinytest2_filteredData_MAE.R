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

testthat::test_that("initializes", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-overview-main_filter_accordion"))
  testthat::expect_equal(app_driver$get_text("#filter_panel-overview-table > table > tbody > tr:nth-child(1) > td:nth-child(1)"),
                         "MAE")
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-MAE-dataset_filter_accordion"))

})
testthat::test_that("Toggle visibility of Active Filter Summary", {
  app_driver <- app()

  selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 3L)
  testthat::expect_true("show" %in% unlist(out[[1]]))

  selector <- ".filter-panel > .teal-slice:nth-of-type(1) .accordion-button"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)

  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 3L)
  testthat::expect_true(!"show" %in% unlist(out[[1]]))
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
  testthat::expect_length(out, 3L)
  testthat::expect_true("show" %in% unlist(out[[2]]))

  selector <- ".filter-panel > #filter_panel-active.teal-slice > div > div > div > button"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)

  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 3L)
  testthat::expect_true(!"show" %in% unlist(out[[2]]))
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
  testthat::expect_length(out, 3L)
  testthat::expect_true("show" %in% unlist(out[[3L]]))

  selector <- "#MAE * button"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)

  out <- app_driver$get_js(selector_collapsable)
  testthat::expect_length(out, 3L)
  testthat::expect_true(!"show" %in% unlist(out[[3L]]))
  app_driver$stop()
})

testthat::test_that("Toggle visibility of plot inside filter", {
  app_driver <- app()
  body_filter <- get_class("#filter_panel-active-MAE-subjects-MAE_years_to_birth-body")
  class_before <- app_driver$get_js(body_filter)
  testthat::expect_true(!"show" %in% unlist(class_before))

  selector <- "#filter_panel-active-MAE-subjects-MAE_years_to_birth > div.filter-card-header"
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(duration = default_idle_duration*2,
                           timeout = default_idle_timeout)
  out <- app_driver$get_js(body_filter)
  testthat::expect_true("show" %in% unlist(out))
  app_driver$stop()
})

testthat::test_that("Remove one filter", {
  app_driver <- app()
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-MAE-subjects-MAE_years_to_birth"))
  app_driver$click("filter_panel-active-MAE-subjects-MAE_years_to_birth-remove")
  app_driver$wait_for_idle(timeout = default_idle_timeout, duration = default_idle_duration)
  testthat::expect_false(is_visible(app_driver, "#filter_panel-active-MAE-subjects-MAE_years_to_birth"))
  app_driver$stop()
})


testthat::test_that("Remove filters from a dataset", {
  app_driver <- app()
  id_filters <- "#filter_panel-active-MAE-subjects-MAE_years_to_birth"
  testthat::expect_true(is_visible(app_driver, id_filters))
  app_driver$click("filter_panel-active-MAE-remove_filters")
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)
  testthat::expect_false(is_visible(app_driver, id_filters))
  app_driver$stop()
})

testthat::test_that("Remove filters from all datasets", {
  app_driver <- app()
  id_filter_MAE <- "#filter_panel-active-MAE-subjects-MAE_years_to_birth"
  testthat::expect_true(is_visible(app_driver, id_filter_MAE))
  app_driver$click("filter_panel-active-remove_all_filters")
  app_driver$wait_for_idle(duration = default_idle_duration,
                           timeout = default_idle_timeout)
  testthat::expect_false(is_visible(app_driver, id_filter_MAE))
  app_driver$stop()
})
