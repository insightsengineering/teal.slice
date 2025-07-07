local_app_driver <- function(name = "filteredData_MAE",
                             variant = sprintf("app_driver_%s", name),
                             envir = parent.frame()) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("MultiAssayExperiment")
  skip_if_too_deep(5)

  data(miniACC, package = "MultiAssayExperiment")
  fs <- teal_slices(
    teal_slice(dataname = "iris", varname = "Species", selected = "virginica", keep_na = FALSE),
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
  datasets <- init_filtered_data(list(MAE = miniACC, iris = iris))
  datasets$set_filter_state(state = fs)

  ui <- fluidPage(
    fluidRow(
      # ui for the filter panel
      column(
        width = 3,
        # What we want to test:
        datasets$ui_filter_panel("filter_panel")
      )
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
    load_timeout = default_idle_timeout * 2,
    wait = TRUE,
    seed = 20250626
  )
  withr::defer(app_driver$stop(), envir = envir)
  app_driver$wait_for_idle()
  app_driver
}

testthat::describe("Toggle visibility of", {
  it("Active Filter Summary", {
    app_driver <- local_app_driver()

    selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_true("show" %in% unlist(out[[1L]]))

    selector <- ".filter-panel > .teal-slice:nth-of-type(1) .accordion-button"
    app_driver$click(selector = selector)
    app_driver$wait_for_idle()

    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_false("show" %in% unlist(out[[1L]]))
    testthat::expect_equal(
      app_driver$get_text(
        paste0(
          "#filter_panel-overview-main_filter_accordion > div >",
          " div.accordion-header > button > div.accordion-title"
        )
      ),
      "Active Filter Summary"
    )

    text <- app_driver$get_text("#filter_panel-active-MAE-subjects-MAE_vital_status-body")
    expect_equal(clean_text(text), c("0 (58)", "1 (34)"))
  })

  it("Filter Data", {
    app_driver <- local_app_driver()

    selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_true("show" %in% unlist(out[[2L]]))

    selector <- ".filter-panel > #filter_panel-active.teal-slice > div > div > div > button"
    app_driver$click(selector = selector)
    app_driver$wait_for_idle()

    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_false("show" %in% unlist(out[[2L]]))
    testthat::expect_equal(
      app_driver$get_text(paste0(
        "#filter_panel-active-main_filter_accordion > div > ",
        "div.accordion-header > button > div.accordion-title"
      )),
      "Filter Data"
    )
  })

  it("filters for a dataset", {
    app_driver <- local_app_driver()

    selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_true("show" %in% unlist(out[[3L]]))

    selector <- "#MAE * button"
    app_driver$click(selector = selector)
    app_driver$wait_for_idle()

    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_false("show" %in% unlist(out[[3L]]))
  })

  it("plot inside filter", {
    app_driver <- local_app_driver()
    body_filter <- get_class("#filter_panel-active-MAE-subjects-MAE_years_to_birth-body")
    class_before <- app_driver$get_js(body_filter)
    testthat::expect_false("show" %in% unlist(class_before))

    selector <- "#filter_panel-active-MAE-subjects-MAE_years_to_birth > div.filter-card-header"
    app_driver$click(selector = selector)
    app_driver$wait_for_idle(duration = default_idle_duration * 2)
    out <- app_driver$get_js(body_filter)
    testthat::expect_true("show" %in% unlist(out))
  })
})

testthat::describe("Remove", {
  ns <- shiny::NS("filter_panel-active-MAE")
  id_ns <- shiny::NS(sprintf("#%s", ns(NULL)))

  it("has all filter are visible when app loads", {
    app_driver <- local_app_driver()
    testthat::expect_true(is_visible(app_driver, id_ns("subjects-MAE_years_to_birth")))
    testthat::expect_true(is_visible(app_driver, id_ns("subjects-MAE_vital_status")))
    testthat::expect_true(is_visible(app_driver, id_ns("subjects-MAE_gender")))
    testthat::expect_true(is_visible(app_driver, id_ns("RPPAArray-MAE_ARRAY_TYPE_RPPAArray_subset")))
    testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-filter-iris_Species"))
  })

  it("one filter", {
    app_driver <- local_app_driver()
    app_driver$click(ns("subjects-MAE_years_to_birth-remove"))
    app_driver$wait_for_idle(duration = default_idle_duration * 2)
    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_years_to_birth")))
    testthat::expect_true(is_visible(app_driver, id_ns("subjects-MAE_vital_status")))
  })

  it("all filters from a dataset", {
    app_driver <- local_app_driver()
    app_driver$click("filter_panel-active-MAE-remove_filters")
    app_driver$wait_for_idle(duration = default_idle_duration * 2)

    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_years_to_birth")))
    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_vital_status")))
    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_gender")))
    testthat::expect_false(is_visible(app_driver, id_ns("RPPAArray-MAE_ARRAY_TYPE_RPPAArray_subset")))
    testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-filter-iris_Species"))
  })

  it("all filters from all datasets", {
    app_driver <- local_app_driver()
    app_driver$click("filter_panel-active-remove_all_filters")
    app_driver$wait_for_idle(duration = default_idle_duration * 2)
    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_years_to_birth")))
    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_vital_status")))
    testthat::expect_false(is_visible(app_driver, id_ns("subjects-MAE_gender")))
    testthat::expect_false(is_visible(app_driver, id_ns("RPPAArray-MAE_ARRAY_TYPE_RPPAArray_subset")))
    testthat::expect_false(is_visible(app_driver, "#filter_panel-active-iris-filter-iris_Species"))
  })
})

testthat::test_that("Add one filter", {
  app_driver <- local_app_driver()

  app_driver$click(selector = "#filter_panel-active-MAE-add_filter_icon")
  app_driver$wait_for_idle(duration = default_idle_duration * 4) # Wait for the panel open animation
  expect_true(is_visible(app_driver, "#filter_panel-active-MAE-add_panel"))

  # Select variable
  testthat::expect_no_error(app_driver$set_inputs(`filter_panel-active-MAE-MAE-subjects-var_to_add` = "patientID"))
  app_driver$wait_for_idle(duration = default_idle_duration * 4) # Wait for the panel open animation

  # Check output
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-MAE-subjects-MAE_patientID"))
  element <- "#filter_panel-active-MAE-subjects-MAE_patientID-summary-summary"
  text <- app_driver$get_text(element)
  testthat::expect_true(endsWith(clean_text(text), "levels selected"))
})
