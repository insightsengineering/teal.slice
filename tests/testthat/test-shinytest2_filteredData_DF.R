local_app_driver <- function(name = "filteredData",
                             variant = sprintf("app_driver_%s", name),
                             envir = parent.frame()) {
  testthat::skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)

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
          tabPanel(title = "iris", DT::DTOutput("iris_table")),
          tabPanel(title = "mtcars", DT::DTOutput("mtcars_table"))
        )
      ),
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

    # get the filtered datasets and put them inside reactives for analysis
    iris_filtered_data <- reactive(datasets$get_data(dataname = "iris", filtered = TRUE))
    mtcars_filtered_data <- reactive(datasets$get_data(dataname = "mtcars", filtered = TRUE))

    output$iris_table <- DT::renderDataTable(iris_filtered_data())
    output$mtcars_table <- DT::renderDataTable(mtcars_filtered_data())
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

testthat::describe("App has content for", {
  it("'Active Filter Summary'", {
    app_driver <- local_app_driver()

    testthat::expect_equal(
      app_driver$get_text(
        paste(
          "#filter_panel-overview-main_filter_accordion > div >",
          "div.accordion-header > button > div.accordion-title"
        )
      ),
      "Active Filter Summary"
    )

    table <- "#filter_panel-overview-table > table"
    text <- app_driver$get_text(table)

    testthat::expect_equal(
      clean_text(text),
      c("Data Name", "Obs", "iris", "50/150", "mtcars", "5/32")
    )
  })

  it("'Filter Data'", {
    app_driver <- local_app_driver()
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

    text <- app_driver$get_text("#filter_panel-active-iris-filter-iris_Species-inputs-selection")
    testthat::expect_equal(
      clean_text(text),
      c("setosa (50/50)", "versicolor (50/50)", "virginica (50/50)")
    )
  })
})

testthat::describe("Clicking toggle buttons show and hide (by removing the show class)", {
  it("'Active Filter Summary'", {
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
  })

  it("'Filter Data'", {
    app_driver <- local_app_driver()

    selector_collapsable <- get_class(".accordion-item:nth-of-type(1) > div.accordion-collapse")
    out <- app_driver$get_js(selector_collapsable)
    testthat::expect_length(out, 4L)
    testthat::expect_true("show" %in% unlist(out[[2L]]))
  })

  it("filters for a dataset", {
    app_driver <- local_app_driver()

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
  })
})

testthat::describe("Clicking remove buttons removes the html element of associated filter-state-card", {
  ns <- function(dataset, id) shiny::NS(sprintf("filter_panel-active-%s", dataset), id)
  id_ns <- function(dataset, id) sprintf("#%s", ns(dataset, id))

  it("one filter", {
    app_driver <- local_app_driver()
    testthat::expect_true(is_existing(app_driver, id_ns("mtcars", "filter-4_cyl")))
    app_driver$click(ns("mtcars", "filter-4_cyl-remove"))
    app_driver$wait_for_idle()
    testthat::expect_false(is_existing(app_driver, id_ns("mtcars", "filter-4_cyl")))
  })

  it("filters from a dataset", {
    app_driver <- local_app_driver()
    testthat::expect_true(is_existing(app_driver, id_ns("iris", "filter-iris_Species")))
    testthat::expect_true(is_existing(app_driver, id_ns("mtcars", "filter-4_cyl")))
    testthat::expect_true(is_existing(app_driver, id_ns("mtcars", "filter-mtcars_mpg")))
    app_driver$click(ns("mtcars", "remove_filters"))
    app_driver$wait_for_idle()
    testthat::expect_true(is_existing(app_driver, id_ns("iris", "filter-iris_Species")))
    testthat::expect_false(is_existing(app_driver, id_ns("mtcars", "filter-4_cyl")))
    testthat::expect_false(is_existing(app_driver, id_ns("mtcars", "filter-mtcars_mpg")))
  })

  it("filters from all datasets", {
    app_driver <- local_app_driver()
    testthat::expect_true(is_existing(app_driver, id_ns("iris", "filter-iris_Species")))
    testthat::expect_true(is_existing(app_driver, id_ns("mtcars", "filter-4_cyl")))
    testthat::expect_true(is_existing(app_driver, id_ns("mtcars", "filter-mtcars_mpg")))
    app_driver$click("filter_panel-active-remove_all_filters")
    app_driver$wait_for_idle()
    testthat::expect_false(is_existing(app_driver, id_ns("iris", "filter-iris_Species")))
    testthat::expect_false(is_existing(app_driver, id_ns("mtcars", "filter-4_cyl")))
    testthat::expect_false(is_existing(app_driver, id_ns("mtcars", "filter-mtcars_mpg")))
  })
})

testthat::describe("exclude_varnames", {
  it("are dropped from the possible filter variable selection dropdown", {
    app_driver <- local_app_driver()
    testthat::expect_false(is_existing(app_driver, "#filter_panel-active-mtcars-mtcars-filter-var_to_add > option"))
    app_driver$click(selector = "#filter_panel-active-mtcars-add_filter_icon")
    app_driver$wait_for_idle(duration = default_idle_duration * 4) # Wait for the panel open animation
    testthat::expect_true(is_existing(app_driver, "#filter_panel-active-mtcars-mtcars-filter-var_to_add > option"))
    text <- app_driver$get_text("#filter_panel-active-mtcars-mtcars-filter-var_to_add > option")
    testthat::expect_false("cyl" %in% text)
  })
})

testthat::test_that("Add one filter", {
  app_driver <- local_app_driver()

  # Click to add filter
  app_driver$click(selector = "#filter_panel-active-iris-add_filter_icon")
  app_driver$wait_for_idle(duration = default_idle_duration * 4) # Wait for the panel open animation
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-add_panel"))

  # Select variable
  testthat::expect_no_error(app_driver$set_inputs(`filter_panel-active-iris-iris-filter-var_to_add` = "Sepal.Length"))
  app_driver$wait_for_idle(duration = default_idle_duration * 4) # Wait for the panel to be added

  # Check output
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-iris-filter-iris_Sepal_Length"))
  element <- "#filter_panel-active-iris-filter-iris_Sepal_Length * div.filter-card-varname"
  text <- app_driver$get_text(element)
  testthat::expect_equal(trimws(text), "Sepal.Length")
})
