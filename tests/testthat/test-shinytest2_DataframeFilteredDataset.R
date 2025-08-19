# Create app
# ... arguments are passed to teal_slices which is used by set_filter_state
local_app_driver <- function(...,
                             name = "filteredData",
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
      ...,
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

testthat::describe("Toggle button shows and hide", {
  it("'Active Filter Summary' panel", {
    app_driver <- local_app_driver()
    testthat::expect_true(is_visible(app_driver, "#filter_panel-overview-table"))
    app_driver$click(selector = "#filter_panel-overview-main_filter_accordion * button")
    app_driver$wait_for_idle(timeout = default_idle_timeout * 8)
    testthat::expect_false(is_visible(app_driver, "#filter_panel-overview-table"))
  })
  it("'Filter Data' panel", {
    app_driver <- local_app_driver()
    testthat::expect_true(is_visible(app_driver, "#filter_panel-active-filter_active_vars_contents"))
    app_driver$click(selector = "#filter_panel-active-main_filter_accordion > div > div.accordion-header > button")
    app_driver$wait_for_idle(timeout = default_idle_timeout * 8)
    testthat::expect_false(is_visible(app_driver, "#filter_panel-active-filter_active_vars_contents"))
  })
})

testthat::describe("datasets passed to filter data", {
  it("creates Active Filter Summary panel for all of them", {
    app_driver <- local_app_driver()
    selector <- "#filter_panel-active * div.accordion-body * button.accordion-button > div.accordion-title"
    text <- app_driver$get_text(selector)
    testthat::expect_equal(text, c("iris", "mtcars"))
  })
  it("creates Filter Data panel for all of them", {
    app_driver <- local_app_driver()
    table <- "#filter_panel-overview-table > table > tbody * td:first-child"
    text <- app_driver$get_text(table)
    testthat::expect_equal(text, c("iris", "mtcars"))
  })
})

testthat::describe("teal_slice objects pass to filter data", {
  it("displays filter/unfiltered counts in Active Filter Summary", {
    app_driver <- local_app_driver(
      teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
      teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
      teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0))
    )
    table <- "#filter_panel-overview-table > table > tbody * td:nth-child(2)"
    text <- app_driver$get_text(table)
    expect_equal(text, c("50/150", "5/32"))
  })

  it("initializes filter_cards for each teal_slice", {
    app_driver <- local_app_driver(
      teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
      teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
      teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0))
    )
    text <- app_driver$get_text("div.filter-card-varname > strong")
    expect_equal(text, c("Species", "4 cyl", "mpg"))
  })
})

testthat::test_that("Clicking add button on the datasets shows add filter panel", {
  app_driver <- local_app_driver()
  testthat::expect_true(is_existing(app_driver, "#filter_panel-active-mtcars-add_filter_icon"))
  app_driver$click(selector = "#filter_panel-active-mtcars-add_filter_icon")
  app_driver$wait_for_idle(duration = default_idle_duration * 8) # Wait for the panel open animation
  testthat::expect_true(is_existing(app_driver, "#filter_panel-active-mtcars-mtcars-filter-var_to_add > option"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-mtcars-filter-var_to_add > option"))
})

testthat::test_that("Clicking add and selecting a variable adds the card for a given variable", {
  app_driver <- local_app_driver()
  app_driver$click(selector = "#filter_panel-active-mtcars-add_filter_icon")
  app_driver$wait_for_idle(duration = default_idle_duration * 8)
  app_driver$click(selector = "#filter_panel-active-mtcars-mtcars-filter-var_to_add")
  app_driver$wait_for_idle(duration = default_idle_duration * 8)
  app_driver$click(selector = "#filter_panel-active-mtcars-mtcars-filter-var_to_add_input > div > div > button")
  app_driver$wait_for_idle(duration = default_idle_duration * 8)
  app_driver$click(selector = "#bs-select-1-8")
  app_driver$wait_for_idle(duration = default_idle_duration * 8)
  selector <- paste0(
    "#filter_panel-active-mtcars-filter-mtcars_am > div.filter-card-header > div.filter-card-title",
    "> div.filter-card-varname > strong"
  )
  text <- app_driver$get_text(selector = selector)
  testthat::expect_equal(text, "am")
})

testthat::test_that("include_varnames limits choices in add dropdown", {
  iris_vars <- c("Species", "Sepal.Length")
  app_driver <- local_app_driver(
    # FIXME: If no filter is set it is not possible to know the list of options
    teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
    include_varnames = list(iris = iris_vars)
  )

  app_driver$click(selector = "#filter_panel-active-iris-add_filter_icon")
  app_driver$wait_for_idle(duration = default_idle_duration * 8)

  options <- app_driver$get_text("#filter_panel-active-iris-iris-filter-var_to_add > option")
  testthat::expect_equal(iris_vars, options)
})

testthat::test_that("exclude_varnames limits choices in add dropdown", {
  iris_vars <- c("Species", "Sepal.Length")
  app_driver <- local_app_driver(
    # FIXME: If no filter is set it is not possible to know the list of options
    teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
    exclude_varnames = list(iris = iris_vars)
  )

  app_driver$click(selector = "#filter_panel-active-iris-add_filter_icon")
  app_driver$wait_for_idle(duration = default_idle_duration * 8)

  options <- app_driver$get_text(selector = "#filter_panel-active-iris-iris-filter-var_to_add > option")
  testthat::expect_false(any(iris_vars %in% options))
})

testthat::test_that("Remove filter button removes a specific filter card", {
  app_driver <- local_app_driver(
    teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
    teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
    teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0))
  )
  selector <- "#filter_panel-active-mtcars-filter-mtcars_mpg-remove"
  testthat::expect_true(is_visible(app_driver, selector))
  filters_before <- app_driver$get_text("div.filter-card-varname > strong")
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(default_idle_duration * 8)

  filters_after <- app_driver$get_text("div.filter-card-varname > strong")
  testthat::expect_equal(setdiff(filters_before, filters_after), "mpg")
})

testthat::test_that("Remove datasets filters removes all cards.", {
  app_driver <- local_app_driver(
    teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
    teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
    teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0))
  )

  selector <- "#filter_panel-active-mtcars-remove_filters"
  testthat::expect_true(is_visible(app_driver, selector))
  filters_before <- app_driver$get_text("div.filter-card-varname > strong")
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(default_idle_duration * 8)
  filters_after <- app_driver$get_text("div.filter-card-varname > strong")
  testthat::expect_equal(setdiff(filters_before, filters_after), c("4 cyl", "mpg"))
})

testthat::test_that("Remove all filters button removes all cards for all datasets.", {
  app_driver <- local_app_driver(
    teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
    teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
    teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0))
  )
  selector <- "#filter_panel-active-remove_all_filters"
  testthat::expect_true(is_visible(app_driver, selector))
  app_driver$click(selector = selector)
  app_driver$wait_for_idle(default_idle_duration * 8)
  filters_after <- app_driver$get_text("div.filter-card-varname > strong")
  testthat::expect_true(is.null(filters_after))
})

testthat::test_that("Expanding a card shows filter choices.", {
  app_driver <- local_app_driver(
    teal_slice(dataname = "mtcars", id = "4 cyl", title = "4 Cylinders", expr = "cyl == 4"),
    teal_slice(dataname = "mtcars", varname = "mpg", selected = c(20.0, 25.0))
  )

  select_4_cyl <- "#filter_panel-active-mtcars-filter-4_cyl  > div.filter-card-header"
  app_driver$click(selector = select_4_cyl)
  app_driver$wait_for_idle(default_idle_duration * 8)
  testthat::expect_false(is_existing(app_driver, "#filter_panel-active-mtcars-filter-4_cyl-body"))

  select_mpg <- "#filter_panel-active-mtcars-filter-mtcars_mpg > div.filter-card-header"
  app_driver$click(selector = select_mpg)
  app_driver$wait_for_idle(default_idle_duration * 8)
  testthat::expect_true(is_existing(app_driver, "#filter_panel-active-mtcars-filter-mtcars_mpg-body"))
  testthat::expect_true(is_visible(app_driver, "#filter_panel-active-mtcars-filter-mtcars_mpg-body"))
})
