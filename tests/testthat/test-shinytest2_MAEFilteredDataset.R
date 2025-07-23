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

testthat::test_that("Active filter summary has all the experiments.", {
  app_driver <- local_app_driver()

  selector <- "#filter_panel-active-MAE-filters > div.shiny-html-output.accordion.shiny-bound-output"
  text <- app_driver$get_js(get_attribute(selector, "data-label"))
  clean_text <- gsub(pattern = "> ", replacement = "", unlist(text))
  testthat::expect_equal(clean_text, c(
    "subjects", "RNASeq2GeneNorm", "gistict",
    "RPPAArray", "Mutations", "miRNASeqGene"
  ))
})
