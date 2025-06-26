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

