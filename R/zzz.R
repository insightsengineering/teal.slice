.onLoad <- function(libname, pkgname) { # nolint
  # adapted from https://github.com/r-lib/devtools/blob/master/R/zzz.R
  teal_default_options <- list(teal.threshold_slider_vs_checkboxgroup = 5)
  op <- options()
  toset <- !(names(teal_default_options) %in% names(op))
  if (any(toset)) options(teal_default_options[toset])

  # Set up the teal logger instance
  teal.logger::register_logger("teal.slice")

  invisible()
}


### GLOBAL VARIABLES ###

.filterable_class <- c("logical", "integer", "numeric", "factor", "character", "Date", "POSIXct", "POSIXlt")


### END GLOBAL VARIABLES ###


### ENSURE CHECK PASSES

# this function is necessary for check to properly process code dependencies within R6 classes
.rectify_dependencies_check <- function() {
  dplyr::filter
  grDevices::rgb
  lifecycle::badge
  logger::log_trace
  plotly::plot_ly
  shinycssloaders::withSpinner
  shinyWidgets::pickerOptions
  teal.widgets::optionalSelectInput
}


### END ENSURE CHECK PASSES
