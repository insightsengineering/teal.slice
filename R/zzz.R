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

# This function is necessary for check to properly process code dependencies within R6 classes.
# If `package` is listed in `Imports` in `DESCRIPTION`,
# (1) check goes through `NAMESPACE` looking for any `importFrom(package,<foo>)` statements
# or an `import(package)` statement. If none are found,
# (2) check looks for `package::*` calls in the code. If none are found again,
# (3) check throws a NOTE;
# #  Namespaces in Imports field not imported from:
# #    'package'
# #  All declared Imports should be used.
# This note is banned by our CI.
# When package::* statements are made within an R6 class, they are not registered.
# This function provides single references to the imported namespaces for check to notice.
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
