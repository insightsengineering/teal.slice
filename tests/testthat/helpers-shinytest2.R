default_idle_timeout <- 20000 # Wait (ms) at most until idle
default_idle_duration <- 200 # Time (ms) it is idle

#' Function to check if an selector is visible in a shiny app
#'
#' The [shinytest2::AppDriver$wait_for_js()] method is used to check if the selector
#' throws an error when the selector is not visible.
#'
#' @param selector `character(1)` CSS selector of the element to check visibility for.
#' @param app_driver `shinytest2::AppDriver` AppDriver object of
#' the shiny app.
#' @param timeout `numeric(1)` maximum time to wait for the element to be
#' visible. The default is the timeout set in the [shinytest2::AppDriver] object.
#' @param expectation_fun `function` expectation function to use for checking
#' visibility.
#' @return `logical(1)` whether the selector is visible.
#' @keywords internal
expect_visible <- function(selector, app_driver, timeout) {
  checkmate::assert(
    .var.name = "selector",
    combine = "and",
    checkmate::check_string(selector),
    if (grepl("[\"]", selector)) "Cannot contain double quotes (\") in CSS selectors" else TRUE
  )
  checkmate::assert_r6(app_driver, "AppDriver")

  tryCatch(
    {
      app_driver$wait_for_js(
        sprintf(
          "Array.from(document.querySelectorAll(\"%s\")).map(el => el.checkVisibility() && (el.textContent.trim().length > 0 || el.children.length > 0)).some(Boolean)",
          selector
        ),
        timeout
      )
      testthat::succeed()
    },
    error = function(err) {
      testthat::fail(sprintf("CSS selector '%s' does not produce any visible elements.", selector))
    }
  )
}

#' @describeIn expect_visible Check if an selector is hidden for a given timeout.
expect_hidden <- function(selector, app_driver, timeout) {
  checkmate::assert_string(selector)
  checkmate::assert_r6(app_driver, "AppDriver")
  tryCatch(
    {
      app_driver$wait_for_js(
        sprintf(
          "!Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility() && (el.textContent.trim().length > 0 || el.children.length > 0)).some(Boolean)",
          selector
        ),
        timeout
      )
      testthat::succeed()
    },
    error = function(err) testthat::fail(sprintf("CSS selector '%s' produces visible elements.", selector))
  )
}

# Write a js code to extract the classes
get_attribute <- function(selector, attribute) {
  sprintf(
    "Array.from(document.querySelectorAll('%s')).map(el => el.getAttribute('%s'))",
    selector, attribute
  )
}

is_existing <- function(app_driver, element) {
  js_script <- sprintf("document.querySelectorAll('%s').length > 0;", element)
  app_driver$get_js(js_script)
}
