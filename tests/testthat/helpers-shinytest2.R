default_idle_timeout <- 20000 # Wait (ms) at most until idle
default_idle_duration <- 200 # Time (ms) it is idle

# Check visibility (borrowed from teal.widgets/tests/testthat/helpers-utils.R)
is_visible <- function(app_driver, element) {
  js_script <- sprintf("
    Array.from(document.querySelectorAll('%s')).map(el => {
      return el.checkVisibility() && (el.textContent.trim().length > 0 || el.children.length > 0);
    });
  ", element)

  any(unlist(app_driver$get_js(js_script)))
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
