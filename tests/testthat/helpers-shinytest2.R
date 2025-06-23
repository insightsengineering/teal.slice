default_idle_timeout <- 20000 # Wait (ms) at most until idle
default_idle_duration <- 200 # Time (ms) it is idle

# Check visibility (borrowed from teal.widgets/tests/testthat/helpers-utils.R)
is_visible <- function(app_driver, element) {
  any(
    unlist(
      app_driver$get_js(
        sprintf(
          "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility())",
          element
        )
      )
    )
  )
}


# Write a js code to extract the classes
get_class <- function(id) {
  unlist(
    app_driver$get_js(
      sprintf(
        "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility())",
        element
      )
    )
  )
}


element_class_shown <- function(id) {
  sprintf("const element = document.getElementById('%s');
           element.classList.contains('show');", id)
}

element_expanded_attribute <- function(id) {
  sprintf("const element = document.getElementById('%s');
  element.getAttribute('aria-expanded');", id)
}
