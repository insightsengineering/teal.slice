#' Initialize `FilteredData`
#'
#' Function creates a `FilteredData` object.
#'
#' @param x (`named list`) of datasets.
#' @param join_keys (`join_keys`) see [`teal.data::join_keys()`].
#' @param code `r lifecycle::badge("deprecated")`
#' @param check `r lifecycle::badge("deprecated")`
#'
#' @return Object of class `FilteredData`.
#'
#' @examples
#' datasets <- init_filtered_data(list(iris = iris, mtcars = mtcars))
#' datasets
#'
#' @export
init_filtered_data <- function(x, join_keys = teal.data::join_keys(), code, check) { # nolint
  checkmate::assert_list(x, any.missing = FALSE, names = "unique")
  checkmate::assert_class(join_keys, "join_keys")
  if (!missing(code)) {
    lifecycle::deprecate_stop(
      "0.5.0",
      "init_filtered_data(code = 'No longer supported')"
    )
  }
  if (!missing(check)) {
    lifecycle::deprecate_stop(
      "0.5.0",
      "init_filtered_data(check = 'No longer supported')"
    )
  }
  FilteredData$new(x, join_keys = join_keys)
}

#' Evaluate expression with meaningful message
#'
#' Method created for the `FilteredData` object to execute filter call with
#' meaningful message. After evaluation used environment should contain
#' all necessary bindings.
#'
#' @param expr (`language`)
#' @param env (`environment`) where expression is evaluated.
#' @return `NULL`, invisibly.
#' @keywords internal
eval_expr_with_msg <- function(expr, env) {
  lapply(
    expr,
    function(x) {
      tryCatch(
        eval(x, envir = env),
        error = function(e) {
          stop(
            sprintf(
              "Call execution failed:\n - call:\n   %s\n - message:\n   %s ",
              deparse1(x, collapse = "\n"), e
            )
          )
        }
      )
    }
  )
  invisible(NULL)
}


#' Toggle button properties.
#'
#' Switch between different icons or titles on a button.
#'
#' Wrapper functions that use `shinyjs::runjs` to change button properties in response to events,
#' typically clicking those very buttons.
#' `shiny`'s `actionButton` and `actionLink` create `<a>` tags,
#' which may contain a child `<i>` tag that specifies an icon to be displayed.
#' `toggle_icon` calls the `toggleClass` (when `one_way = FALSE`) or
#' `removeClass` and `addClass` methods (when `one_way = TRUE`) to change icons.
#' `toggle_title` calls the `attr` method to modify the `Title` attribute of the button.
#'
#' @param input_id (`character(1)`) (name-spaced) id of the button
#' @param icons,titles (`character(2)`) vector specifying values between which to toggle
#' @param one_way (`logical(1)`) flag specifying whether to keep toggling;
#'                if TRUE, the target will be changed
#'                from the first element of `icons`/`titles` to the second
#'
#' @return `NULL`, invisibly.
#'
#' @name toggle_button
#' @rdname toggle_button
#' @keywords internal
toggle_icon <- function(input_id, icons, one_way = FALSE) {
  checkmate::assert_string(input_id)
  checkmate::assert_character(icons, len = 2L)
  checkmate::assert_flag(one_way)

  expr <-
    if (one_way) {
      sprintf(
        "$('#%s i').removeClass('%s').addClass('%s');",
        input_id, icons[1], icons[2]
      )
    } else {
      sprintf("$('#%s i').toggleClass('%s');", input_id, paste(icons, collapse = " "))
    }

  shinyjs::runjs(expr)

  invisible(NULL)
}

#' @rdname toggle_button
#' @keywords internal
toggle_title <- function(input_id, titles, one_way = FALSE) {
  checkmate::assert_string(input_id)
  checkmate::assert_character(titles, len = 2L)
  checkmate::assert_flag(one_way)

  expr <-
    if (one_way) {
      sprintf(
        "$('a#%s').attr('title', '%s');",
        input_id, titles[2]
      )
    } else {
      sprintf(
        paste0(
          "var button_id = 'a#%1$s';",
          "var curr = $(button_id).attr('title');",
          "if (curr == '%2$s') { $(button_id).attr('title', '%3$s');",
          "} else { $(button_id).attr('title', '%2$s');",
          "}"
        ),
        input_id, titles[1], titles[2]
      )
    }

  shinyjs::runjs(expr)

  invisible(NULL)
}

#' @inherit teal.data::topological_sort description details params title
#'
#' @keywords internal
topological_sort <- function(graph) {
  utils::getFromNamespace("topological_sort", ns = "teal.data")(graph)
}
