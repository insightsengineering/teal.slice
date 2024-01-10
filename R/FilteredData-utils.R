#' Initialize `FilteredData`
#'
#' Initialize `FilteredData`
#' @param x (named `list`) of datasets.
#' @param join_keys (`join_keys`) see [teal.data::join_keys()].
#' @param code (deprecated)
#' @param check (deprecated)
#' @examples
#' datasets <- init_filtered_data(list(iris = iris, mtcars = mtcars))
#' datasets
#' @export
init_filtered_data <- function(x, join_keys = teal.data::join_keys(), code, check) { # nolint
  checkmate::assert_list(x, any.missing = FALSE, names = "unique")
  checkmate::assert_class(join_keys, "join_keys")
  if (!missing(code)) {
    lifecycle::deprecate_stop(
      "0.4.1",
      "init_filtered_data(code = 'No longer supported')"
    )
  }
  if (!missing(check)) {
    lifecycle::deprecate_stop(
      "0.4.1",
      "init_filtered_data(check = 'No longer supported')"
    )
  }
  FilteredData$new(x, join_keys = join_keys)
}

#' Evaluate expression with meaningful message
#'
#' Method created for the `FilteredData` to execute filter call with
#' meaningful message. After evaluation used environment should contain
#' all necessary bindings.
#' @param expr (`language`)
#' @param env (`environment`) where expression is evaluated.
#' @return invisible `NULL`.
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
      return(invisible(NULL))
    }
  )
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
#' @param input_id `character(1)` (name-spaced) id of the button
#' @param icons,titles `character(2)` vector specifying values between which to toggle
#' @param one_way `logical(1)` flag specifying whether to keep toggling;
#'                if TRUE, the target will be changed
#'                from the first element of `icons`/`titles` to the second
#'
#' @return Invisible NULL.
#' @examples
#' # use non-exported function from teal.slice
#' toggle_icon <- getFromNamespace("toggle_icon", "teal.slice")
#'
#' library(shiny)
#'
#' ui <- fluidPage(
#'   shinyjs::useShinyjs(),
#'   actionButton("hide_content", label = "hide", icon = icon("xmark")),
#'   actionButton("show_content", label = "show", icon = icon("check")),
#'   actionButton("toggle_content", label = "toggle", icon = icon("angle-down")),
#'   br(),
#'   div(
#'     id = "content",
#'     verbatimTextOutput("printout")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$hide_content,
#'                {
#'                  shinyjs::hide("content")
#'                  toggle_icon("toggle_content", c("fa-angle-down", "fa-angle-right"), one_way = TRUE)
#'                },
#'                ignoreInit = TRUE
#'   )
#'
#'   observeEvent(input$show_content,
#'                {
#'                  shinyjs::show("content")
#'                  toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"), one_way = TRUE)
#'                },
#'                ignoreInit = TRUE
#'   )
#'
#'   observeEvent(input$toggle_content,
#'                {
#'                  shinyjs::toggle("content")
#'                  toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"))
#'                },
#'                ignoreInit = TRUE
#'   )
#'
#'   output$printout <- renderPrint({
#'     head(faithful, 10)
#'   })
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
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

#' Topological graph sort
#'
#' Graph is a list which for each node contains a vector of child nodes
#' in the returned list, parents appear before their children.
#'
#' Implementation of `Kahn` algorithm with a modification to maintain the order of input elements.
#'
#' @param graph (named `list`) list with node vector elements
#' @examples
#' # use non-exported function from teal.slice
#' topological_sort <- getFromNamespace("topological_sort", "teal.slice")
#'
#' topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
#' topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
#' topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
#' @keywords internal
topological_sort <- function(graph) {
  # compute in-degrees
  in_degrees <- list()
  for (node in names(graph)) {
    in_degrees[[node]] <- 0
    for (to_edge in graph[[node]]) {
      in_degrees[[to_edge]] <- 0
    }
  }

  for (node in graph) {
    for (to_edge in node) {
      in_degrees[[to_edge]] <- in_degrees[[to_edge]] + 1
    }
  }

  # sort
  visited <- 0
  sorted <- list()
  zero_in <- list()
  for (node in names(in_degrees)) {
    if (in_degrees[[node]] == 0) zero_in <- append(zero_in, node)
  }
  zero_in <- rev(zero_in)

  while (length(zero_in) != 0) {
    visited <- visited + 1
    sorted <- c(zero_in[[1]], sorted)
    for (edge_to in graph[[zero_in[[1]]]]) {
      in_degrees[[edge_to]] <- in_degrees[[edge_to]] - 1
      if (in_degrees[[edge_to]] == 0) {
        zero_in <- append(zero_in, edge_to, 1)
      }
    }
    zero_in[[1]] <- NULL
  }

  if (visited != length(in_degrees)) {
    stop(
      "Graph is not a directed acyclic graph. Cycles involving nodes: ",
      paste0(setdiff(names(in_degrees), sorted), collapse = " ")
    )
  } else {
    return(sorted)
  }
}
