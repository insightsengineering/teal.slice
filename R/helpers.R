
#' Toggle button properties.
#'
#' Switch between different icons or titles on a button.
#'
#' @param input_id `character(1)` (namespaced) id of the button
#' @param icons,titles `character(2)` vector specifing values between which to toggle
#' @param one_way `logical(1)` flag specifying whether to keep toggling;
#'                if TRUE, the target will be changed
#'                from the first element of `icons`/`titles` to the second
#'
#' @return Invisible NULL.
#'
#' @name toggle_button
#'
#' @examples
#' \dontrun{
#'
#' # continuously switch between right- and down-pointing chevrons
#' toggle_icon("toggle_element", c("fa-angle-right", "fa-angle-down"))
#'
#' # switch right- to down-pointing chevron
#' toggle_icon("toggle_element", c("fa-angle-right", "fa-angle-down"))
#' }
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
#'
#'   observeEvent(input[["hide_content"]], {
#'     shinyjs::hide("content")
#'     toggle_icon("toggle_content", c("fa-angle-down", "fa-angle-right"), one_way = TRUE)
#'   }, ignoreInit = TRUE)
#'
#'   observeEvent(input[["show_content"]], {
#'     shinyjs::show("content")
#'     toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"), one_way = TRUE)
#'   }, ignoreInit = TRUE)
#'
#'   observeEvent(input[["toggle_content"]], {
#'     shinyjs::toggle("content")
#'     toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"))
#'   }, ignoreInit = TRUE)
#'
#'   output[["printout"]] <- renderPrint({
#'     head(faithful, 10)
#'   })
#'
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @rdname toggle_button
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
toggle_title <- function(input_id, titles, one_way = FALSE) {
  checkmate::assert_string(input_id)
  checkmate::assert_character(titles, len = 2L)
  checkmate::assert_flag(one_way)

  expr <-
    if (one_way) {
      sprintf(
        "$(a#%s).attr('title', '%s');",
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
  if (one_way) print(expr)

  shinyjs::runjs(expr)

  invisible(NULL)
}
