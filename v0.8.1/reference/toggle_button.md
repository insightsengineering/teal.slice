# Toggle button properties.

Switch between different icons or titles on a button.

## Usage

``` r
toggle_icon(input_id, icons, one_way = FALSE)

toggle_title(input_id, titles, one_way = FALSE)
```

## Arguments

- input_id:

  (`character(1)`) (name-spaced) id of the button

- icons, titles:

  (`character(2)`) vector specifying values between which to toggle

- one_way:

  (`logical(1)`) flag specifying whether to keep toggling; if TRUE, the
  target will be changed from the first element of `icons`/`titles` to
  the second

## Value

`NULL`, invisibly.

## Details

Wrapper functions that use
[`shinyjs::runjs`](https://rdrr.io/pkg/shinyjs/man/runjs.html) to change
button properties in response to events, typically clicking those very
buttons. `shiny`'s `actionButton` and `actionLink` create `<a>` tags,
which may contain a child `<i>` tag that specifies an icon to be
displayed. `toggle_icon` calls the `toggleClass` (when
`one_way = FALSE`) or `removeClass` and `addClass` methods (when
`one_way = TRUE`) to change icons. `toggle_title` calls the `attr`
method to modify the `Title` attribute of the button.

## Examples

``` r
# use non-exported function from teal.slice
toggle_icon <- getFromNamespace("toggle_icon", "teal.slice")

library(shiny)
library(shinyjs)

ui <- bslib::page_fluid(
  useShinyjs(),
  actionButton("hide_content", label = "hide", icon = icon("xmark")),
  actionButton("show_content", label = "show", icon = icon("check")),
  actionButton("toggle_content", label = "toggle", icon = icon("angle-down")),
  tags$br(),
  tags$div(
    id = "content",
    verbatimTextOutput("printout")
  )
)

server <- function(input, output, session) {
  observeEvent(input$hide_content,
    {
      hide("content")
      toggle_icon("toggle_content", c("fa-angle-down", "fa-angle-right"), one_way = TRUE)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$show_content,
    {
      show("content")
      toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"), one_way = TRUE)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$toggle_content,
    {
      toggle("content")
      toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"))
    },
    ignoreInit = TRUE
  )

  output$printout <- renderPrint({
    head(faithful, 10)
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
