% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilteredData-utils.R
\name{toggle_button}
\alias{toggle_button}
\alias{toggle_icon}
\alias{toggle_title}
\title{Toggle button properties.}
\usage{
toggle_icon(input_id, icons, one_way = FALSE)

toggle_title(input_id, titles, one_way = FALSE)
}
\arguments{
\item{input_id}{(\code{character(1)}) (name-spaced) id of the button}

\item{icons, titles}{(\code{character(2)}) vector specifying values between which to toggle}

\item{one_way}{(\code{logical(1)}) flag specifying whether to keep toggling;
if TRUE, the target will be changed
from the first element of \code{icons}/\code{titles} to the second}
}
\value{
\code{NULL}, invisibly.
}
\description{
Switch between different icons or titles on a button.
}
\details{
Wrapper functions that use \code{shinyjs::runjs} to change button properties in response to events,
typically clicking those very buttons.
\code{shiny}'s \code{actionButton} and \code{actionLink} create \verb{<a>} tags,
which may contain a child \verb{<i>} tag that specifies an icon to be displayed.
\code{toggle_icon} calls the \code{toggleClass} (when \code{one_way = FALSE}) or
\code{removeClass} and \code{addClass} methods (when \code{one_way = TRUE}) to change icons.
\code{toggle_title} calls the \code{attr} method to modify the \code{Title} attribute of the button.
}
\examples{
# use non-exported function from teal.slice
toggle_icon <- getFromNamespace("toggle_icon", "teal.slice")

library(shiny)
library(shinyjs)

ui <- fluidPage(
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

}
\keyword{internal}
