% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilterStateLogical.R
\docType{class}
\name{LogicalFilterState}
\alias{LogicalFilterState}
\title{\code{FilterState} object for logical data}
\description{
Manages choosing a logical state.
}
\examples{
# use non-exported function from teal.slice
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
LogicalFilterState <- getFromNamespace("LogicalFilterState", "teal.slice")

library(shiny)

filter_state <- LogicalFilterState$new(
  x = sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
  slice = teal_slice(varname = "x", dataname = "data")
)
isolate(filter_state$get_call())
filter_state$set_state(
  teal_slice(dataname = "data", varname = "x", selected = TRUE, keep_na = TRUE)
)
isolate(filter_state$get_call())

# working filter in an app
library(shinyjs)

data_logical <- c(sample(c(TRUE, FALSE), 10, replace = TRUE), NA)
fs <- LogicalFilterState$new(
  x = data_logical,
  slice = teal_slice(dataname = "data", varname = "x", selected = FALSE, keep_na = TRUE)
)

ui <- fluidPage(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  column(4, tags$div(
    tags$h4("LogicalFilterState"),
    fs$ui("fs")
  )),
  column(4, tags$div(
    id = "outputs", # div id is needed for toggling the element
    tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
    textOutput("condition_logical"), tags$br(),
    tags$h4("Unformatted state"), # display raw filter state
    textOutput("unformatted_logical"), tags$br(),
    tags$h4("Formatted state"), # display human readable filter state
    textOutput("formatted_logical"), tags$br()
  )),
  column(4, tags$div(
    tags$h4("Programmatic filter control"),
    actionButton("button1_logical", "set drop NA", width = "100\%"), tags$br(),
    actionButton("button2_logical", "set keep NA", width = "100\%"), tags$br(),
    actionButton("button3_logical", "set a selection", width = "100\%"), tags$br(),
    actionButton("button0_logical", "set initial state", width = "100\%"), tags$br()
  ))
)

server <- function(input, output, session) {
  fs$server("fs")
  output$condition_logical <- renderPrint(fs$get_call())
  output$formatted_logical <- renderText(fs$format())
  output$unformatted_logical <- renderPrint(fs$get_state())
  # modify filter state programmatically
  observeEvent(
    input$button1_logical,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = FALSE))
  )
  observeEvent(
    input$button2_logical,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = TRUE))
  )
  observeEvent(
    input$button3_logical,
    fs$set_state(teal_slice(dataname = "data", varname = "x", selected = TRUE))
  )
  observeEvent(
    input$button0_logical,
    fs$set_state(
      teal_slice(dataname = "data", varname = "x", selected = FALSE, keep_na = TRUE)
    )
  )
}

if (interactive()) {
  shinyApp(ui, server)
}

}
\keyword{internal}
\section{Super class}{
\code{\link[teal.slice:FilterState]{teal.slice::FilterState}} -> \code{LogicalFilterState}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LogicalFilterState-new}{\code{LogicalFilterState$new()}}
\item \href{#method-LogicalFilterState-get_call}{\code{LogicalFilterState$get_call()}}
\item \href{#method-LogicalFilterState-clone}{\code{LogicalFilterState$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="destroy"><a href='../../teal.slice/html/FilterState.html#method-FilterState-destroy'><code>teal.slice::FilterState$destroy()</code></a></span></li>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="format"><a href='../../teal.slice/html/FilterState.html#method-FilterState-format'><code>teal.slice::FilterState$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="get_state"><a href='../../teal.slice/html/FilterState.html#method-FilterState-get_state'><code>teal.slice::FilterState$get_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="print"><a href='../../teal.slice/html/FilterState.html#method-FilterState-print'><code>teal.slice::FilterState$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="server"><a href='../../teal.slice/html/FilterState.html#method-FilterState-server'><code>teal.slice::FilterState$server()</code></a></span></li>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="set_state"><a href='../../teal.slice/html/FilterState.html#method-FilterState-set_state'><code>teal.slice::FilterState$set_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="teal.slice" data-topic="FilterState" data-id="ui"><a href='../../teal.slice/html/FilterState.html#method-FilterState-ui'><code>teal.slice::FilterState$ui()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LogicalFilterState-new"></a>}}
\if{latex}{\out{\hypertarget{method-LogicalFilterState-new}{}}}
\subsection{Method \code{new()}}{
Initialize a \code{FilterState} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogicalFilterState$new(
  x,
  x_reactive = reactive(NULL),
  extract_type = character(0),
  slice
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{(\code{logical})
variable to be filtered.}

\item{\code{x_reactive}}{(\code{reactive})
returning vector of the same type as \code{x}. Is used to update
counts following the change in values of the filtered dataset.
If it is set to \code{reactive(NULL)} then counts based on filtered
dataset are not shown.}

\item{\code{extract_type}}{(\code{character})
specifying whether condition calls should be prefixed by \code{dataname}. Possible values:
\itemize{
\item \code{character(0)} (default) \code{varname} in the condition call will not be prefixed
\item \code{"list"} \code{varname} in the condition call will be returned as \verb{<dataname>$<varname>}
\item \code{"matrix"} \code{varname} in the condition call will be returned as \verb{<dataname>[, <varname>]}
}}

\item{\code{slice}}{(\code{teal_slice})
specification of this filter state.
\code{teal_slice} is stored in the object and \code{set_state} directly manipulates values within \code{teal_slice}.
\code{get_state} returns \code{teal_slice} object which can be reused in other places.
Note that \code{teal_slice} is a \code{reactiveValues}, which means it has reference semantics, i.e.
changes made to an object are automatically reflected in all places that refer to the same \code{teal_slice}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Object of class \code{LogicalFilterState}, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LogicalFilterState-get_call"></a>}}
\if{latex}{\out{\hypertarget{method-LogicalFilterState-get_call}{}}}
\subsection{Method \code{get_call()}}{
Returns reproducible condition call for current selection.
For \code{LogicalFilterState} it's a \verb{!<varname>} or \verb{<varname>} and optionally \verb{is.na(<varname>)}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogicalFilterState$get_call(dataname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataname}}{name of data set; defaults to \code{private$get_dataname()}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{call}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LogicalFilterState-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LogicalFilterState-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LogicalFilterState$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
