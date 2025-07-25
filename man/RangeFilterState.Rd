% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilterStateRange.R
\docType{class}
\name{RangeFilterState}
\alias{RangeFilterState}
\title{\code{FilterState} object for numeric data}
\description{
Manages choosing a numeric range.
}
\examples{
# use non-exported function from teal.slice
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
RangeFilterState <- getFromNamespace("RangeFilterState", "teal.slice")

library(shiny)

filter_state <- RangeFilterState$new(
  x = c(NA, Inf, seq(1:10)),
  slice = teal_slice(varname = "x", dataname = "data")
)
isolate(filter_state$get_call())
filter_state$set_state(
  teal_slice(
    dataname = "data",
    varname = "x",
    selected = c(3L, 8L),
    keep_na = TRUE,
    keep_inf = TRUE
  )
)
isolate(filter_state$get_call())

# working filter in an app
library(shinyjs)

data_range <- c(runif(100, 0, 1), NA, Inf)
fs <- RangeFilterState$new(
  x = data_range,
  slice = teal_slice(
    dataname = "data",
    varname = "x",
    selected = c(0.15, 0.93),
    keep_na = TRUE,
    keep_inf = TRUE
  )
)

ui <- fluidPage(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  column(4, tags$div(
    tags$h4("RangeFilterState"),
    fs$ui("fs")
  )),
  column(4, tags$div(
    id = "outputs", # div id is needed for toggling the element
    tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
    textOutput("condition_range"), tags$br(),
    tags$h4("Unformatted state"), # display raw filter state
    textOutput("unformatted_range"), tags$br(),
    tags$h4("Formatted state"), # display human readable filter state
    textOutput("formatted_range"), tags$br()
  )),
  column(4, tags$div(
    tags$h4("Programmatic filter control"),
    actionButton("button1_range", "set drop NA", width = "100\%"), tags$br(),
    actionButton("button2_range", "set keep NA", width = "100\%"), tags$br(),
    actionButton("button3_range", "set drop Inf", width = "100\%"), tags$br(),
    actionButton("button4_range", "set keep Inf", width = "100\%"), tags$br(),
    actionButton("button5_range", "set a range", width = "100\%"), tags$br(),
    actionButton("button6_range", "set full range", width = "100\%"), tags$br(),
    actionButton("button0_range", "set initial state", width = "100\%"), tags$br()
  ))
)

server <- function(input, output, session) {
  fs$server("fs")
  output$condition_range <- renderPrint(fs$get_call())
  output$formatted_range <- renderText(fs$format())
  output$unformatted_range <- renderPrint(fs$get_state())
  # modify filter state programmatically
  observeEvent(
    input$button1_range,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = FALSE))
  )
  observeEvent(
    input$button2_range,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = TRUE))
  )
  observeEvent(
    input$button3_range,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_inf = FALSE))
  )
  observeEvent(
    input$button4_range,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_inf = TRUE))
  )
  observeEvent(
    input$button5_range,
    fs$set_state(
      teal_slice(dataname = "data", varname = "x", selected = c(0.2, 0.74))
    )
  )
  observeEvent(
    input$button6_range,
    fs$set_state(teal_slice(dataname = "data", varname = "x", selected = c(0, 1)))
  )
  observeEvent(
    input$button0_range,
    fs$set_state(
      teal_slice("data", "variable", selected = c(0.15, 0.93), keep_na = TRUE, keep_inf = TRUE)
    )
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
}
\keyword{internal}
\section{Super class}{
\code{\link[teal.slice:FilterState]{teal.slice::FilterState}} -> \code{RangeFilterState}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RangeFilterState-new}{\code{RangeFilterState$new()}}
\item \href{#method-RangeFilterState-get_call}{\code{RangeFilterState$get_call()}}
\item \href{#method-RangeFilterState-get_keep_inf}{\code{RangeFilterState$get_keep_inf()}}
\item \href{#method-RangeFilterState-clone}{\code{RangeFilterState$clone()}}
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
\if{html}{\out{<a id="method-RangeFilterState-new"></a>}}
\if{latex}{\out{\hypertarget{method-RangeFilterState-new}{}}}
\subsection{Method \code{new()}}{
Initialize a \code{FilterState} object for range selection.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RangeFilterState$new(
  x,
  x_reactive = reactive(NULL),
  extract_type = character(0),
  slice
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{(\code{numeric})
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
Object of class \code{RangeFilterState}, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RangeFilterState-get_call"></a>}}
\if{latex}{\out{\hypertarget{method-RangeFilterState-get_call}{}}}
\subsection{Method \code{get_call()}}{
Returns reproducible condition call for current selection.
For this class returned call looks like
\verb{<varname> >= <min value> & <varname> <= <max value>} with
optional \verb{is.na(<varname>)} and \verb{is.finite(<varname>)}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RangeFilterState$get_call(dataname)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-RangeFilterState-get_keep_inf"></a>}}
\if{latex}{\out{\hypertarget{method-RangeFilterState-get_keep_inf}{}}}
\subsection{Method \code{get_keep_inf()}}{
Returns current \code{keep_inf} selection.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RangeFilterState$get_keep_inf()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{logical(1)}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RangeFilterState-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RangeFilterState-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RangeFilterState$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
