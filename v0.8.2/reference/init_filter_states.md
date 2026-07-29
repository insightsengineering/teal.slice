# Initialize `FilterStates` object

Initialize `FilterStates` object

## Usage

``` r
init_filter_states(
  data,
  data_reactive = reactive(NULL),
  dataname,
  datalabel = NULL,
  ...
)
```

## Arguments

- data:

  (`data.frame` or `MultiAssayExperiment` or `SummarizedExperiment` or
  `matrix`) object to subset.

- data_reactive:

  (`function(sid)`) should return an object of the same type as `data`
  or `NULL`. This function is needed for the `FilterState` `shiny`
  module to update counts if filtered data changes. If function returns
  `NULL` then filtered counts are not shown. Function has to have `sid`
  argument being a character which is related to `sid` argument in the
  `get_call` method.

- dataname:

  (`character(1)`) name of the data used in the subset expression,
  passed to the function argument attached to this `FilterStates`.

- datalabel:

  (`character(1)`) optional text label.

- ...:

  optional, additional arguments for specific classes: keys.

## Value

Object of class `FilterStates`.

## Examples

``` r
# use non-exported function from teal.slice
init_filter_states <- getFromNamespace("init_filter_states", "teal.slice")

df <- data.frame(
  character = letters,
  numeric = seq_along(letters),
  date = seq(Sys.Date(), length.out = length(letters), by = "1 day"),
  datetime = seq(Sys.time(), length.out = length(letters), by = "33.33 hours")
)
rf <- init_filter_states(
  data = df,
  dataname = "DF"
)

library(shiny)
ui <- bslib::page_fluid(
  actionButton("clear", tags$span(icon("xmark"), "Remove all filters")),
  rf$ui_add(id = "add"),
  rf$ui_active("states"),
  verbatimTextOutput("expr"),
)

server <- function(input, output, session) {
  rf$srv_add(id = "add")
  rf$srv_active(id = "states")
  output$expr <- renderText({
    deparse1(rf$get_call(), collapse = "\n")
  })
  observeEvent(input$clear, rf$clear_filter_states())
}

if (interactive()) {
  shinyApp(ui, server)
}
```
