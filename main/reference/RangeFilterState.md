# `FilterState` object for numeric data

Manages choosing a numeric range.

## Super class

[`teal.slice::FilterState`](https://insightsengineering.github.io/teal.slice/reference/FilterState.md)
-\> `RangeFilterState`

## Methods

### Public methods

- [`RangeFilterState$new()`](#method-RangeFilterState-new)

- [`RangeFilterState$get_call()`](#method-RangeFilterState-get_call)

- [`RangeFilterState$get_keep_inf()`](#method-RangeFilterState-get_keep_inf)

- [`RangeFilterState$clone()`](#method-RangeFilterState-clone)

Inherited methods

- [`teal.slice::FilterState$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-destroy)
- [`teal.slice::FilterState$format()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-format)
- [`teal.slice::FilterState$get_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-get_state)
- [`teal.slice::FilterState$print()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-print)
- [`teal.slice::FilterState$server()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-server)
- [`teal.slice::FilterState$set_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-set_state)
- [`teal.slice::FilterState$ui()`](https://insightsengineering.github.io/teal.slice/reference/FilterState.html#method-ui)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize a `FilterState` object for range selection.

#### Usage

    RangeFilterState$new(
      x,
      x_reactive = reactive(NULL),
      extract_type = character(0),
      slice
    )

#### Arguments

- `x`:

  (`numeric`) variable to be filtered.

- `x_reactive`:

  (`reactive`) returning vector of the same type as `x`. Is used to
  update counts following the change in values of the filtered dataset.
  If it is set to `reactive(NULL)` then counts based on filtered dataset
  are not shown.

- `extract_type`:

  (`character`) specifying whether condition calls should be prefixed by
  `dataname`. Possible values:

  - `character(0)` (default) `varname` in the condition call will not be
    prefixed

  - `"list"` `varname` in the condition call will be returned as
    `<dataname>$<varname>`

  - `"matrix"` `varname` in the condition call will be returned as
    `<dataname>[, <varname>]`

- `slice`:

  (`teal_slice`) specification of this filter state. `teal_slice` is
  stored in the object and `set_state` directly manipulates values
  within `teal_slice`. `get_state` returns `teal_slice` object which can
  be reused in other places. Note that `teal_slice` is a
  `reactiveValues`, which means it has reference semantics, i.e. changes
  made to an object are automatically reflected in all places that refer
  to the same `teal_slice`.

#### Returns

Object of class `RangeFilterState`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Returns reproducible condition call for current selection. For this
class returned call looks like
`<varname> >= <min value> & <varname> <= <max value>` with optional
`is.na(<varname>)` and `is.finite(<varname>)`.

#### Usage

    RangeFilterState$get_call(dataname)

#### Arguments

- `dataname`:

  name of data set; defaults to `private$get_dataname()`

#### Returns

`call`

------------------------------------------------------------------------

### Method `get_keep_inf()`

Returns current `keep_inf` selection.

#### Usage

    RangeFilterState$get_keep_inf()

#### Returns

`logical(1)`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RangeFilterState$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
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
#> NULL
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
#> is.na(x) | (is.infinite(x) | x >= 3 & x <= 8)

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

ui <- bslib::page_fluid(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  bslib::layout_column_wrap(
    width = 1 / 3,
    tags$div(
      tags$h4("RangeFilterState"),
      fs$ui("fs")
    ),
    tags$div(
      id = "outputs", # div id is needed for toggling the element
      tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
      textOutput("condition_range"), tags$br(),
      tags$h4("Unformatted state"), # display raw filter state
      textOutput("unformatted_range"), tags$br(),
      tags$h4("Formatted state"), # display human readable filter state
      textOutput("formatted_range"), tags$br()
    ),
    tags$div(
      tags$h4("Programmatic filter control"),
      actionButton("button1_range", "set drop NA", width = "100%"), tags$br(),
      actionButton("button2_range", "set keep NA", width = "100%"), tags$br(),
      actionButton("button3_range", "set drop Inf", width = "100%"), tags$br(),
      actionButton("button4_range", "set keep Inf", width = "100%"), tags$br(),
      actionButton("button5_range", "set a range", width = "100%"), tags$br(),
      actionButton("button6_range", "set full range", width = "100%"), tags$br(),
      actionButton("button0_range", "set initial state", width = "100%"), tags$br()
    )
  )
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
```
