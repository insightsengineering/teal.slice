# `FilterState` object for `Date` data

Manages choosing a range of `Date`s.

## Super class

[`teal.slice::FilterState`](https://insightsengineering.github.io/teal.slice/reference/FilterState.md)
-\> `DateFilterState`

## Methods

### Public methods

- [`DateFilterState$new()`](#method-DateFilterState-new)

- [`DateFilterState$get_call()`](#method-DateFilterState-get_call)

- [`DateFilterState$clone()`](#method-DateFilterState-clone)

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

Initialize a `FilterState` object.

#### Usage

    DateFilterState$new(
      x,
      x_reactive = reactive(NULL),
      slice,
      extract_type = character(0)
    )

#### Arguments

- `x`:

  (`Date`) variable to be filtered.

- `x_reactive`:

  (`reactive`) returning vector of the same type as `x`. Is used to
  update counts following the change in values of the filtered dataset.
  If it is set to `reactive(NULL)` then counts based on filtered dataset
  are not shown.

- `slice`:

  (`teal_slice`) specification of this filter state. `teal_slice` is
  stored in the object and `set_state` directly manipulates values
  within `teal_slice`. `get_state` returns `teal_slice` object which can
  be reused in other places. Note that `teal_slice` is a
  `reactiveValues`, which means it has reference semantics, i.e. changes
  made to an object are automatically reflected in all places that refer
  to the same `teal_slice`.

- `extract_type`:

  (`character`) specifying whether condition calls should be prefixed by
  `dataname`. Possible values:

  - `character(0)` (default) `varname` in the condition call will not be
    prefixed

  - `"list"` `varname` in the condition call will be returned as
    `<dataname>$<varname>`

  - `"matrix"` `varname` in the condition call will be returned as
    `<dataname>[, <varname>]`

#### Returns

Object of class `DateFilterState`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Returns reproducible condition call for current selection. For this
class returned call looks like
`<varname> >= <min value> & <varname> <= <max value>` with optional
`is.na(<varname>)`.

#### Usage

    DateFilterState$get_call(dataname)

#### Arguments

- `dataname`:

  (`character(1)`) containing possibly prefixed name of data set

#### Returns

`call` or `NULL`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DateFilterState$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
DateFilterState <- getFromNamespace("DateFilterState", "teal.slice")

library(shiny)

filter_state <- DateFilterState$new(
  x = c(Sys.Date() + seq(1:10), NA),
  slice = teal_slice(varname = "x", dataname = "data"),
  extract_type = character(0)
)
isolate(filter_state$get_call())
#> NULL
filter_state$set_state(
  teal_slice(
    dataname = "data",
    varname = "x",
    selected = c(Sys.Date() + 3L, Sys.Date() + 8L),
    keep_na = TRUE
  )
)
isolate(filter_state$get_call())
#> is.na(x) | x >= as.Date("2025-12-05") & x <= as.Date("2025-12-10")

# working filter in an app
library(shinyjs)

dates <- c(Sys.Date() - 100, Sys.Date())
data_date <- c(seq(from = dates[1], to = dates[2], length.out = 100), NA)
fs <- DateFilterState$new(
  x = data_date,
  slice = teal_slice(
    dataname = "data", varname = "x", selected = data_date[c(47, 98)], keep_na = TRUE
  )
)

ui <- bslib::page_fluid(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  bslib::layout_column_wrap(
    width = 1 / 3,
    tags$div(
      tags$h4("DateFilterState"),
      fs$ui("fs")
    ),
    tags$div(
      id = "outputs", # div id is needed for toggling the element
      tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
      textOutput("condition_date"), tags$br(),
      tags$h4("Unformatted state"), # display raw filter state
      textOutput("unformatted_date"), tags$br(),
      tags$h4("Formatted state"), # display human readable filter state
      textOutput("formatted_date"), tags$br()
    ),
    tags$div(
      tags$h4("Programmatic filter control"),
      actionButton("button1_date", "set drop NA", width = "100%"), tags$br(),
      actionButton("button2_date", "set keep NA", width = "100%"), tags$br(),
      actionButton("button3_date", "set a range", width = "100%"), tags$br(),
      actionButton("button4_date", "set full range", width = "100%"), tags$br(),
      actionButton("button0_date", "set initial state", width = "100%"), tags$br()
    )
  )
)

server <- function(input, output, session) {
  fs$server("fs")
  output$condition_date <- renderPrint(fs$get_call())
  output$formatted_date <- renderText(fs$format())
  output$unformatted_date <- renderPrint(fs$get_state())
  # modify filter state programmatically
  observeEvent(
    input$button1_date,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = FALSE))
  )
  observeEvent(
    input$button2_date,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = TRUE))
  )
  observeEvent(
    input$button3_date,
    fs$set_state(teal_slice(dataname = "data", varname = "x", selected = data_date[c(34, 56)]))
  )
  observeEvent(
    input$button4_date,
    fs$set_state(teal_slice(dataname = "data", varname = "x", selected = dates))
  )
  observeEvent(
    input$button0_date,
    fs$set_state(
      teal_slice("data", "variable", selected = data_date[c(47, 98)], keep_na = TRUE)
    )
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
```
