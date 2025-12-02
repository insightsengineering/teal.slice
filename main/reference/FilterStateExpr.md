# `FilterStateExpr` `R6` class

Sister class to `FilterState` that handles arbitrary filter expressions.

## Details

Creates a filter state around a predefined condition call (logical
predicate). The condition call is independent of the data and the filter
card allows no interaction (the filter is always fixed).

## Methods

### Public methods

- [`FilterStateExpr$new()`](#method-FilterStateExpr-new)

- [`FilterStateExpr$destroy()`](#method-FilterStateExpr-destroy)

- [`FilterStateExpr$format()`](#method-FilterStateExpr-format)

- [`FilterStateExpr$print()`](#method-FilterStateExpr-print)

- [`FilterStateExpr$get_state()`](#method-FilterStateExpr-get_state)

- [`FilterStateExpr$set_state()`](#method-FilterStateExpr-set_state)

- [`FilterStateExpr$get_call()`](#method-FilterStateExpr-get_call)

- [`FilterStateExpr$server()`](#method-FilterStateExpr-server)

- [`FilterStateExpr$ui()`](#method-FilterStateExpr-ui)

- [`FilterStateExpr$clone()`](#method-FilterStateExpr-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize a `FilterStateExpr` object.

#### Usage

    FilterStateExpr$new(slice)

#### Arguments

- `slice`:

  (`teal_slice_expr`)

#### Returns

Object of class `FilterStateExpr`, invisibly.

------------------------------------------------------------------------

### Method `destroy()`

Destroys a `FilterStateExpr` object.

#### Usage

    FilterStateExpr$destroy()

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Returns a formatted string representing this `FilterStateExpr` object.

#### Usage

    FilterStateExpr$format(show_all = FALSE, trim_lines = TRUE)

#### Arguments

- `show_all`:

  (`logical(1)`) passed to `format.teal_slice`

- `trim_lines`:

  (`logical(1)`) passed to `format.teal_slice`

#### Returns

`character(1)` the formatted string

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints this `FilterStateExpr` object.

#### Usage

    FilterStateExpr$print(...)

#### Arguments

- `...`:

  arguments passed to the `format` method

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `get_state()`

Returns a complete description of this filter state.

#### Usage

    FilterStateExpr$get_state()

#### Returns

A `teal_slice` object.

------------------------------------------------------------------------

### Method `set_state()`

Does nothing. Exists for compatibility.

#### Usage

    FilterStateExpr$set_state(state)

#### Arguments

- `state`:

  (`teal_slice`)

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Get reproducible call.

#### Usage

    FilterStateExpr$get_call(dataname)

#### Arguments

- `dataname`:

  (`ignored`) for a consistency with `FilterState`

  Returns reproducible condition call for current selection relevant for
  selected variable type. Method is using internal reactive values which
  makes it reactive and must be executed in reactive or isolated
  context.

#### Returns

`call` or `NULL`

------------------------------------------------------------------------

### Method `server()`

`shiny` module server.

#### Usage

    FilterStateExpr$server(id, remove_callback)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `remove_callback`:

  (`function`) callback to handle removal of this `FilterState` object
  from `state_list`

#### Returns

Reactive expression signaling that the remove button has been clicked.

------------------------------------------------------------------------

### Method `ui()`

`shiny` module UI. The UI for this class contains simple message stating
that it is not supported.

#### Usage

    FilterStateExpr$ui(id, parent_id = "cards")

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `parent_id`:

  (`character(1)`) id of the `FilterStates` card container.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterStateExpr$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
FilterStateExpr <- getFromNamespace("FilterStateExpr", "teal.slice")

filter_state <- FilterStateExpr$new(
  slice = teal_slice(
    dataname = "x",
    id = "FA",
    title = "Adult females",
    expr = "sex == 'F' & age >= 18"
  )
)
filter_state$get_call()
#> sex == "F" & age >= 18

# working filter in an app
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  bslib::layout_column_wrap(
    style = htmltools::css(grid_template_columns = "1fr 2fr"),
    tags$div(
      tags$h4("ChoicesFilterState"),
      filter_state$ui("fs")
    ),
    tags$div(
      tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
      textOutput("condition_choices"), tags$br(),
      tags$h4("Unformatted state"), # display raw filter state
      textOutput("unformatted_choices"), tags$br(),
      tags$h4("Formatted state"), # display human readable filter state
      textOutput("formatted_choices"), tags$br()
    )
  )
)

server <- function(input, output, session) {
  filter_state$server("fs")
  output$condition_choices <- renderPrint(filter_state$get_call())
  output$formatted_choices <- renderText(filter_state$format())
  output$unformatted_choices <- renderPrint(filter_state$get_state())
}

if (interactive()) {
  shinyApp(ui, server)
}
```
