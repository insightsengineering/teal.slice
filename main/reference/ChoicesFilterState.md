# `FilterState` object for categorical data

Manages choosing elements from a set.

## Super class

[`teal.slice::FilterState`](https://insightsengineering.github.io/teal.slice/reference/FilterState.md)
-\> `ChoicesFilterState`

## Methods

### Public methods

- [`ChoicesFilterState$new()`](#method-ChoicesFilterState-new)

- [`ChoicesFilterState$get_call()`](#method-ChoicesFilterState-get_call)

- [`ChoicesFilterState$clone()`](#method-ChoicesFilterState-clone)

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

    ChoicesFilterState$new(
      x,
      x_reactive = reactive(NULL),
      slice,
      extract_type = character(0)
    )

#### Arguments

- `x`:

  (`character`) variable to be filtered.

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

Object of class `ChoicesFilterState`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Returns reproducible condition call for current selection. For this
class returned call looks like `<varname> %in% c(<values selected>)`
with optional `is.na(<varname>)`.

#### Usage

    ChoicesFilterState$get_call(dataname)

#### Arguments

- `dataname`:

  (`character(1)`) name of data set; defaults to
  `private$get_dataname()`

#### Returns

`call` or `NULL`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ChoicesFilterState$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
ChoicesFilterState <- getFromNamespace("ChoicesFilterState", "teal.slice")

library(shiny)

filter_state <- ChoicesFilterState$new(
  x = c(LETTERS, NA),
  slice = teal_slice(varname = "var", dataname = "data")
)
isolate(filter_state$get_call())
#> NULL
filter_state$set_state(
  teal_slice(
    dataname = "data",
    varname = "var",
    selected = "A",
    keep_na = TRUE
  )
)
isolate(filter_state$get_call())
#> is.na(var) | var == "A"

# working filter in an app
library(shinyjs)
#> 
#> Attaching package: ‘shinyjs’
#> The following object is masked from ‘package:shiny’:
#> 
#>     runExample
#> The following objects are masked from ‘package:methods’:
#> 
#>     removeClass, show

data_choices <- c(sample(letters[1:4], 100, replace = TRUE), NA)
attr(data_choices, "label") <- "lowercase letters"
fs <- ChoicesFilterState$new(
  x = data_choices,
  slice = teal_slice(
    dataname = "data", varname = "variable", selected = c("a", "c"), keep_na = TRUE
  )
)

ui <- bslib::page_fluid(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  bslib::layout_column_wrap(
    width = 1 / 3,
    tags$div(
      tags$h4("ChoicesFilterState"),
      fs$ui("fs")
    ),
    tags$div(
      tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
      textOutput("condition_choices"), tags$br(),
      tags$h4("Unformatted state"), # display raw filter state
      textOutput("unformatted_choices"), tags$br(),
      tags$h4("Formatted state"), # display human readable filter state
      textOutput("formatted_choices"), tags$br()
    ),
    tags$div(
      tags$h4("Programmatic filter control"),
      actionButton("button1_choices", "set drop NA", width = "100%"), tags$br(),
      actionButton("button2_choices", "set keep NA", width = "100%"), tags$br(),
      actionButton("button3_choices", "set selection: a, b", width = "100%"), tags$br(),
      actionButton("button4_choices", "deselect all", width = "100%"), tags$br(),
      actionButton("button0_choices", "set initial state", width = "100%"), tags$br()
    )
  )
)

server <- function(input, output, session) {
  fs$server("fs")
  output$condition_choices <- renderPrint(fs$get_call())
  output$formatted_choices <- renderText(fs$format())
  output$unformatted_choices <- renderPrint(fs$get_state())
  # modify filter state programmatically
  observeEvent(
    input$button1_choices,
    fs$set_state(
      teal_slice(dataname = "data", varname = "variable", keep_na = FALSE)
    )
  )
  observeEvent(
    input$button2_choices,
    fs$set_state(
      teal_slice(dataname = "data", varname = "variable", keep_na = TRUE)
    )
  )
  observeEvent(
    input$button3_choices,
    fs$set_state(
      teal_slice(dataname = "data", varname = "variable", selected = c("a", "b"))
    )
  )
  observeEvent(
    input$button4_choices,
    fs$set_state(
      teal_slice(dataname = "data", varname = "variable", selected = character(0), keep_na = TRUE)
    )
  )
  observeEvent(
    input$button0_choices,
    fs$set_state(
      teal_slice(dataname = "data", varname = "variable", selected = c("a", "c"), keep_na = TRUE)
    )
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
```
