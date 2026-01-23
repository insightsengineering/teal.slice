# `FilterState` object for date time data

Manages choosing a range of date-times.

## Super class

[`teal.slice::FilterState`](https://insightsengineering.github.io/teal.slice/reference/FilterState.md)
-\> `DatetimeFilterState`

## Methods

### Public methods

- [`DatetimeFilterState$new()`](#method-DatetimeFilterState-new)

- [`DatetimeFilterState$get_call()`](#method-DatetimeFilterState-get_call)

- [`DatetimeFilterState$clone()`](#method-DatetimeFilterState-clone)

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

Initialize a `FilterState` object. This class has an extra field,
`private$timezone`, which is set to
[`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) by default.
However, in case when using this module in `teal` app, one needs
timezone of the app user. App user timezone is taken from
`session$userData$timezone` and is set only if object is initialized in
`shiny`.

#### Usage

    DatetimeFilterState$new(
      x,
      x_reactive = reactive(NULL),
      extract_type = character(0),
      slice
    )

#### Arguments

- `x`:

  (`POSIXct` or `POSIXlt`) variable to be filtered.

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

Object of class `DatetimeFilterState`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Returns reproducible condition call for current selection. For this
class returned call looks like
`<varname> >= as.POSIXct(<min>) & <varname> <= <max>)` with optional
`is.na(<varname>)`.

#### Usage

    DatetimeFilterState$get_call(dataname)

#### Arguments

- `dataname`:

  name of data set; defaults to `private$get_dataname()`

#### Returns

`call`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatetimeFilterState$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
DatetimeFilterState <- getFromNamespace("DatetimeFilterState", "teal.slice")

library(shiny)

filter_state <- DatetimeFilterState$new(
  x = c(Sys.time() + seq(0, by = 3600, length.out = 10), NA),
  slice = teal_slice(varname = "x", dataname = "data"),
  extract_type = character(0)
)
isolate(filter_state$get_call())
#> is.na(x) | x >= as.POSIXct("2026-01-23 13:27:36", tz = "Etc/UTC") & 
#>     x < as.POSIXct("2026-01-23 22:27:37", tz = "Etc/UTC")
filter_state$set_state(
  teal_slice(
    dataname = "data",
    varname = "x",
    selected = c(Sys.time() + 3L, Sys.time() + 8L),
    keep_na = TRUE
  )
)
isolate(filter_state$get_call())
#> is.na(x) | x >= as.POSIXct("2026-01-23 13:27:39.285126", tz = "Etc/UTC") & 
#>     x < as.POSIXct("2026-01-23 13:27:45.285166", tz = "Etc/UTC")

# working filter in an app
library(shinyjs)

datetimes <- as.POSIXct(c("2012-01-01 12:00:00", "2020-01-01 12:00:00"))
data_datetime <- c(seq(from = datetimes[1], to = datetimes[2], length.out = 100), NA)
fs <- DatetimeFilterState$new(
  x = data_datetime,
  slice = teal_slice(
    varname = "x", dataname = "data", selected = data_datetime[c(47, 98)], keep_na = TRUE
  )
)

ui <- bslib::page_fluid(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  bslib::layout_column_wrap(
    width = 1 / 3,
    tags$div(
      tags$h4("DatetimeFilterState"),
      fs$ui("fs")
    ),
    tags$div(
      id = "outputs", # div id is needed for toggling the element
      tags$h4("Condition (i.e. call)"), # display the condition call generated by this FilterState
      textOutput("condition_datetime"), tags$br(),
      tags$h4("Unformatted state"), # display raw filter state
      textOutput("unformatted_datetime"), tags$br(),
      tags$h4("Formatted state"), # display human readable filter state
      textOutput("formatted_datetime"), tags$br()
    ),
    tags$div(
      tags$h4("Programmatic filter control"),
      actionButton("button1_datetime", "set drop NA", width = "100%"), tags$br(),
      actionButton("button2_datetime", "set keep NA", width = "100%"), tags$br(),
      actionButton("button3_datetime", "set a range", width = "100%"), tags$br(),
      actionButton("button4_datetime", "set full range", width = "100%"), tags$br(),
      actionButton("button0_datetime", "set initial state", width = "100%"), tags$br()
    )
  )
)

server <- function(input, output, session) {
  fs$server("fs")
  output$condition_datetime <- renderPrint(fs$get_call())
  output$formatted_datetime <- renderText(fs$format())
  output$unformatted_datetime <- renderPrint(fs$get_state())
  # modify filter state programmatically
  observeEvent(
    input$button1_datetime,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = FALSE))
  )
  observeEvent(
    input$button2_datetime,
    fs$set_state(teal_slice(dataname = "data", varname = "x", keep_na = TRUE))
  )
  observeEvent(
    input$button3_datetime,
    fs$set_state(
      teal_slice(dataname = "data", varname = "x", selected = data_datetime[c(34, 56)])
    )
  )
  observeEvent(
    input$button4_datetime,
    fs$set_state(
      teal_slice(dataname = "data", varname = "x", selected = datetimes)
    )
  )
  observeEvent(
    input$button0_datetime,
    fs$set_state(
      teal_slice(
        dataname = "data", varname = "x", selected = data_datetime[c(47, 98)], keep_na = TRUE
      )
    )
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
```
