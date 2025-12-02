# `FilterStates` subclass for data frames

Handles filter states in a `data.frame`.

## Super class

[`teal.slice::FilterStates`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.md)
-\> `DFFilterStates`

## Methods

### Public methods

- [`DFFilterStates$new()`](#method-DFFilterStates-new)

- [`DFFilterStates$clone()`](#method-DFFilterStates-clone)

Inherited methods

- [`teal.slice::FilterStates$clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-clear_filter_states)
- [`teal.slice::FilterStates$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-destroy)
- [`teal.slice::FilterStates$format()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-format)
- [`teal.slice::FilterStates$get_call()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-get_call)
- [`teal.slice::FilterStates$get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-get_filter_state)
- [`teal.slice::FilterStates$print()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-print)
- [`teal.slice::FilterStates$remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-remove_filter_state)
- [`teal.slice::FilterStates$set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-set_filter_state)
- [`teal.slice::FilterStates$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-srv_active)
- [`teal.slice::FilterStates$srv_add()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-srv_add)
- [`teal.slice::FilterStates$ui_active()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-ui_active)
- [`teal.slice::FilterStates$ui_add()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-ui_add)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initializes `DFFilterStates` object by setting `dataname` and
initializing `state_list`
([`shiny::reactiveVal`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)).
This class contains a single `state_list` with no specified name, which
means that when calling the subset function associated with this class
([`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)),
a list of conditions is passed to unnamed arguments (`...`).

#### Usage

    DFFilterStates$new(
      data,
      data_reactive = function(sid = "") NULL,
      dataname,
      datalabel = NULL,
      keys = character(0)
    )

#### Arguments

- `data`:

  (`data.frame`) the `R` object which
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  function will be applied on.

- `data_reactive`:

  (`function(sid)`) should return a `data.frame` object or `NULL`. This
  object is needed for the `FilterState` counts being updated on a
  change in filters. If function returns `NULL` then filtered counts are
  not shown. Function has to have `sid` argument being a character.

- `dataname`:

  (`character`) name of the data used in the *subset expression*. Passed
  to the function argument attached to this `FilterStates`.

- `datalabel`:

  (`character(1)`) optional text label.

- `keys`:

  (`character`) key column names.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DFFilterStates$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
include_css_files <- getFromNamespace("include_css_files", "teal.slice")
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
init_filter_states <- getFromNamespace("init_filter_states", "teal.slice")

library(shiny)
library(shinyjs)

# create data frame to filter
data_df <- data.frame(
  NUM1 = 1:100,
  NUM2 = round(runif(100, min = 20, max = 23)),
  CHAR1 = sample(LETTERS[1:6], size = 100, replace = TRUE),
  CHAR2 = sample(c("M", "F"), size = 100, replace = TRUE),
  DATE = seq(as.Date("2020-01-01"), by = 1, length.out = 100),
  DATETIME = as.POSIXct(seq(as.Date("2020-01-01"), by = 1, length.out = 100))
)
data_na <- data.frame(
  NUM1 = NA,
  NUM2 = NA,
  CHAR1 = NA,
  CHAR2 = NA,
  DATE = NA,
  DATETIME = NA
)
data_df <- rbind(data_df, data_na)

# initiate `FilterStates` object
filter_states_df <- init_filter_states(
  data = data_df,
  dataname = "dataset",
  datalabel = ("label")
)

ui <- bslib::page_fluid(
  useShinyjs(),
  include_css_files(pattern = "filter-panel"),
  include_js_files(pattern = "count-bar-labels"),
  bslib::layout_column_wrap(
    width = 1 / 3,
    tags$div(
      tags$h4("Active filters"),
      filter_states_df$ui_active("fsdf")
    ),
    tags$div(
      tags$h4("Manual filter control"),
      filter_states_df$ui_add("add_filters"), tags$br(),
      tags$h4("Condition (i.e. call)"), # display the subset expression generated by FilterStates
      textOutput("call_df"), tags$br(),
      tags$h4("Formatted state"), # display human readable filter state
      textOutput("formatted_df"), tags$br()
    ),
    tags$div(
      tags$h4("Programmatic filter control"),
      actionButton("button1_df", "set NUM1 < 30", width = "100%"), tags$br(),
      actionButton("button2_df", "set NUM2 %in% c(20, 21)", width = "100%"), tags$br(),
      actionButton("button3_df", "set CHAR1 %in% c(\"B\", \"C\", \"D\")", width = "100%"),
      tags$br(),
      actionButton("button4_df", "set CHAR2 == \"F\"", width = "100%"), tags$br(),
      actionButton("button5_df", "set DATE <= 2020-02-02", width = "100%"), tags$br(),
      actionButton("button6_df", "set DATETIME <= 2020-02-02", width = "100%"), tags$br(),
      tags$hr(),
      actionButton("button7_df", "remove NUM1", width = "100%"), tags$br(),
      actionButton("button8_df", "remove NUM2", width = "100%"), tags$br(),
      actionButton("button9_df", "remove CHAR1", width = "100%"), tags$br(),
      actionButton("button10_df", "remove CHAR2", width = "100%"), tags$br(),
      actionButton("button11_df", "remove DATE", width = "100%"), tags$br(),
      actionButton("button12_df", "remove DATETIME", width = "100%"), tags$br(),
      tags$hr(),
      actionButton("button0_df", "clear all filters", width = "100%"), tags$br()
    )
  )
)

server <- function(input, output, session) {
  filter_states_df$srv_add("add_filters")
  filter_states_df$srv_active("fsdf")

  output$call_df <- renderPrint(filter_states_df$get_call())
  output$formatted_df <- renderText(filter_states_df$format())

  observeEvent(input$button1_df, {
    filter_state <- teal_slices(teal_slice("dataset", "NUM1", selected = c(0, 30)))
    filter_states_df$set_filter_state(state = filter_state)
  })
  observeEvent(input$button2_df, {
    filter_state <- teal_slices(teal_slice("dataset", "NUM2", selected = c(20, 21)))
    filter_states_df$set_filter_state(state = filter_state)
  })
  observeEvent(input$button3_df, {
    filter_state <- teal_slices(teal_slice("dataset", "CHAR1", selected = c("B", "C", "D")))
    filter_states_df$set_filter_state(state = filter_state)
  })
  observeEvent(input$button4_df, {
    filter_state <- teal_slices(teal_slice("dataset", "CHAR2", selected = c("F")))
    filter_states_df$set_filter_state(state = filter_state)
  })
  observeEvent(input$button5_df, {
    filter_state <- teal_slices(
      teal_slice("dataset", "DATE", selected = c("2020-01-01", "2020-02-02"))
    )
    filter_states_df$set_filter_state(state = filter_state)
  })
  observeEvent(input$button6_df, {
    filter_state <- teal_slices(
      teal_slice("dataset", "DATETIME", selected = as.POSIXct(c("2020-01-01", "2020-02-02")))
    )
    filter_states_df$set_filter_state(state = filter_state)
  })

  observeEvent(input$button7_df, {
    filter_state <- teal_slices(teal_slice("dataset", "NUM1"))
    filter_states_df$remove_filter_state(filter_state)
  })
  observeEvent(input$button8_df, {
    filter_state <- teal_slices(teal_slice("dataset", "NUM2"))
    filter_states_df$remove_filter_state(filter_state)
  })
  observeEvent(input$button9_df, {
    filter_state <- teal_slices(teal_slice("dataset", "CHAR1"))
    filter_states_df$remove_filter_state(filter_state)
  })
  observeEvent(input$button10_df, {
    filter_state <- teal_slices(teal_slice("dataset", "CHAR2"))
    filter_states_df$remove_filter_state(filter_state)
  })
  observeEvent(input$button11_df, {
    filter_state <- teal_slices(
      teal_slice("dataset", "DATE")
    )
    filter_states_df$remove_filter_state(filter_state)
  })
  observeEvent(input$button12_df, {
    filter_state <- teal_slices(
      teal_slice("dataset", "DATETIME", selected = as.POSIXct(c("2020-01-01", "2020-02-02")))
    )
    filter_states_df$remove_filter_state(filter_state)
  })

  observeEvent(input$button0_df, filter_states_df$clear_filter_states())
}

if (interactive()) {
  shinyApp(ui, server)
}
```
