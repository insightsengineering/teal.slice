# Initialize `FilterState`

Initializes a `FilterState` object corresponding to the class of the
filtered variable.

## Usage

``` r
init_filter_state(
  x,
  x_reactive = reactive(NULL),
  slice,
  extract_type = character(0)
)
```

## Arguments

- x:

  (`vector`) variable to be filtered.

- x_reactive:

  (`reactive`) returning vector of the same type as `x`. Is used to
  update counts following the change in values of the filtered dataset.
  If it is set to `reactive(NULL)` then counts based on filtered dataset
  are not shown.

- slice:

  (`teal_slice`) specification of this filter state. `teal_slice` is
  stored in the object and `set_state` directly manipulates values
  within `teal_slice`. `get_state` returns `teal_slice` object which can
  be reused in other places. Note that `teal_slice` is a
  `reactiveValues`, which means it has reference semantics, i.e. changes
  made to an object are automatically reflected in all places that refer
  to the same `teal_slice`.

- extract_type:

  (`character`) specifying whether condition calls should be prefixed by
  `dataname`. Possible values:

  - `character(0)` (default) `varname` in the condition call will not be
    prefixed

  - `"list"` `varname` in the condition call will be returned as
    `<dataname>$<varname>`

  - `"matrix"` `varname` in the condition call will be returned as
    `<dataname>[, <varname>]`

## Value

`FilterState` object

## Examples

``` r
# use non-exported function from teal.slice
init_filter_state <- getFromNamespace("init_filter_state", "teal.slice")

library(shiny)

filter_state <- init_filter_state(
  x = c(1:10, NA, Inf),
  x_reactive = reactive(c(1:10, NA, Inf)),
  slice = teal_slice(
    varname = "varname",
    dataname = "dataname"
  ),
  extract_type = "matrix"
)

isolate(filter_state$get_call())
#> NULL

# working filter in an app

ui <- bslib::page_fluid(
  filter_state$ui(id = "app"),
  verbatimTextOutput("call")
)
server <- function(input, output, session) {
  filter_state$server("app")

  output$call <- renderText(
    deparse1(filter_state$get_call(), collapse = "\n")
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
```
