# `FilterState` object for empty variables

`FilterState` subclass representing an empty variable.

## Super class

[`teal.slice::FilterState`](https://insightsengineering.github.io/teal.slice/reference/FilterState.md)
-\> `EmptyFilterState`

## Methods

### Public methods

- [`EmptyFilterState$new()`](#method-EmptyFilterState-new)

- [`EmptyFilterState$get_call()`](#method-EmptyFilterState-get_call)

- [`EmptyFilterState$clone()`](#method-EmptyFilterState-clone)

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

Initialize `EmptyFilterState` object.

#### Usage

    EmptyFilterState$new(
      x,
      x_reactive = reactive(NULL),
      extract_type = character(0),
      slice
    )

#### Arguments

- `x`:

  (`vector`) variable to be filtered,

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

Object of class `EmptyFilterState`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Returns reproducible condition call for current selection relevant for
selected variable type. Uses internal reactive values, hence must be
called in reactive or isolated context.

#### Usage

    EmptyFilterState$get_call(dataname)

#### Arguments

- `dataname`:

  name of data set; defaults to `private$get_dataname()`

#### Returns

`logical(1)`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EmptyFilterState$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
include_js_files <- getFromNamespace("include_js_files", "teal.slice")
EmptyFilterState <- getFromNamespace("EmptyFilterState", "teal.slice")

library(shiny)

filter_state <- EmptyFilterState$new(
  x = NA,
  slice = teal_slice(varname = "x", dataname = "data"),
  extract_type = character(0)
)
isolate(filter_state$get_call())
#> NULL
filter_state$set_state(teal_slice(dataname = "data", varname = "x", keep_na = TRUE))
isolate(filter_state$get_call())
#> NULL
```
