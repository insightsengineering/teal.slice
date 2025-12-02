# `DefaultFilteredDataset` `R6` class

Stores any object as inert entity. Filtering is not supported.

## Super class

[`teal.slice::FilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.md)
-\> `DefaultFilteredDataset`

## Methods

### Public methods

- [`DefaultFilteredDataset$new()`](#method-DefaultFilteredDataset-new)

- [`DefaultFilteredDataset$format()`](#method-DefaultFilteredDataset-format)

- [`DefaultFilteredDataset$get_call()`](#method-DefaultFilteredDataset-get_call)

- [`DefaultFilteredDataset$get_filter_state()`](#method-DefaultFilteredDataset-get_filter_state)

- [`DefaultFilteredDataset$set_filter_state()`](#method-DefaultFilteredDataset-set_filter_state)

- [`DefaultFilteredDataset$clear_filter_states()`](#method-DefaultFilteredDataset-clear_filter_states)

- [`DefaultFilteredDataset$get_filter_overview()`](#method-DefaultFilteredDataset-get_filter_overview)

- [`DefaultFilteredDataset$ui_active()`](#method-DefaultFilteredDataset-ui_active)

- [`DefaultFilteredDataset$ui_add()`](#method-DefaultFilteredDataset-ui_add)

- [`DefaultFilteredDataset$clone()`](#method-DefaultFilteredDataset-clone)

Inherited methods

- [`teal.slice::FilteredDataset$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-destroy)
- [`teal.slice::FilteredDataset$get_dataname()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataname)
- [`teal.slice::FilteredDataset$get_dataset()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset)
- [`teal.slice::FilteredDataset$get_dataset_label()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset_label)
- [`teal.slice::FilteredDataset$get_keys()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_keys)
- [`teal.slice::FilteredDataset$print()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-print)
- [`teal.slice::FilteredDataset$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_active)
- [`teal.slice::FilteredDataset$srv_add()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_add)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initializes this `DefaultFilteredDataset` object.

#### Usage

    DefaultFilteredDataset$new(dataset, dataname, label = character(0))

#### Arguments

- `dataset`:

  any type of object; will not be filtered.

- `dataname`:

  (`character(1)`) syntactically valid name given to the dataset.

- `label`:

  (`character(1)`) label to describe the dataset.

#### Returns

Object of class `DefaultFilteredDataset`, invisibly.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Returns a formatted string representing this `DefaultFilteredDataset`
object.

#### Usage

    DefaultFilteredDataset$format(show_all, trim_lines = FALSE)

#### Arguments

- `show_all`:

  (`logical(1)`) for method consistency, ignored.

- `trim_lines`:

  (`logical(1)`) flag specifying whether to trim lines if class names
  are too long.

#### Returns

The formatted string.

------------------------------------------------------------------------

### Method `get_call()`

#### Usage

    DefaultFilteredDataset$get_call(sid)

#### Arguments

- `sid`:

  (`character(1)`) for method consistency, ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

#### Usage

    DefaultFilteredDataset$get_filter_state()

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

#### Usage

    DefaultFilteredDataset$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`) for method consistency, ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

#### Usage

    DefaultFilteredDataset$clear_filter_states(force)

#### Arguments

- `force`:

  (`logical(1)`) for method consistency, ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `get_filter_overview()`

Creates row for filter overview in the form of  
`dataname` - unsupported data class

#### Usage

    DefaultFilteredDataset$get_filter_overview()

#### Returns

A `data.frame`.

------------------------------------------------------------------------

### Method `ui_active()`

Overwrites parent method.

#### Usage

    DefaultFilteredDataset$ui_active(id, allow_add)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `allow_add`:

  (ignored)

#### Details

Blank UI module that would list active filter states for this dataset.

#### Returns

An empty `div`.

------------------------------------------------------------------------

### Method `ui_add()`

Overwrites parent method.

#### Usage

    DefaultFilteredDataset$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Details

Blank UI module that would list active filter states for this dataset.

#### Returns

An empty `div`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DefaultFilteredDataset$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
DefaultFilteredDataset <- getFromNamespace("DefaultFilteredDataset", "teal.slice")

library(shiny)

ds <- DefaultFilteredDataset$new(letters, "letters")
isolate(ds$get_filter_state())
isolate(ds$get_call())
```
