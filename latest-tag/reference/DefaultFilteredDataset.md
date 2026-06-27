# `DefaultFilteredDataset` `R6` class

Stores any object as inert entity. Filtering is not supported.

## Super class

[`FilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.md)
-\> `DefaultFilteredDataset`

## Methods

### Public methods

- [`DefaultFilteredDataset$new()`](#method-DefaultFilteredDataset-initialize)

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

- [`FilteredDataset$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-destroy)
- [`FilteredDataset$get_dataname()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataname)
- [`FilteredDataset$get_dataset()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset)
- [`FilteredDataset$get_dataset_label()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset_label)
- [`FilteredDataset$get_keys()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_keys)
- [`FilteredDataset$print()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-print)
- [`FilteredDataset$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_active)
- [`FilteredDataset$srv_add()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_add)

------------------------------------------------------------------------

### `DefaultFilteredDataset$new()`

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

### `DefaultFilteredDataset$format()`

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

### `DefaultFilteredDataset$get_call()`

#### Usage

    DefaultFilteredDataset$get_call(sid)

#### Arguments

- `sid`:

  (`character(1)`) for method consistency, ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `DefaultFilteredDataset$get_filter_state()`

#### Usage

    DefaultFilteredDataset$get_filter_state()

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `DefaultFilteredDataset$set_filter_state()`

#### Usage

    DefaultFilteredDataset$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`) for method consistency, ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `DefaultFilteredDataset$clear_filter_states()`

#### Usage

    DefaultFilteredDataset$clear_filter_states(force)

#### Arguments

- `force`:

  (`logical(1)`) for method consistency, ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `DefaultFilteredDataset$get_filter_overview()`

Creates row for filter overview in the form of\
`dataname` - unsupported data class

#### Usage

    DefaultFilteredDataset$get_filter_overview()

#### Returns

A `data.frame`.

------------------------------------------------------------------------

### `DefaultFilteredDataset$ui_active()`

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

### `DefaultFilteredDataset$ui_add()`

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

### `DefaultFilteredDataset$clone()`

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
