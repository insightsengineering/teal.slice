# `FilteredDataset` `R6` class

`FilteredDataset` is a class which renders/controls `FilterStates`(s)
Each `FilteredDataset` contains `filter_states` field - a `list` which
contains one (`data.frame`) or multiple (`MultiAssayExperiment`)
`FilterStates` objects. Each `FilterStates` is responsible for one
filter/subset expression applied for specific components of the dataset.

## Methods

### Public methods

- [`FilteredDataset$new()`](#method-FilteredDataset-new)

- [`FilteredDataset$destroy()`](#method-FilteredDataset-destroy)

- [`FilteredDataset$format()`](#method-FilteredDataset-format)

- [`FilteredDataset$print()`](#method-FilteredDataset-print)

- [`FilteredDataset$clear_filter_states()`](#method-FilteredDataset-clear_filter_states)

- [`FilteredDataset$get_call()`](#method-FilteredDataset-get_call)

- [`FilteredDataset$get_filter_state()`](#method-FilteredDataset-get_filter_state)

- [`FilteredDataset$set_filter_state()`](#method-FilteredDataset-set_filter_state)

- [`FilteredDataset$get_dataname()`](#method-FilteredDataset-get_dataname)

- [`FilteredDataset$get_dataset()`](#method-FilteredDataset-get_dataset)

- [`FilteredDataset$get_filter_overview()`](#method-FilteredDataset-get_filter_overview)

- [`FilteredDataset$get_keys()`](#method-FilteredDataset-get_keys)

- [`FilteredDataset$get_dataset_label()`](#method-FilteredDataset-get_dataset_label)

- [`FilteredDataset$ui_active()`](#method-FilteredDataset-ui_active)

- [`FilteredDataset$srv_active()`](#method-FilteredDataset-srv_active)

- [`FilteredDataset$ui_add()`](#method-FilteredDataset-ui_add)

- [`FilteredDataset$srv_add()`](#method-FilteredDataset-srv_add)

- [`FilteredDataset$clone()`](#method-FilteredDataset-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initializes this `FilteredDataset` object.

#### Usage

    FilteredDataset$new(
      dataset,
      dataname,
      keys = character(0),
      label = attr(dataset, "label", exact = TRUE)
    )

#### Arguments

- `dataset`:

  any object

- `dataname`:

  (`character(1)`) syntactically valid name given to the dataset.

- `keys`:

  (`character`) optional vector of primary key column names.

- `label`:

  (`character(1)`) label to describe the dataset.

#### Returns

Object of class `FilteredDataset`, invisibly.

------------------------------------------------------------------------

### Method `destroy()`

Destroys a `FilteredDataset` object.

#### Usage

    FilteredDataset$destroy()

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Returns a formatted string representing this `FilteredDataset` object.

#### Usage

    FilteredDataset$format(show_all = FALSE, trim_lines = TRUE)

#### Arguments

- `show_all`:

  (`logical(1)`) passed to `format.teal_slice`.

- `trim_lines`:

  (`logical(1)`) passed to `format.teal_slice`.

#### Returns

The formatted character string.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints this `FilteredDataset` object.

#### Usage

    FilteredDataset$print(...)

#### Arguments

- `...`:

  additional arguments passed to `format`.

------------------------------------------------------------------------

### Method [`clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Removes all filter items applied to this dataset.

#### Usage

    FilteredDataset$clear_filter_states(force = FALSE)

#### Arguments

- `force`:

  (`logical(1)`) flag specifying whether to include anchored filter
  states.

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `get_call()`

Gets a filter expression.

This function returns filter calls equivalent to selected items within
each of `filter_states`. Configuration of the calls is constant and
depends on `filter_states` type and order which are set during
initialization.

#### Usage

    FilteredDataset$get_call(sid = "")

#### Arguments

- `sid`:

  (`character`) when specified, the method returns code containing
  conditions calls of `FilterState` objects with `sid` different to this
  `sid` argument.

#### Returns

Either a `list` of filter `call`s, or `NULL`.

------------------------------------------------------------------------

### Method [`get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Gets states of all contained `FilterState` objects.

#### Usage

    FilteredDataset$get_filter_state()

#### Returns

A `teal_slices` object.

------------------------------------------------------------------------

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Set filter state.

#### Usage

    FilteredDataset$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`)

#### Returns

Virtual method, returns nothing and raises error.

------------------------------------------------------------------------

### Method `get_dataname()`

Gets the name of the dataset.

#### Usage

    FilteredDataset$get_dataname()

#### Returns

A character string.

------------------------------------------------------------------------

### Method `get_dataset()`

Gets the dataset object in this `FilteredDataset`.

#### Usage

    FilteredDataset$get_dataset(filtered = FALSE)

#### Arguments

- `filtered`:

  (`logical(1)`)

#### Returns

The stored dataset. If `data.frame` or `MultiAssayExperiment`, either
raw or as a reactive with current filters applied (depending on
`filtered`).

------------------------------------------------------------------------

### Method `get_filter_overview()`

Get filter overview of a dataset.

#### Usage

    FilteredDataset$get_filter_overview()

#### Returns

Virtual method, returns nothing and raises an error.

------------------------------------------------------------------------

### Method `get_keys()`

Gets the key columns for this dataset.

#### Usage

    FilteredDataset$get_keys()

#### Returns

Character vector of variable names

------------------------------------------------------------------------

### Method `get_dataset_label()`

Gets the dataset label.

#### Usage

    FilteredDataset$get_dataset_label()

#### Returns

Character string.

------------------------------------------------------------------------

### Method `ui_active()`

`shiny` module containing active filters for a dataset, along with a
title and a remove button.

#### Usage

    FilteredDataset$ui_active(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### Method `srv_active()`

Server module for a dataset active filters.

#### Usage

    FilteredDataset$srv_active(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `ui_add()`

UI module to add filter variable for this dataset.

#### Usage

    FilteredDataset$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

Virtual method, returns nothing and raises error.

------------------------------------------------------------------------

### Method `srv_add()`

Server module to add filter variable for this dataset. For this class
`srv_add` calls multiple modules of the same name from `FilterStates` as
`MAEFilteredDataset` contains one `FilterStates` object for `colData`
and one for each experiment.

#### Usage

    FilteredDataset$srv_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilteredDataset$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
