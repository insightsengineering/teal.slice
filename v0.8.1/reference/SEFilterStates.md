# `FilterStates` subclass for `SummarizedExperiment`s

Handles filter states in a `SummaryExperiment`.

## Super class

[`FilterStates`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.md)
-\> `SEFilterStates`

## Methods

### Public methods

- [`SEFilterStates$new()`](#method-SEFilterStates-initialize)

- [`SEFilterStates$set_filter_state()`](#method-SEFilterStates-set_filter_state)

- [`SEFilterStates$ui_add()`](#method-SEFilterStates-ui_add)

- [`SEFilterStates$srv_add()`](#method-SEFilterStates-srv_add)

- [`SEFilterStates$clone()`](#method-SEFilterStates-clone)

Inherited methods

- [`FilterStates$clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-clear_filter_states)
- [`FilterStates$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-destroy)
- [`FilterStates$format()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-format)
- [`FilterStates$get_call()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-get_call)
- [`FilterStates$get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-get_filter_state)
- [`FilterStates$print()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-print)
- [`FilterStates$remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-remove_filter_state)
- [`FilterStates$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-srv_active)
- [`FilterStates$ui_active()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-ui_active)

------------------------------------------------------------------------

### `SEFilterStates$new()`

Initialize `SEFilterStates` object.

#### Usage

    SEFilterStates$new(
      data,
      data_reactive = function(sid = "") NULL,
      dataname,
      datalabel = NULL
    )

#### Arguments

- `data`:

  (`SummarizedExperiment`) the `R` object which `subset` function is
  applied on.

- `data_reactive`:

  (`function(sid)`) should return a `SummarizedExperiment` object or
  `NULL`. This object is needed for the `FilterState` counts being
  updated on a change in filters. If function returns `NULL` then
  filtered counts are not shown. Function has to have `sid` argument
  being a character.

- `dataname`:

  (`character(1)`) name of the data used in the expression specified to
  the function argument attached to this `FilterStates`.

- `datalabel`:

  (`character(1)`) optional text label. Should be the name of
  experiment.

------------------------------------------------------------------------

### `SEFilterStates$set_filter_state()`

Set filter state.

#### Usage

    SEFilterStates$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`) `teal_slice` objects should contain the field
  `arg %in% c("subset", "select")`

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `SEFilterStates$ui_add()`

`shiny` UI module to add filter variable.

#### Usage

    SEFilterStates$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### `SEFilterStates$srv_add()`

`shiny` server module to add filter variable.

Module controls available choices to select as a filter variable.
Selected filter variable is being removed from available choices.
Removed filter variable gets back to available choices. This module
unlike other `FilterStates` classes manages two sets of filter
variables - one for `colData` and another for `rowData`.

#### Usage

    SEFilterStates$srv_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`NULL`

------------------------------------------------------------------------

### `SEFilterStates$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SEFilterStates$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
