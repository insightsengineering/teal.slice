# `FilterStates` subclass for `SummarizedExperiment`s

Handles filter states in a `SummaryExperiment`.

## Super class

[`teal.slice::FilterStates`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.md)
-\> `SEFilterStates`

## Methods

### Public methods

- [`SEFilterStates$new()`](#method-SEFilterStates-new)

- [`SEFilterStates$set_filter_state()`](#method-SEFilterStates-set_filter_state)

- [`SEFilterStates$ui_add()`](#method-SEFilterStates-ui_add)

- [`SEFilterStates$srv_add()`](#method-SEFilterStates-srv_add)

- [`SEFilterStates$clone()`](#method-SEFilterStates-clone)

Inherited methods

- [`teal.slice::FilterStates$clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-clear_filter_states)
- [`teal.slice::FilterStates$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-destroy)
- [`teal.slice::FilterStates$format()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-format)
- [`teal.slice::FilterStates$get_call()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-get_call)
- [`teal.slice::FilterStates$get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-get_filter_state)
- [`teal.slice::FilterStates$print()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-print)
- [`teal.slice::FilterStates$remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-remove_filter_state)
- [`teal.slice::FilterStates$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-srv_active)
- [`teal.slice::FilterStates$ui_active()`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.html#method-ui_active)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

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

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

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

### Method `ui_add()`

`shiny` UI module to add filter variable.

#### Usage

    SEFilterStates$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### Method `srv_add()`

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SEFilterStates$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
