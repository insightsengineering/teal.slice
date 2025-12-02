# `FilterStates` subclass for matrices

Handles filter states in a `matrix`.

## Super class

[`teal.slice::FilterStates`](https://insightsengineering.github.io/teal.slice/reference/FilterStates.md)
-\> `MatrixFilterStates`

## Methods

### Public methods

- [`MatrixFilterStates$new()`](#method-MatrixFilterStates-new)

- [`MatrixFilterStates$clone()`](#method-MatrixFilterStates-clone)

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

Initialize `MatrixFilterStates` object.

#### Usage

    MatrixFilterStates$new(
      data,
      data_reactive = function(sid = "") NULL,
      dataname,
      datalabel = NULL
    )

#### Arguments

- `data`:

  (`matrix`) the `R` object which `subset` function is applied on.

- `data_reactive`:

  (`function(sid)`) should return a `matrix` object or `NULL`. This
  object is needed for the `FilterState` counts being updated on a
  change in filters. If function returns `NULL` then filtered counts are
  not shown. Function has to have `sid` argument being a character.

- `dataname`:

  (`character(1)`) name of the data used in the subset expression.
  Passed to the function argument attached to this `FilterStates`.

- `datalabel`:

  (`character(1)`) optional text label. Should be a name of experiment.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MatrixFilterStates$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
