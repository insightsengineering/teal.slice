# `FilterState` abstract class

Abstract class to encapsulate single filter state.

## Details

This class is responsible for managing a single filter item within a
`FilteredData` object and outputs a condition call (logical predicate)
for subsetting one variable. Filter states depend on the variable type:
(`logical`, `integer`, `numeric`, `character`, `factor`, `Date`,
`POSIXct`, `POSIXlt`) and `FilterState` subclasses exist that correspond
to those types.

- `logical`: `class = LogicalFilterState`

- `integer`: `class = RangeFilterState`

- `numeric`: `class = RangeFilterState`

- `character`: `class = ChoicesFilterState`

- `factor`: `class = ChoicesFilterState`

- `Date`: `class = DateFilterState`

- `POSIXct`, `POSIXlt`: `class = DatetimeFilterState`

- all `NA` entries: `class: FilterState`, cannot be filtered

- default: `FilterState`, cannot be filtered

Each variable's filter state is an `R6` object keeps the variable that
is filtered, a `teal_slice` object that describes the filter state, as
well as a `shiny` module (UI and server) that allows the user to alter
the filter state. Changes to the filter state that cause some
observations to be omitted trigger the `get_call` method and every `R`
function call up in the reactive chain.

## Modifying state

Modifying a `FilterState` object is possible in three scenarios:

- In an interactive session, by passing an appropriate `teal_slice` to
  the `set_state` method.

- In a running application, by changing appropriate inputs.

- In a running application, by using
  [filter_state_api](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)
  which directly uses `set_state` method of the `FilterState` object.

## Methods

### Public methods

- [`FilterState$new()`](#method-FilterState-new)

- [`FilterState$destroy()`](#method-FilterState-destroy)

- [`FilterState$format()`](#method-FilterState-format)

- [`FilterState$print()`](#method-FilterState-print)

- [`FilterState$set_state()`](#method-FilterState-set_state)

- [`FilterState$get_state()`](#method-FilterState-get_state)

- [`FilterState$get_call()`](#method-FilterState-get_call)

- [`FilterState$server()`](#method-FilterState-server)

- [`FilterState$ui()`](#method-FilterState-ui)

- [`FilterState$clone()`](#method-FilterState-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize a `FilterState` object.

#### Usage

    FilterState$new(
      x,
      x_reactive = reactive(NULL),
      slice,
      extract_type = character(0)
    )

#### Arguments

- `x`:

  (`vector`) variable to be filtered.

- `x_reactive`:

  (`reactive`) returning vector of the same type as `x`. Is used to
  update counts following the change in values of the filtered dataset.
  If it is set to `reactive(NULL)` then counts based on filtered dataset
  are not shown.

- `slice`:

  (`teal_slice`) specification of this filter state. `teal_slice` is
  stored in the object and `set_state` directly manipulates values
  within `teal_slice`. `get_state` returns `teal_slice` object which can
  be reused in other places. Note that `teal_slice` is a
  `reactiveValues`, which means it has reference semantics, i.e. changes
  made to an object are automatically reflected in all places that refer
  to the same `teal_slice`.

- `extract_type`:

  (`character`) specifying whether condition calls should be prefixed by
  `dataname`. Possible values:

  - `character(0)` (default) `varname` in the condition call will not be
    prefixed

  - `"list"` `varname` in the condition call will be returned as
    `<dataname>$<varname>`

  - `"matrix"` `varname` in the condition call will be returned as
    `<dataname>[, <varname>]`

#### Returns

Object of class `FilterState`, invisibly.

------------------------------------------------------------------------

### Method `destroy()`

Destroys a `FilterState` object.

#### Usage

    FilterState$destroy()

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Returns a formatted string representing this `FilterState` object.

#### Usage

    FilterState$format(show_all = FALSE, trim_lines = TRUE)

#### Arguments

- `show_all`:

  (`logical(1)`) passed to `format.teal_slice`

- `trim_lines`:

  (`logical(1)`) passed to `format.teal_slice`

#### Returns

`character(1)` the formatted string

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints this `FilterState` object.

#### Usage

    FilterState$print(...)

#### Arguments

- `...`:

  additional arguments

------------------------------------------------------------------------

### Method `set_state()`

Sets mutable parameters of the filter state.

- `fixed` state is prevented from changing state

- `anchored` state is prevented from removing state

#### Usage

    FilterState$set_state(state)

#### Arguments

- `state`:

  (`teal_slice`)

#### Returns

`self` invisibly

------------------------------------------------------------------------

### Method `get_state()`

Returns a complete description of the filter state.

#### Usage

    FilterState$get_state()

#### Returns

A `teal_slice` object.

------------------------------------------------------------------------

### Method `get_call()`

Returns reproducible condition call for current selection relevant for
selected variable type. Method is using internal reactive values which
makes it reactive and must be executed in reactive or isolated context.

#### Usage

    FilterState$get_call()

------------------------------------------------------------------------

### Method `server()`

`shiny` module server.

#### Usage

    FilterState$server(id, remove_callback)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `remove_callback`:

  (`function`) callback to handle removal of this `FilterState` object
  from `state_list`

#### Returns

Reactive expression signaling that remove button has been clicked.

------------------------------------------------------------------------

### Method `ui()`

`shiny` UI module. The UI for this class contains simple message stating
that it is not supported.

#### Usage

    FilterState$ui(id, parent_id = "cards")

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `parent_id`:

  (`character(1)`) id of the `FilterStates` card container

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterState$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
