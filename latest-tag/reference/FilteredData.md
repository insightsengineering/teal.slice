# Class to encapsulate filtered datasets

Manages filtering of all datasets in the application or module.

## Details

The main purpose of this class is to provide a collection of reactive
datasets, each dataset having a filter state that determines how it is
filtered.

For each dataset, `get_filter_expr` returns the call to filter the
dataset according to the filter state. The data itself can be obtained
through `get_data`.

The datasets are filtered lazily, i.e. only when requested / needed in a
`shiny` app.

By design, any `dataname` set through `set_dataset` cannot be removed
because other code may already depend on it. As a workaround, the
underlying data can be set to `NULL`.

The class currently supports variables of the following types within
datasets:

- `choices`: variable of type `factor`, e.g. `ADSL$COUNTRY`,
  `iris$Species` zero or more options can be selected, when the variable
  is a factor

- `logical`: variable of type `logical`, e.g. `ADSL$TRT_FLAG` exactly
  one option must be selected, `TRUE` or `FALSE`

- `ranges`: variable of type `numeric`, e.g. `ADSL$AGE`,
  `iris$Sepal.Length` numerical range, a range within this range can be
  selected

- `dates`: variable of type `Date`, `POSIXlt` Other variables cannot be
  used for filtering the data in this class.

Common arguments are:

1.  `filtered`: whether to return a filtered result or not

2.  `dataname`: the name of one of the datasets in this `FilteredData`
    object

3.  `varname`: one of the columns in a dataset

## Methods

### Public methods

- [`FilteredData$new()`](#method-FilteredData-new)

- [`FilteredData$destroy()`](#method-FilteredData-destroy)

- [`FilteredData$datanames()`](#method-FilteredData-datanames)

- [`FilteredData$get_datalabel()`](#method-FilteredData-get_datalabel)

- [`FilteredData$set_available_teal_slices()`](#method-FilteredData-set_available_teal_slices)

- [`FilteredData$get_available_teal_slices()`](#method-FilteredData-get_available_teal_slices)

- [`FilteredData$get_call()`](#method-FilteredData-get_call)

- [`FilteredData$get_data()`](#method-FilteredData-get_data)

- [`FilteredData$get_join_keys()`](#method-FilteredData-get_join_keys)

- [`FilteredData$get_filter_overview()`](#method-FilteredData-get_filter_overview)

- [`FilteredData$get_keys()`](#method-FilteredData-get_keys)

- [`FilteredData$set_dataset()`](#method-FilteredData-set_dataset)

- [`FilteredData$set_join_keys()`](#method-FilteredData-set_join_keys)

- [`FilteredData$get_filter_state()`](#method-FilteredData-get_filter_state)

- [`FilteredData$format()`](#method-FilteredData-format)

- [`FilteredData$print()`](#method-FilteredData-print)

- [`FilteredData$set_filter_state()`](#method-FilteredData-set_filter_state)

- [`FilteredData$remove_filter_state()`](#method-FilteredData-remove_filter_state)

- [`FilteredData$clear_filter_states()`](#method-FilteredData-clear_filter_states)

- [`FilteredData$ui_filter_panel()`](#method-FilteredData-ui_filter_panel)

- [`FilteredData$srv_filter_panel()`](#method-FilteredData-srv_filter_panel)

- [`FilteredData$ui_active()`](#method-FilteredData-ui_active)

- [`FilteredData$srv_active()`](#method-FilteredData-srv_active)

- [`FilteredData$ui_overview()`](#method-FilteredData-ui_overview)

- [`FilteredData$srv_overview()`](#method-FilteredData-srv_overview)

- [`FilteredData$clone()`](#method-FilteredData-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize a `FilteredData` object.

#### Usage

    FilteredData$new(data_objects, join_keys = teal.data::join_keys())

#### Arguments

- `data_objects`:

  (`named list`) List of data objects. Names of the list will be used as
  `dataname` for respective datasets.

- `join_keys`:

  (`join_keys`) optional joining keys, see
  [`teal.data::join_keys()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/join_keys.html).

------------------------------------------------------------------------

### Method `destroy()`

Destroys a `FilteredData` object.

#### Usage

    FilteredData$destroy()

------------------------------------------------------------------------

### Method `datanames()`

Gets `datanames`.

#### Usage

    FilteredData$datanames()

#### Details

The `datanames` are returned in the order in which they must be
evaluated (in case of dependencies).

#### Returns

Character vector.

------------------------------------------------------------------------

### Method `get_datalabel()`

Gets data label for the dataset. Useful to display in `Show R Code`.

#### Usage

    FilteredData$get_datalabel(dataname)

#### Arguments

- `dataname`:

  (`character(1)`) name of the dataset

#### Returns

Character string.

------------------------------------------------------------------------

### Method `set_available_teal_slices()`

Set list of external filter states available for activation.

#### Usage

    FilteredData$set_available_teal_slices(x)

#### Arguments

- `x`:

  (`reactive`) should return `teal_slices`

#### Details

Unlike adding new filter from the column, these filters can come with
some prespecified settings. `teal_slices` are wrapped in a `reactive` so
they can be updated from elsewhere in the app. Filters passed in `x` are
limited to those that can be set for this `FilteredData` object, i.e.
they have the correct `dataname` and `varname` (waived
`teal_slice_fixed` as they do not have `varname`). List is accessible in
`ui/srv_active` through `ui/srv_available_filters`.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `get_available_teal_slices()`

Get list of filter states available for this object.

#### Usage

    FilteredData$get_available_teal_slices()

#### Details

All `teal_slice` objects that have been created since the beginning of
the app session are stored in one `teal_slices` object. This returns a
subset of that `teal_slices`, describing filter states that can be set
for this object.

#### Returns

`reactive` that returns `teal_slices`.

------------------------------------------------------------------------

### Method `get_call()`

Gets a `call` to filter the dataset according to the filter state.

#### Usage

    FilteredData$get_call(dataname)

#### Arguments

- `dataname`:

  (`character(1)`) name of the dataset

#### Details

It returns a `call` to filter the dataset only, assuming the other
(filtered) datasets it depends on are available.

Together with `self$datanames()` which returns the datasets in the
correct evaluation order, this generates the whole filter code, see the
function `FilteredData$get_filter_code`.

For the return type, note that
[`rlang::is_expression`](https://rlang.r-lib.org/reference/is_expression.html)
returns `TRUE` on the return type, both for base `R` expressions and
calls (single expression, capturing a function call).

The filtered dataset has the name given by
`self$filtered_dataname(dataname)`

This can be used for the `Show R Code` generation.

#### Returns

A list of `call`s.

------------------------------------------------------------------------

### Method `get_data()`

Gets filtered or unfiltered dataset.

For `filtered = FALSE`, the original data set with `set_data` is
returned including all attributes.

#### Usage

    FilteredData$get_data(dataname, filtered = TRUE)

#### Arguments

- `dataname`:

  (`character(1)`) name of the dataset.

- `filtered`:

  (`logical(1)`) whether to return a filtered or unfiltered dataset.

#### Returns

A data object, a `data.frame` or a `MultiAssayExperiment`.

------------------------------------------------------------------------

### Method `get_join_keys()`

Get join keys between two datasets.

#### Usage

    FilteredData$get_join_keys()

#### Returns

`join_keys`

------------------------------------------------------------------------

### Method `get_filter_overview()`

Creates filter overview table to be displayed in the application. One
row is created per dataset, according to the `get_filter_overview`
methods of the contained `FilteredDataset` objects.

#### Usage

    FilteredData$get_filter_overview(datanames)

#### Arguments

- `datanames`:

  (`character`) vector of dataset names.

#### Returns

A `data.frame` listing the numbers of observations in all datasets.

------------------------------------------------------------------------

### Method `get_keys()`

Get keys for the dataset.

#### Usage

    FilteredData$get_keys(dataname)

#### Arguments

- `dataname`:

  (`character(1)`) name of the dataset.

#### Returns

Character vector of key column names.

------------------------------------------------------------------------

### Method `set_dataset()`

Adds a dataset to this `FilteredData`.

#### Usage

    FilteredData$set_dataset(data, dataname)

#### Arguments

- `data`:

  (`data.frame` or `MultiAssayExperiment`) data to be filtered.

- `dataname`:

  (`character(1)`) the name of the `dataset` to be added to this object.

#### Details

`set_dataset` creates a `FilteredDataset` object which keeps `dataset`
for the filtering purpose. If this data has a parent specified in the
`join_keys` object stored in `private$join_keys` then created
`FilteredDataset` (child) gets linked with other `FilteredDataset`
(parent). "Child" dataset return filtered data then dependent on the
reactive filtered data of the "parent". See more in documentation of
`parent` argument in `DataframeFilteredDataset` constructor.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `set_join_keys()`

Set the `join_keys`.

#### Usage

    FilteredData$set_join_keys(join_keys)

#### Arguments

- `join_keys`:

  (`join_keys`), see
  [`teal.data::join_keys()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/join_keys.html).

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method [`get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Gets states of all contained `FilterState` objects.

#### Usage

    FilteredData$get_filter_state()

#### Returns

A `teal_slices` object.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Returns a formatted string representing this `FilteredData` object.

#### Usage

    FilteredData$format(show_all = FALSE, trim_lines = TRUE)

#### Arguments

- `show_all`:

  (`logical(1)`) passed to `format.teal_slice`.

- `trim_lines`:

  (`logical(1)`) passed to `format.teal_slice`.

#### Returns

`character(1)` the formatted string.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints this `FilteredData` object.

#### Usage

    FilteredData$print(...)

#### Arguments

- `...`:

  additional arguments passed to `format`.

------------------------------------------------------------------------

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Sets active filter states.

#### Usage

    FilteredData$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`)

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Removes one or more `FilterState` from a `FilteredData` object.

#### Usage

    FilteredData$remove_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`) specifying `FilterState` objects to remove;
  `teal_slice`s may contain only `dataname` and `varname`, other
  elements are ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Remove all `FilterStates` of a `FilteredDataset` or all `FilterStates`
of a `FilteredData` object.

#### Usage

    FilteredData$clear_filter_states(datanames = self$datanames(), force = FALSE)

#### Arguments

- `datanames`:

  (`character`) names of datasets for which to remove all filter states.
  Defaults to all datasets in this `FilteredData` object.

- `force`:

  (`logical(1)`) flag specifying whether to include anchored filter
  states.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `ui_filter_panel()`

top-level `shiny` module for the filter panel in the `teal` app.
Contains 1) filter overview panel, 2) filter active panel, and 3) add
filters panel.

#### Usage

    FilteredData$ui_filter_panel(id, active_datanames = self$datanames)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `active_datanames`:

  (`reactive`) defining subset of `self$datanames()` to be displayed.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### Method `srv_filter_panel()`

Server function for filter panel.

#### Usage

    FilteredData$srv_filter_panel(id, active_datanames = self$datanames)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `active_datanames`:

  (`function` or `reactive`) returning `datanames` that should be shown
  on the filter panel. Must be a subset of the `datanames` in this
  `FilteredData`. If the function returns `NULL` (as opposed to
  `character(0)`), the filter panel will be hidden.

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `ui_active()`

Server module responsible for displaying active filters.

#### Usage

    FilteredData$ui_active(id, active_datanames = self$datanames)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `active_datanames`:

  (`reactive`) defining subset of `self$datanames()` to be displayed.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### Method `srv_active()`

Server module responsible for displaying active filters.

#### Usage

    FilteredData$srv_active(id, active_datanames = self$datanames)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `active_datanames`:

  (`reactive`) defining subset of `self$datanames()` to be displayed.

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `ui_overview()`

Creates the UI definition for the module showing counts for each dataset
contrasting the filtered to the full unfiltered dataset.

Per dataset, it displays the number of rows/observations in each
dataset, the number of unique subjects.

#### Usage

    FilteredData$ui_overview(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

------------------------------------------------------------------------

### Method `srv_overview()`

Server function to display the number of records in the filtered and
unfiltered data.

#### Usage

    FilteredData$srv_overview(id, active_datanames = self$datanames)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

- `active_datanames`:

  (`reactive`) returning `datanames` that should be shown on the filter
  panel, must be a subset of the `datanames` argument provided to
  `ui_filter_panel`; if the function returns `NULL` (as opposed to
  `character(0)`), the filter panel will be hidden.

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilteredData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
FilteredData <- getFromNamespace("FilteredData", "teal.slice")

library(shiny)

datasets <- FilteredData$new(list(iris = iris, mtcars = mtcars))

# get datanames
datasets$datanames()
#> [1] "iris"   "mtcars"

datasets$set_filter_state(
  teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "virginica"))
)

datasets$set_filter_state(
  teal_slices(teal_slice(dataname = "mtcars", varname = "mpg", selected = c(15, 20)))
)

isolate(datasets$get_filter_state())
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "iris",
#>       "varname"        : "Species",
#>       "id"             : "iris Species",
#>       "choices"        : ["setosa", "versicolor", "virgin...
#>       "selected"       : ["virginica"],
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mtcars",
#>       "varname"        : "mpg",
#>       "id"             : "mtcars mpg",
#>       "choices"        : [10.4, 34],
#>       "selected"       : [15, 20],
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     }
#>   ],
#>   "attributes": {
#>     "include_varnames" : {
#>       "iris"           : ["Sepal.Length", "Sepal.Width", ...
#>       "mtcars"         : ["mpg", "cyl", "disp", "hp", "dr...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 
isolate(datasets$get_call("iris"))
#> $filter
#> iris <- dplyr::filter(iris, Species == "virginica")
#> 
isolate(datasets$get_call("mtcars"))
#> $filter
#> mtcars <- dplyr::filter(mtcars, mpg >= 15 & mpg <= 20)
#> 

### set_filter_state
library(shiny)

data(miniACC, package = "MultiAssayExperiment")
datasets <- FilteredData$new(list(iris = iris, mae = miniACC))
fs <- teal_slices(
  teal_slice(
    dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
    keep_na = TRUE, keep_inf = FALSE
  ),
  teal_slice(
    dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"),
    keep_na = FALSE
  ),
  teal_slice(
    dataname = "mae", varname = "years_to_birth", selected = c(30, 50),
    keep_na = TRUE, keep_inf = FALSE
  ),
  teal_slice(dataname = "mae", varname = "vital_status", selected = "1", keep_na = FALSE),
  teal_slice(dataname = "mae", varname = "gender", selected = "female", keep_na = TRUE),
  teal_slice(
    dataname = "mae", varname = "ARRAY_TYPE",
    selected = "", keep_na = TRUE, experiment = "RPPAArray", arg = "subset"
  )
)
datasets$set_filter_state(state = fs)
isolate(datasets$get_filter_state())
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "iris",
#>       "varname"        : "Sepal.Length",
#>       "id"             : "iris Sepal.Length",
#>       "choices"        : [4.2999999999999998, 7.900000000...
#>       "selected"       : [5.0999999999999996, 6.400000000...
#>       "keep_na"        : true,
#>       "keep_inf"       : false,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "iris",
#>       "varname"        : "Species",
#>       "id"             : "iris Species",
#>       "choices"        : ["setosa", "versicolor", "virgin...
#>       "selected"       : ["setosa", "versicolor"],
#>       "keep_na"        : false,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mae",
#>       "varname"        : "years_to_birth",
#>       "id"             : "mae years_to_birth",
#>       "choices"        : [14, 83],
#>       "selected"       : [30, 50],
#>       "keep_na"        : true,
#>       "keep_inf"       : false,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mae",
#>       "varname"        : "vital_status",
#>       "id"             : "mae vital_status",
#>       "choices"        : ["0", "1"],
#>       "selected"       : ["1"],
#>       "keep_na"        : false,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mae",
#>       "varname"        : "gender",
#>       "id"             : "mae gender",
#>       "choices"        : ["female", "male"],
#>       "selected"       : ["female"],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mae",
#>       "varname"        : "ARRAY_TYPE",
#>       "id"             : "mae ARRAY_TYPE RPPAArray subset..
#>       "choices"        : ["", "protein_level"],
#>       "selected"       : [""],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "arg"            : "subset",
#>       "experiment"     : "RPPAArray"
#>     }
#>   ],
#>   "attributes": {
#>     "include_varnames" : {
#>       "iris"           : ["Sepal.Length", "Sepal.Width", ...
#>       "mae"            : ["patientID", "years_to_birth", ...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 
```
