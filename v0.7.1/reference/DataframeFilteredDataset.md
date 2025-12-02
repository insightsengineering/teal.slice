# The `DataframeFilteredDataset` `R6` class

The `DataframeFilteredDataset` `R6` class

The `DataframeFilteredDataset` `R6` class

## Super class

[`teal.slice::FilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.md)
-\> `DataframeFilteredDataset`

## Methods

### Public methods

- [`DataframeFilteredDataset$new()`](#method-DataframeFilteredDataset-new)

- [`DataframeFilteredDataset$get_call()`](#method-DataframeFilteredDataset-get_call)

- [`DataframeFilteredDataset$set_filter_state()`](#method-DataframeFilteredDataset-set_filter_state)

- [`DataframeFilteredDataset$remove_filter_state()`](#method-DataframeFilteredDataset-remove_filter_state)

- [`DataframeFilteredDataset$ui_add()`](#method-DataframeFilteredDataset-ui_add)

- [`DataframeFilteredDataset$get_filter_overview()`](#method-DataframeFilteredDataset-get_filter_overview)

- [`DataframeFilteredDataset$clone()`](#method-DataframeFilteredDataset-clone)

Inherited methods

- [`teal.slice::FilteredDataset$clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-clear_filter_states)
- [`teal.slice::FilteredDataset$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-destroy)
- [`teal.slice::FilteredDataset$format()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-format)
- [`teal.slice::FilteredDataset$get_dataname()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataname)
- [`teal.slice::FilteredDataset$get_dataset()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset)
- [`teal.slice::FilteredDataset$get_dataset_label()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset_label)
- [`teal.slice::FilteredDataset$get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_filter_state)
- [`teal.slice::FilteredDataset$get_keys()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_keys)
- [`teal.slice::FilteredDataset$print()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-print)
- [`teal.slice::FilteredDataset$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_active)
- [`teal.slice::FilteredDataset$srv_add()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_add)
- [`teal.slice::FilteredDataset$ui_active()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-ui_active)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initializes this `DataframeFilteredDataset` object.

#### Usage

    DataframeFilteredDataset$new(
      dataset,
      dataname,
      keys = character(0),
      parent_name = character(0),
      parent = NULL,
      join_keys = character(0),
      label = character(0)
    )

#### Arguments

- `dataset`:

  (`data.frame`) single `data.frame` for which filters are rendered.

- `dataname`:

  (`character(1)`) syntactically valid name given to the dataset.

- `keys`:

  (`character`) optional vector of primary key column names.

- `parent_name`:

  (`character(1)`) name of the parent dataset.

- `parent`:

  (`reactive`) that returns a filtered `data.frame` from other
  `FilteredDataset` named `parent_name`. Passing `parent` results in a
  `reactive` link that causes re-filtering of this `dataset` based on
  the changes in `parent`.

- `join_keys`:

  (`character`) vector of names of columns in this dataset to join with
  `parent` dataset. If column names in the parent do not match these,
  they should be given as the names of this vector.

- `label`:

  (`character(1)`) label to describe the dataset.

#### Returns

Object of class `DataframeFilteredDataset`, invisibly.

------------------------------------------------------------------------

### Method `get_call()`

Gets the subset expression.

This function returns subset expressions equivalent to selected items
within each of `filter_states`. Configuration of the expressions is
constant and depends on `filter_states` type and order which are set
during initialization. This class contains single `FilterStates` which
contains single `state_list` and all `FilterState` objects apply to one
argument (`...`) in a
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
call.

#### Usage

    DataframeFilteredDataset$get_call(sid = "")

#### Arguments

- `sid`:

  (`character`) when specified, the method returns code containing
  conditions calls of `FilterState` objects with `sid` different to that
  of this `sid` argument.

#### Returns

Either a `list` of length 1 containing a filter `call`, or `NULL`.

------------------------------------------------------------------------

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Set filter state.

#### Usage

    DataframeFilteredDataset$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`)

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Remove one or more `FilterState` form a `FilteredDataset`.

#### Usage

    DataframeFilteredDataset$remove_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`) specifying `FilterState` objects to remove;
  `teal_slice`s may contain only `dataname` and `varname`, other
  elements are ignored

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `ui_add()`

UI module to add filter variable for this dataset.

#### Usage

    DataframeFilteredDataset$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### Method `get_filter_overview()`

Creates row for filter overview in the form of  
`dataname -- observations (remaining/total)` - data.frame

#### Usage

    DataframeFilteredDataset$get_filter_overview()

#### Returns

A `data.frame`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataframeFilteredDataset$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
DataframeFilteredDataset <- getFromNamespace("DataframeFilteredDataset", "teal.slice")

library(shiny)

ds <- DataframeFilteredDataset$new(iris, "iris")
ds$set_filter_state(
  teal_slices(
    teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
    teal_slice(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
  )
)
isolate(ds$get_filter_state())
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
#>       "dataname"       : "iris",
#>       "varname"        : "Petal.Length",
#>       "id"             : "iris Petal.Length",
#>       "choices"        : [1, 6.9000000000000004],
#>       "selected"       : [2, 5],
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     }
#>   ],
#>   "attributes": {
#>     "include_varnames" : {
#>       "iris"           : ["Sepal.Length", "Sepal.Width", ...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 
isolate(ds$get_call())
#> $filter
#> iris <- dplyr::filter(iris, Species == "virginica" & (Petal.Length >= 
#>     2 & Petal.Length <= 5))
#> 

## set_filter_state
dataset <- DataframeFilteredDataset$new(iris, "iris")
fs <- teal_slices(
  teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
  teal_slice(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
)
dataset$set_filter_state(state = fs)
isolate(dataset$get_filter_state())
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
#>       "dataname"       : "iris",
#>       "varname"        : "Petal.Length",
#>       "id"             : "iris Petal.Length",
#>       "choices"        : [1, 6.9000000000000004],
#>       "selected"       : [2, 5],
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     }
#>   ],
#>   "attributes": {
#>     "include_varnames" : {
#>       "iris"           : ["Sepal.Length", "Sepal.Width", ...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 
```
