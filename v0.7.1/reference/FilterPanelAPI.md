# Class to encapsulate the API of the filter panel of a teal app

An API class for managing filter states in a teal application's filter
panel.

## Details

The purpose of this class is to encapsulate the API of the filter panel
in a new class `FilterPanelAPI` so that it can be passed and used in the
server call of any module instead of passing the whole `FilteredData`
object.

This class is supported by methods to set, get, remove filter states in
the filter panel API.

## Methods

### Public methods

- [`FilterPanelAPI$new()`](#method-FilterPanelAPI-new)

- [`FilterPanelAPI$get_filter_state()`](#method-FilterPanelAPI-get_filter_state)

- [`FilterPanelAPI$set_filter_state()`](#method-FilterPanelAPI-set_filter_state)

- [`FilterPanelAPI$remove_filter_state()`](#method-FilterPanelAPI-remove_filter_state)

- [`FilterPanelAPI$clear_filter_states()`](#method-FilterPanelAPI-clear_filter_states)

- [`FilterPanelAPI$clone()`](#method-FilterPanelAPI-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize a `FilterPanelAPI` object.

#### Usage

    FilterPanelAPI$new(datasets)

#### Arguments

- `datasets`:

  (`FilteredData`)

------------------------------------------------------------------------

### Method [`get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Gets the reactive values from the active `FilterState` objects of the
`FilteredData` object.

Gets all active filters in the form of a nested list. The output list is
a compatible input to `set_filter_state`.

#### Usage

    FilterPanelAPI$get_filter_state()

#### Returns

`list` with named elements corresponding to `FilteredDataset` objects
with active filters.

------------------------------------------------------------------------

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Sets active filter states.

#### Usage

    FilterPanelAPI$set_filter_state(filter)

#### Arguments

- `filter`:

  (`teal_slices`)

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Remove one or more `FilterState` of a `FilteredDataset` in the
`FilteredData` object.

#### Usage

    FilterPanelAPI$remove_filter_state(filter)

#### Arguments

- `filter`:

  (`teal_slices`) specifying `FilterState` objects to remove;
  `teal_slice`s may contain only `dataname` and `varname`, other
  elements are ignored

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Remove all `FilterStates` of the `FilteredData` object.

#### Usage

    FilterPanelAPI$clear_filter_states(datanames)

#### Arguments

- `datanames`:

  (`character`) `datanames` to remove their `FilterStates`; omit to
  remove all `FilterStates` in the `FilteredData` object

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterPanelAPI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(shiny)

fd <- init_filtered_data(list(iris = iris))
fpa <- FilterPanelAPI$new(fd)

# get the actual filter state --> empty named list
isolate(fpa$get_filter_state())
#> {
#>   "slices": [],
#>   "attributes": {
#>     "include_varnames" : {
#>       "iris"           : ["Sepal.Length", "Sepal.Width", ...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 

# set a filter state
set_filter_state(
  fpa,
  teal_slices(
    teal_slice(dataname = "iris", varname = "Species", selected = "setosa", keep_na = TRUE)
  )
)

# get the actual filter state --> named list with filters
isolate(fpa$get_filter_state())
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "iris",
#>       "varname"        : "Species",
#>       "id"             : "iris Species",
#>       "choices"        : ["setosa", "versicolor", "virgin...
#>       "selected"       : ["setosa"],
#>       "keep_na"        : true,
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

# remove all_filter_states
fpa$clear_filter_states()

# get the actual filter state --> empty named list
isolate(fpa$get_filter_state())
#> {
#>   "slices": [],
#>   "attributes": {
#>     "include_varnames" : {
#>       "iris"           : ["Sepal.Length", "Sepal.Width", ...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 
```
