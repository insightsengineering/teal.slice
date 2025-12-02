# `MAEFilteredDataset` `R6` class

`MAEFilteredDataset` `R6` class

`MAEFilteredDataset` `R6` class

## Super class

[`teal.slice::FilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.md)
-\> `MAEFilteredDataset`

## Methods

### Public methods

- [`MAEFilteredDataset$new()`](#method-MAEFilteredDataset-new)

- [`MAEFilteredDataset$set_filter_state()`](#method-MAEFilteredDataset-set_filter_state)

- [`MAEFilteredDataset$remove_filter_state()`](#method-MAEFilteredDataset-remove_filter_state)

- [`MAEFilteredDataset$ui_add()`](#method-MAEFilteredDataset-ui_add)

- [`MAEFilteredDataset$get_filter_overview()`](#method-MAEFilteredDataset-get_filter_overview)

- [`MAEFilteredDataset$clone()`](#method-MAEFilteredDataset-clone)

Inherited methods

- [`teal.slice::FilteredDataset$clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-clear_filter_states)
- [`teal.slice::FilteredDataset$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-destroy)
- [`teal.slice::FilteredDataset$format()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-format)
- [`teal.slice::FilteredDataset$get_call()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_call)
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

Initialize `MAEFilteredDataset` object.

#### Usage

    MAEFilteredDataset$new(
      dataset,
      dataname,
      keys = character(0),
      label = character(0)
    )

#### Arguments

- `dataset`:

  (`MulitiAssayExperiment`) single `MulitiAssayExperiment` for which
  filters are rendered.

- `dataname`:

  (`character(1)`) syntactically valid name given to the dataset.

- `keys`:

  (`character`) optional vector of primary key column names.

- `label`:

  (`character(1)`) label to describe the dataset.

#### Returns

Object of class `MAEFilteredDataset`, invisibly.

------------------------------------------------------------------------

### Method [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Set filter state.

#### Usage

    MAEFilteredDataset$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`)

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method [`remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)

Remove one or more `FilterState` of a `MAEFilteredDataset`.

#### Usage

    MAEFilteredDataset$remove_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`) specifying `FilterState` objects to remove;
  `teal_slice`s may contain only `dataname` and `varname`, other
  elements are ignored.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### Method `ui_add()`

UI module to add filter variable for this dataset.

#### Usage

    MAEFilteredDataset$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### Method `get_filter_overview()`

Creates row for filter overview in the form of  
`dataname -- observations (remaining/total) -- subjects (remaining/total)` -
MAE

#### Usage

    MAEFilteredDataset$get_filter_overview()

#### Returns

A `data.frame`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MAEFilteredDataset$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# use non-exported function from teal.slice
MAEFilteredDataset <- getFromNamespace("MAEFilteredDataset", "teal.slice")

data(miniACC, package = "MultiAssayExperiment")
dataset <- MAEFilteredDataset$new(miniACC, "MAE")
fs <- teal_slices(
  teal_slice(
    dataname = "MAE", varname = "years_to_birth", selected = c(30, 50), keep_na = TRUE
  ),
  teal_slice(
    dataname = "MAE", varname = "vital_status", selected = "1", keep_na = FALSE
  ),
  teal_slice(
    dataname = "MAE", varname = "gender", selected = "female", keep_na = TRUE
  ),
  teal_slice(
    dataname = "MAE", varname = "ARRAY_TYPE", selected = "", keep_na = TRUE
  )
)
dataset$set_filter_state(state = fs)
#> Warning: filters for columns: ARRAY_TYPE excluded from MAE

library(shiny)
isolate(dataset$get_filter_state())
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "MAE",
#>       "varname"        : "years_to_birth",
#>       "id"             : "MAE years_to_birth",
#>       "choices"        : [14, 83],
#>       "selected"       : [30, 50],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "MAE",
#>       "varname"        : "vital_status",
#>       "id"             : "MAE vital_status",
#>       "choices"        : ["0", "1"],
#>       "selected"       : ["1"],
#>       "keep_na"        : false,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "MAE",
#>       "varname"        : "gender",
#>       "id"             : "MAE gender",
#>       "choices"        : ["female", "male"],
#>       "selected"       : ["female"],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     }
#>   ],
#>   "attributes": {
#>     "include_varnames" : {
#>       "MAE"            : ["patientID", "years_to_birth", ...
#>     },
#>     "count_type"       : "none",
#>     "allow_add"        : true
#>   }
#> } 
```
