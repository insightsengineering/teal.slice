# `MAEFilteredDataset` `R6` class

`MAEFilteredDataset` `R6` class

## Super class

[`FilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.md)
-\> `MAEFilteredDataset`

## Methods

### Public methods

- [`MAEFilteredDataset$new()`](#method-MAEFilteredDataset-initialize)

- [`MAEFilteredDataset$set_filter_state()`](#method-MAEFilteredDataset-set_filter_state)

- [`MAEFilteredDataset$remove_filter_state()`](#method-MAEFilteredDataset-remove_filter_state)

- [`MAEFilteredDataset$ui_add()`](#method-MAEFilteredDataset-ui_add)

- [`MAEFilteredDataset$get_filter_overview()`](#method-MAEFilteredDataset-get_filter_overview)

- [`MAEFilteredDataset$clone()`](#method-MAEFilteredDataset-clone)

Inherited methods

- [`FilteredDataset$clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-clear_filter_states)
- [`FilteredDataset$destroy()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-destroy)
- [`FilteredDataset$format()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-format)
- [`FilteredDataset$get_call()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_call)
- [`FilteredDataset$get_dataname()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataname)
- [`FilteredDataset$get_dataset()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset)
- [`FilteredDataset$get_dataset_label()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_dataset_label)
- [`FilteredDataset$get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_filter_state)
- [`FilteredDataset$get_keys()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-get_keys)
- [`FilteredDataset$print()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-print)
- [`FilteredDataset$srv_active()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_active)
- [`FilteredDataset$srv_add()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-srv_add)
- [`FilteredDataset$ui_active()`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.html#method-ui_active)

------------------------------------------------------------------------

### `MAEFilteredDataset$new()`

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

### `MAEFilteredDataset$set_filter_state()`

Set filter state.

#### Usage

    MAEFilteredDataset$set_filter_state(state)

#### Arguments

- `state`:

  (`teal_slices`)

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `MAEFilteredDataset$remove_filter_state()`

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

### `MAEFilteredDataset$ui_add()`

UI module to add filter variable for this dataset.

#### Usage

    MAEFilteredDataset$ui_add(id)

#### Arguments

- `id`:

  (`character(1)`) `shiny` module instance id.

#### Returns

`shiny.tag`

------------------------------------------------------------------------

### `MAEFilteredDataset$get_filter_overview()`

Creates row for filter overview in the form of\
`dataname -- observations (remaining/total) -- subjects (remaining/total)` -
MAE

#### Usage

    MAEFilteredDataset$get_filter_overview()

#### Returns

A `data.frame`.

------------------------------------------------------------------------

### `MAEFilteredDataset$clone()`

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
