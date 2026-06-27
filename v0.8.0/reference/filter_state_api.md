# Managing `FilteredData` states

Set, get and remove filter states of `FilteredData` object.

## Usage

``` r
set_filter_state(datasets, filter)

get_filter_state(datasets)

remove_filter_state(datasets, filter)

clear_filter_states(datasets, force = FALSE)
```

## Arguments

- datasets:

  (`FilteredData`) object to store filter state and filtered datasets,
  shared across modules

  see
  [`FilteredData`](https://insightsengineering.github.io/teal.slice/reference/FilteredData.md)
  for details

- filter:

  (`teal_slices`) specify filters in place on app start-up

- force:

  (`logical(1)`) flag specifying whether to include anchored filter
  states.

## Value

- `set_*`, `remove_*` and `clear_filter_state` return `NULL` invisibly

- `get_filter_state` returns a named `teal_slices` object containing a
  `teal_slice` for every existing `FilterState`

## See also

[`teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice.md)

## Examples

``` r
datasets <- init_filtered_data(list(iris = iris, mtcars = mtcars))
fs <- teal_slices(
  teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor")),
  teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
  teal_slice(dataname = "mtcars", varname = "gear", selected = c(4, 5)),
  teal_slice(dataname = "mtcars", varname = "carb", selected = c(4, 10))
)

# set initial filter state
set_filter_state(datasets, filter = fs)

# get filter state
get_filter_state(datasets)
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "iris",
#>       "varname"        : "Species",
#>       "id"             : "iris Species",
#>       "choices"        : ["setosa", "versicolor", "virgin...
#>       "selected"       : ["setosa", "versicolor"],
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "iris",
#>       "varname"        : "Sepal.Length",
#>       "id"             : "iris Sepal.Length",
#>       "choices"        : [4.2999999999999998, 7.900000000...
#>       "selected"       : [5.0999999999999996, 6.400000000...
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mtcars",
#>       "varname"        : "gear",
#>       "id"             : "mtcars gear",
#>       "choices"        : ["3", "4", "5"],
#>       "selected"       : ["4", "5"],
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true
#>     },
#>     {
#>       "dataname"       : "mtcars",
#>       "varname"        : "carb",
#>       "id"             : "mtcars carb",
#>       "choices"        : [1, 8],
#>       "selected"       : [4, 8],
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

# modify filter state
set_filter_state(
  datasets,
  teal_slices(
    teal_slice(dataname = "iris", varname = "Species", selected = "setosa", keep_na = TRUE)
  )
)

# remove specific filters
remove_filter_state(
  datasets,
  teal_slices(
    teal_slice(dataname = "iris", varname = "Species"),
    teal_slice(dataname = "mtcars", varname = "gear"),
    teal_slice(dataname = "mtcars", varname = "carb")
  )
)

# remove all states
clear_filter_states(datasets)

# \donttest{
if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
  # Requires MultiAssayExperiment from Bioconductor
  data(miniACC, package = "MultiAssayExperiment")

  datasets <- init_filtered_data(list(mae = miniACC))
  fs <- teal_slices(
    teal_slice(
      dataname = "mae", varname = "years_to_birth", selected = c(30, 50),
      keep_na = TRUE, keep_inf = FALSE
    ),
    teal_slice(
      dataname = "mae", varname = "vital_status", selected = "1",
      keep_na = FALSE
    ),
    teal_slice(
      dataname = "mae", varname = "gender", selected = "female",
      keep_na = TRUE
    ),
    teal_slice(
      dataname = "mae", varname = "ARRAY_TYPE", selected = "",
      keep_na = TRUE, experiment = "RPPAArray", arg = "subset"
    )
  )

  # set initial filter state
  set_filter_state(datasets, filter = fs)

  # get filter state
  get_filter_state(datasets)

  # modify filter state
  set_filter_state(
    datasets,
    teal_slices(
      teal_slice(dataname = "mae", varname = "years_to_birth", selected = c(40, 60))
    )
  )

  # remove specific filters
  remove_filter_state(
    datasets,
    teal_slices(
      teal_slice(dataname = "mae", varname = "years_to_birth"),
      teal_slice(dataname = "mae", varname = "vital_status")
    )
  )

  # remove all states
  clear_filter_states(datasets)
}
# }
```
