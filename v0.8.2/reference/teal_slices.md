# Complete filter specification

Create `teal_slices` object to package multiple filters and additional
settings. Check out
[`teal_slices-utilities`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md)
functions for working with `teal_slices` object.

## Usage

``` r
teal_slices(
  ...,
  exclude_varnames = NULL,
  include_varnames = NULL,
  count_type = NULL,
  allow_add = TRUE
)
```

## Arguments

- ...:

  any number of `teal_slice` objects.

- include_varnames, exclude_varnames:

  (`named list`s of `character`) where list names match names of data
  sets and vector elements match variable names in respective data sets;
  specify which variables are allowed to be filtered; see `Details`.

- count_type:

  *This is a new feature. Do kindly share your opinions on
  [`teal.slice`'s GitHub
  repository](https://github.com/insightsengineering/teal.slice/).*

  (`character(1)`) string specifying how observations are tallied by
  these filter states. Possible options:

  - `"none"` (default) to have counts of single `FilterState` to show
    unfiltered number only.

  - `"all"` to have counts of single `FilterState` to show number of
    observation in filtered and unfiltered dataset. Note, that issues
    were reported when using this option with `MultiAssayExperiment`.
    Please make sure that adding new filters doesn't fail on target
    platform before deploying for production.

- allow_add:

  (`logical(1)`) logical flag specifying whether the user will be able
  to add new filters

## Value

`teal_slices`, which is an unnamed list of `teal_slice` objects.

## Details

`teal_slices()` collates multiple `teal_slice` objects into a
`teal_slices` object, a complete filter specification. This is used by
all classes above `FilterState` as well as `filter_panel_api` wrapper
functions. `teal_slices` has attributes that modify the behavior of the
filter panel, which are resolved by different classes.

`include_varnames` and `exclude_varnames` determine which variables can
have filters assigned. The former enumerates allowed variables, the
latter enumerates forbidden values. Since these could be mutually
exclusive, it is impossible to set both allowed and forbidden variables
for one data set in one `teal_slices`.

## See also

- [`teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice.md)
  for creating constituent elements of `teal_slices`

- [`teal::slices_store`](https://insightsengineering.github.io/teal/latest-tag/reference/slices_store.html)
  for robust utilities for saving and loading `teal_slices` in `JSON`
  format

- [`is.teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md),
  [`as.teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md),
  [`as.list.teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md),
  \[`[.teal_slices`\],
  [`c.teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md)
  [`print.teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md),
  [`format.teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices-utilities.md)

## Examples

``` r
filter_1 <- teal_slice(
  dataname = "dataname1",
  varname = "varname1",
  choices = letters,
  selected = "b",
  keep_na = TRUE,
  fixed = FALSE,
  extra1 = "extraone"
)
filter_2 <- teal_slice(
  dataname = "dataname1",
  varname = "varname2",
  choices = 1:10,
  keep_na = TRUE,
  selected = 2,
  fixed = TRUE,
  anchored = FALSE,
  extra2 = "extratwo"
)
filter_3 <- teal_slice(
  dataname = "dataname2",
  varname = "varname3",
  choices = 1:10 / 10,
  keep_na = TRUE,
  selected = 0.2,
  fixed = TRUE,
  anchored = FALSE,
  extra1 = "extraone",
  extra2 = "extratwo"
)

all_filters <- teal_slices(
  filter_1,
  filter_2,
  filter_3,
  exclude_varnames = list(
    "dataname1" = "varname2"
  )
)

is.teal_slices(all_filters)
#> [1] TRUE
all_filters[1:2]
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname1",
#>       "id"             : "dataname1 varname1",
#>       "choices"        : ["a", "b", "c", "d", "e", "f", "...
#>       "selected"       : ["b"],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra1"         : "extraone"
#>     },
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname2",
#>       "id"             : "dataname1 varname2",
#>       "choices"        : [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
#>       "selected"       : [2],
#>       "keep_na"        : true,
#>       "fixed"          : true,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra2"         : "extratwo"
#>     }
#>   ],
#>   "attributes": {
#>     "exclude_varnames" : {
#>       "dataname1"      : "varname2"
#>     },
#>     "allow_add"        : true
#>   }
#> } 
c(all_filters[1], all_filters[2])
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname1",
#>       "id"             : "dataname1 varname1",
#>       "choices"        : ["a", "b", "c", "d", "e", "f", "...
#>       "selected"       : ["b"],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra1"         : "extraone"
#>     },
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname2",
#>       "id"             : "dataname1 varname2",
#>       "choices"        : [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
#>       "selected"       : [2],
#>       "keep_na"        : true,
#>       "fixed"          : true,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra2"         : "extratwo"
#>     }
#>   ],
#>   "attributes": {
#>     "exclude_varnames" : {
#>       "dataname1"      : "varname2"
#>     },
#>     "allow_add"        : true
#>   }
#> } 
print(all_filters)
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname1",
#>       "id"             : "dataname1 varname1",
#>       "choices"        : ["a", "b", "c", "d", "e", "f", "...
#>       "selected"       : ["b"],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra1"         : "extraone"
#>     },
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname2",
#>       "id"             : "dataname1 varname2",
#>       "choices"        : [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
#>       "selected"       : [2],
#>       "keep_na"        : true,
#>       "fixed"          : true,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra2"         : "extratwo"
#>     },
#>     {
#>       "dataname"       : "dataname2",
#>       "varname"        : "varname3",
#>       "id"             : "dataname2 varname3",
#>       "choices"        : [0.10000000000000001, 0.20000000...
#>       "selected"       : [0.20000000000000001],
#>       "keep_na"        : true,
#>       "fixed"          : true,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra2"         : "extratwo",
#>       "extra1"         : "extraone"
#>     }
#>   ],
#>   "attributes": {
#>     "exclude_varnames" : {
#>       "dataname1"      : "varname2"
#>     },
#>     "allow_add"        : true
#>   }
#> } 
print(all_filters, trim_lines = FALSE)
#> {
#>   "slices": [
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname1",
#>       "id"             : "dataname1 varname1",
#>       "choices"        : ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"],
#>       "selected"       : ["b"],
#>       "keep_na"        : true,
#>       "fixed"          : false,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra1"         : "extraone"
#>     },
#>     {
#>       "dataname"       : "dataname1",
#>       "varname"        : "varname2",
#>       "id"             : "dataname1 varname2",
#>       "choices"        : [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
#>       "selected"       : [2],
#>       "keep_na"        : true,
#>       "fixed"          : true,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra2"         : "extratwo"
#>     },
#>     {
#>       "dataname"       : "dataname2",
#>       "varname"        : "varname3",
#>       "id"             : "dataname2 varname3",
#>       "choices"        : [0.10000000000000001, 0.20000000000000001, 0.29999999999999999, 0.40000000000000002, 0.5, 0.59999999999999998, 0.69999999999999996, 0.80000000000000004, 0.90000000000000002, 1],
#>       "selected"       : [0.20000000000000001],
#>       "keep_na"        : true,
#>       "fixed"          : true,
#>       "anchored"       : false,
#>       "multiple"       : true,
#>       "extra2"         : "extratwo",
#>       "extra1"         : "extraone"
#>     }
#>   ],
#>   "attributes": {
#>     "exclude_varnames" : {
#>       "dataname1"      : "varname2"
#>     },
#>     "allow_add"        : true
#>   }
#> } 
```
