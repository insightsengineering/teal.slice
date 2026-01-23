# `teal_slices` utility functions

Helper functions for working with
[`teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices.md)
object.

## Usage

``` r
is.teal_slices(x)

as.teal_slices(x)

# S3 method for class 'teal_slices'
as.list(x, recursive = FALSE, ...)

# S3 method for class 'teal_slices'
x[i]

# S3 method for class 'teal_slices'
c(...)

# S3 method for class 'teal_slices'
format(x, show_all = FALSE, trim_lines = TRUE, ...)

# S3 method for class 'teal_slices'
print(x, ...)
```

## Arguments

- x:

  object to test for `teal_slices`, object to convert to `teal_slices`
  or a `teal_slices` object

- recursive:

  (`logical(1)`) flag specifying whether to also convert to list the
  elements of this `teal_slices`

- ...:

  additional arguments passed to other functions.

- i:

  (`character` or `numeric` or `logical`) indicating which elements to
  extract

- show_all:

  (`logical(1)`) whether to display non-null elements of constituent
  `teal_slice` objects

- trim_lines:

  (`logical(1)`) whether to trim lines

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
