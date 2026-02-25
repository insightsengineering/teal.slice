# Specify single filter

Create a `teal_slice` object that holds complete information on
filtering one variable. Check out
[`teal_slice-utilities`](https://insightsengineering.github.io/teal.slice/reference/teal_slice-utilities.md)
functions for working with `teal_slice` object.

## Usage

``` r
teal_slice(
  dataname,
  varname,
  id,
  expr,
  choices = NULL,
  selected = NULL,
  keep_na = NULL,
  keep_inf = NULL,
  fixed = FALSE,
  anchored = FALSE,
  multiple = TRUE,
  title = NULL,
  ...
)
```

## Arguments

- dataname:

  (`character(1)`) name of data set

- varname:

  (`character(1)`) name of variable

- id:

  (`character(1)`) identifier of the filter. Must be specified when
  `expr` is set. When `varname` is specified then `id` is set to
  `"{dataname} {varname}"` by default.

- expr:

  (`character(1)`) string providing a logical expression. Must be a
  valid `R` expression which can be evaluated in the context of the data
  set. For a `data.frame` `var == "x"` is sufficient, but
  [`MultiAssayExperiment::subsetByColData`](https://github.com/waldronlab/MultiAssayExperiment/reference/subsetBy.html)
  requires `dataname` prefix, *e.g.* `data$var == "x"`.

- choices:

  (`vector`) optional, specifies allowed choices; When specified it
  should be a subset of values in variable denoted by `varname`; Type
  and size depends on variable type. Factors are coerced to character.

- selected:

  (`vector`) optional, specifies selected values from `choices`; Type
  and size depends on variable type. Factors are coerced to character.

- keep_na:

  (`logical(1)`) optional flag specifying whether to keep missing values

- keep_inf:

  (`logical(1)`) optional flag specifying whether to keep infinite
  values

- fixed:

  (`logical(1)`) flag specifying whether to fix this filter state
  (forbid setting state)

- anchored:

  (`logical(1)`) flag specifying whether to lock this filter state
  (forbid removing and inactivating)

- multiple:

  (`logical(1)`) optional flag specifying whether more than one value
  can be selected; only applicable to `ChoicesFilterState` and
  `LogicalFilterState`

- title:

  (`character(1)`) optional title of the filter. Ignored when `varname`
  is set.

- ...:

  additional arguments which can be handled by extensions of
  `teal.slice` classes.

## Value

A `teal.slice` object. Depending on whether `varname` or `expr` was
specified, the resulting `teal_slice` also receives class
`teal_slice_var` or `teal_slice_expr`, respectively.

## Details

`teal_slice` object fully describes filter state and can be used to
create, modify, and delete a filter state. A `teal_slice` contains a
number of common fields (all named arguments of `teal_slice`), some of
which are mandatory, but only `dataname` and either `varname` or `expr`
must be specified, while the others have default values.

Setting any of the other values to `NULL` means that those properties
will not be modified (when setting an existing state) or that they will
be determined by data (when creating new a new one). Entire object is
`FilterState` class member and can be accessed with
`FilterState$get_state()`.

A `teal_slice` can come in two flavors:

1.  `teal_slice_var` - this describes a typical interactive filter that
    refers to a single variable, managed by the `FilterState` class.
    This class is created when `varname` is specified. The object
    retains all fields specified in the call. `id` can be created by
    default and need not be specified.

2.  `teal_slice_expr` - this describes a filter state that refers to an
    expression, which can potentially include multiple variables,
    managed by the `FilterStateExpr` class. This class is created when
    `expr` is specified. `dataname` and `anchored` are retained, `fixed`
    is set to `TRUE`, `id` becomes mandatory, `title` remains optional,
    while other arguments are disregarded.

A teal_slice can be passed `FilterState`/`FilterStateExpr` constructors
to instantiate an object. It can also be passed to
`FilterState$set_state` to modify the state. However, once a
`FilterState` is created, only the mutable features can be set with a
teal_slice: `selected`, `keep_na` and `keep_inf`.

Special consideration is given to two fields: `fixed` and `anchored`.
These are always immutable logical flags that default to `FALSE`. In a
`FilterState` instantiated with `fixed = TRUE` the features `selected`,
`keep_na`, `keep_inf` cannot be changed. Note that a `FilterStateExpr`
is always considered to have `fixed = TRUE`. A `FilterState`
instantiated with `anchored = TRUE` cannot be removed.

## Note

Date time objects of `POSIX*t` classes are printed as strings after
converting to UTC timezone.

## Filters in `SumarizedExperiment` and `MultiAssayExperiment` objects

To establish a filter on a column in a `data.frame`, `dataname` and
`varname` are sufficient. `MultiAssayExperiment` objects can be filtered
either on their `colData` slot (which contains subject information) or
on their experiments, which are stored in the `experimentList` slot. For
filters referring to `colData` no extra arguments are needed. If a
filter state is created for an experiment, that experiment name must be
specified in the `experiment` argument. Furthermore, to specify filter
for an `SummarizedExperiment` one must also set `arg` (`"subset"` or
`"select"`, arguments in the
[`subset()`](https://rdrr.io/r/base/subset.html) function for
`SummarizedExperiment`) in order to determine whether the filter refers
to the `SE`'s `rowData` or `colData`.

## See also

[`teal_slices`](https://insightsengineering.github.io/teal.slice/reference/teal_slices.md),
[`is.teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice-utilities.md),
[`as.teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice-utilities.md),
[`as.list.teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice-utilities.md),
[`print.teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice-utilities.md),
[`format.teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice-utilities.md)

## Examples

``` r
x1 <- teal_slice(
  dataname = "data",
  id = "Female adults",
  expr = "SEX == 'F' & AGE >= 18",
  title = "Female adults"
)
x2 <- teal_slice(
  dataname = "data",
  varname = "var",
  choices = c("F", "M", "U"),
  selected = "F",
  keep_na = TRUE,
  keep_inf = TRUE,
  fixed = FALSE,
  anchored = FALSE,
  multiple = TRUE,
  id = "Gender",
  extra_arg = "extra"
)

is.teal_slice(x1)
#> [1] TRUE
as.list(x1)
#> $dataname
#> [1] "data"
#> 
#> $id
#> [1] "Female adults"
#> 
#> $expr
#> [1] "SEX == 'F' & AGE >= 18"
#> 
#> $fixed
#> [1] TRUE
#> 
#> $anchored
#> [1] FALSE
#> 
#> $title
#> [1] "Female adults"
#> 
as.teal_slice(list(dataname = "a", varname = "var"))
#> {
#>   "dataname" : "a",
#>   "varname"  : "var",
#>   "id"       : "a var",
#>   "fixed"    : false,
#>   "anchored" : false,
#>   "multiple" : true
#> }
format(x1)
#> [1] "{\n  \"dataname\" : \"data\",\n  \"id\"       : \"Female adults\",\n  \"expr\"     : \"SEX == 'F' & AGE >= 18\",\n  \"fixed\"    : true,\n  \"anchored\" : false,\n  \"title\"    : \"Female adults\"\n}"
format(x1, show_all = TRUE, trim_lines = FALSE)
#> [1] "{\n  \"dataname\" : \"data\",\n  \"id\"       : \"Female adults\",\n  \"expr\"     : \"SEX == 'F' & AGE >= 18\",\n  \"fixed\"    : true,\n  \"anchored\" : false,\n  \"title\"    : \"Female adults\"\n}"
print(x1)
#> {
#>   "dataname" : "data",
#>   "id"       : "Female adults",
#>   "expr"     : "SEX == 'F' & AGE >= 18",
#>   "fixed"    : true,
#>   "anchored" : false,
#>   "title"    : "Female adults"
#> }
print(x1, show_all = TRUE, trim_lines = FALSE)
#> {
#>   "dataname" : "data",
#>   "id"       : "Female adults",
#>   "expr"     : "SEX == 'F' & AGE >= 18",
#>   "fixed"    : true,
#>   "anchored" : false,
#>   "title"    : "Female adults"
#> }
```
