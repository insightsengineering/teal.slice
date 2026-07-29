# `teal_slice` utility functions

Helper functions for working with
[`teal_slice`](https://insightsengineering.github.io/teal.slice/reference/teal_slice.md)
object.

## Usage

``` r
is.teal_slice(x)

as.teal_slice(x)

# S3 method for class 'teal_slice'
as.list(x, ...)

# S3 method for class 'teal_slice'
format(x, show_all = FALSE, trim_lines = TRUE, ...)

# S3 method for class 'teal_slice'
print(x, ...)
```

## Arguments

- x:

  (`teal.slice`)

- ...:

  additional arguments passed to other functions.

- show_all:

  (`logical(1)`) indicating whether to show all fields. If set to
  `FALSE`, only non-NULL elements will be printed.

- trim_lines:

  (`logical(1)`) indicating whether to trim lines when printing.

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
