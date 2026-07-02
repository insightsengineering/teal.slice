# Gets supported filterable variable names

Gets filterable variable names from a given object. The names match
variables of classes in an vector `teal.slice:::.filterable_class`.

## Usage

``` r
get_supported_filter_varnames(data)
```

## Arguments

- data:

  the `R` object containing elements which class can be checked through
  `vapply` or `apply`.

## Value

`character` vector of variable names.

## Examples

``` r
# use non-exported function from teal.slice
get_supported_filter_varnames <- getFromNamespace("get_supported_filter_varnames", "teal.slice")

df <- data.frame(
  a = letters[1:3],
  b = 1:3,
  c = Sys.Date() + 1:3,
  d = Sys.time() + 1:3,
  z = complex(3)
)
get_supported_filter_varnames(df)
#> [1] "a" "b" "c" "d"
```
