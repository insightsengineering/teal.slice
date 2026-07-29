# Build concatenating call

This function takes a vector of values and returns a `c` call. If the
vector has only one element, the element is returned directly.

## Usage

``` r
make_c_call(choices)
```

## Arguments

- choices:

  A vector of values.

## Value

A `c` call.

## Examples

``` r
# use non-exported function from teal.slice
make_c_call <- getFromNamespace("make_c_call", "teal.slice")
make_c_call(1:3)
#> c(1L, 2L, 3L)
make_c_call(1)
#> [1] 1
```
