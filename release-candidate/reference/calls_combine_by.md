# Compose predicates

Combines calls with a logical operator.

## Usage

``` r
calls_combine_by(calls, operator)
```

## Arguments

- calls:

  (`list`) containing calls (or symbols) to be combined by `operator`

- operator:

  (`character(1)`) infix operator to use in predicate composition,
  *e.g.* `"&"`

## Value

A `call` where elements of `calls` are composed with `operator` or
`NULL` if `calls` is an empty list.

## Details

This function is used to combine logical predicates produced by
`FilterState` objects to build a complete subset expression.

## Examples

``` r
# use non-exported function from teal.slice
calls_combine_by <- getFromNamespace("calls_combine_by", "teal.slice")

calls <- list(
  quote(SEX == "F"), # subsetting on factor
  quote(AGE >= 20 & AGE <= 50), # subsetting on range
  quote(!SURV) # subsetting on logical
)
calls_combine_by(calls, "&")
#> SEX == "F" & (AGE >= 20 & AGE <= 50) & !SURV
```
