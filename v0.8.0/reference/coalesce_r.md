# Recursively coalesce list elements.

Returns first element of list that it not `NULL`, recursively.

## Usage

``` r
coalesce_r(x)
```

## Arguments

- x:

  (`list`), either of atomic vectors or of named lists

## Value

Either an atomic vector of length 1 or a (potentially nested) list.

## Details

Given a list of atomic vectors, the first non-null element is returned.
Given a list of lists, for all `names` found in all elements of the list
the first non-null element of a given name is returned.

This function is used internally in `c.teal_slices` to manage
`teal_slices` attributes.
