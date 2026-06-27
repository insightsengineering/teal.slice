# Converts a list to a `JSON` string

Converts a list representation of `teal_slice` or `teal_slices` into a
`JSON` string. Ensures proper unboxing of list elements. This function
is used by the `format` methods for `teal_slice` and `teal_slices`.

## Usage

``` r
to_json(x)
```

## Arguments

- x:

  (`list`) representation of `teal_slices` object.

## Value

A `JSON` string.
