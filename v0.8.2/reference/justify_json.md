# Justify colons in `JSON` string

This function takes a `JSON` string as input, splits it into lines, and
pads element names with spaces so that colons are justified between
lines.

## Usage

``` r
justify_json(json)
```

## Arguments

- json:

  (`character(1)`) a `JSON` string.

## Value

A list of character strings, which can be collapsed into a `JSON`
string.
