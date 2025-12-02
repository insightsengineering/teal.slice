# Gets filter expression for multiple `datanames` taking into account its order.

To be used in `Show R Code` button.

## Usage

``` r
get_filter_expr(datasets, datanames = datasets$datanames())
```

## Arguments

- datasets:

  (`FilteredData`)

- datanames:

  (`character`) vector of dataset names

## Value

A character string containing all subset expressions.
