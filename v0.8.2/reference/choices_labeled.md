# Set "`<choice>:<label>`" type of names

This is often useful for as it marks up the drop-down boxes for
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

## Usage

``` r
choices_labeled(choices, labels, types = NULL)
```

## Arguments

- choices:

  (`character` or `numeric` or `logical`) vector

- labels:

  (`character`) vector containing labels to be applied to `choices`. If
  `NA` then "Label Missing" will be used.

- types:

  vector containing the types of the columns.

## Value

A named character vector.

## Details

If either `choices` or `labels` are factors, they are coerced to
character. Duplicated elements from `choices` get removed.
