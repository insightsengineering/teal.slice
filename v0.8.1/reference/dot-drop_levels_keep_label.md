# Drop unused factor levels while preserving label attribute

Helper function to drop unused levels from a factor variable while
preserving the `label` attribute. The base R
[`droplevels()`](https://rdrr.io/r/base/droplevels.html) function strips
all attributes except `levels` and `class`, which causes the loss of
variable labels that are commonly used in clinical trial datasets.

## Usage

``` r
.drop_levels_keep_label(x)
```

## Arguments

- x:

  (`factor`) A factor variable, potentially with a `label` attribute.

## Value

The input factor with unused levels dropped and the `label` attribute
preserved.
