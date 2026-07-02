# Get hex code of the current Bootstrap theme color.

Determines the color specification for the currently active Bootstrap
color theme and returns one queried color.

## Usage

``` r
fetch_bs_color(color, alpha = NULL)
```

## Arguments

- color:

  (`character(1)`) naming one of the available theme colors

- alpha:

  either a `numeric(1)` or `character(1)` specifying transparency in the
  range of `0-1` or a hexadecimal value `00-ff`, respectively; set to
  NULL to omit adding the alpha channel

## Value

Named `character(1)` containing a hexadecimal color representation.

## Examples

``` r
fetch_bs_color <- getFromNamespace("fetch_bs_color", "teal.slice")
fetch_bs_color("primary")
#> [1] "#0d6efd"
fetch_bs_color("danger", 0.35)
#> [1] "#dc35455a"
fetch_bs_color("danger", "80")
#> [1] "#dc354580"
```
