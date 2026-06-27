# Get classes of selected columns from dataset

Get classes of selected columns from dataset

## Usage

``` r
variable_types(data, columns = NULL)
```

## Arguments

- data:

  (`data.frame` or `DataFrame` or `matrix`) Object in which to determine
  variable types.

- columns:

  (`character`) Vector of columns in `data` for which to get types. Set
  to `NULL` to get types of all columns.

## Value

Character vector of classes of `columns` from provided `data`.

## Examples

``` r
# use non-exported function from teal.slice
variable_types <- getFromNamespace("variable_types", "teal.slice")

variable_types(
  data.frame(
    x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
    stringsAsFactors = FALSE
  ),
  "x"
)
#>         x 
#> "integer" 

variable_types(
  data.frame(
    x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
    stringsAsFactors = FALSE
  ),
  c("x", "z")
)
#>           x           z 
#>   "integer" "character" 

variable_types(
  data.frame(
    x = 1:3, y = factor(c("a", "b", "a")), z = c("h1", "h2", "h3"),
    stringsAsFactors = FALSE
  )
)
#>           x           y           z 
#>   "integer"    "factor" "character" 
```
