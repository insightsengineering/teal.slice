# Include `JS` files from `/inst/js/` package directory to application header

`system.file` should not be used to access files in other packages, it
does not work with `devtools`. Therefore, we redefine this method in
each package as needed. Thus, we do not export this method.

## Usage

``` r
include_js_files(pattern)
```

## Arguments

- pattern:

  (`character`) pattern of files to be included, passed to `system.file`

## Value

HTML code that includes `JS` files
