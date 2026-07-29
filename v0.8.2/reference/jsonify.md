# Convert a list to a justified `JSON` string

This function takes a list and converts it to a `JSON` string. The
resulting `JSON` string is then optionally justified to improve
readability and trimmed to easier fit in the console when printing.

## Usage

``` r
jsonify(x, trim_lines)
```

## Arguments

- x:

  (`list`), possibly recursive, obtained from `teal_slice` or
  `teal_slices`.

- trim_lines:

  (`logical(1)`) flag specifying whether to trim lines of the `JSON`
  string.

## Value

A `JSON` string representation of the input list.
