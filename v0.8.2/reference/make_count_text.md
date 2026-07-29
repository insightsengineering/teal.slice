# Build count text

Returns a text label describing filtered counts. The text is composed in
the following way:

- when `countnow` is not `NULL`: `<label> (<countnow>/<countmax>)`

- when `countnow` is `NULL`: `<label> (<countmax>)`

## Usage

``` r
make_count_text(label, countmax, countnow = NULL)
```

## Arguments

- label:

  (`character(1)`) Text displayed before counts.

- countmax:

  (`numeric(1)`) Number of unfiltered counts.

- countnow:

  (`numeric(1)`) Number of filtered counts.

## Value

A character string.
