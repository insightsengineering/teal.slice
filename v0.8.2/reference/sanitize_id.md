# Encodes ids to be used in JavaScript and Shiny

Replaces non-ASCII characters into a format that can be used in HTML,
JavaScript and Shiny.

When the id has a character that is not allowed, it is replaced with
`"_"` and a 4 character hash of the original id is added to the start of
the resulting id.

## Usage

``` r
sanitize_id(id)
```

## Arguments

- id:

  (`character(1)`) The id string.

## Value

Sanitized string that removes special characters and spaces.
