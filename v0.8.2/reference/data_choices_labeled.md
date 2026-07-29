# Returns a `choices_labeled` object

Returns a `choices_labeled` object

## Usage

``` r
data_choices_labeled(
  data,
  choices,
  varlabels = teal.data::col_labels(data, fill = TRUE),
  keys = character(0)
)
```

## Arguments

- data:

  (`data.frame` or `DFrame` or `list`) where labels can be taken from in
  case when `varlabels` is not specified. `data` must be specified if
  `varlabels` is not specified.

- choices:

  (`character`) the vector of chosen variables

- varlabels:

  (`character`) the labels of variables in data

- keys:

  (`character`) the names of the key columns in data

## Value

`character(0)` if choices are empty; a `choices_labeled` object
otherwise
