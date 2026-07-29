# Default `teal_slice` id

Create a slice id if none provided.

## Usage

``` r
get_default_slice_id(x)
```

## Arguments

- x:

  (`teal_slice` or `list`)

## Value

(`character(1)`) `id` for a `teal_slice` object.

## Details

Function returns a default `id` for a `teal_slice` object which needs to
be distinct from other `teal_slice` objects created for any
`FilterStates` object. Returned `id` can be treated as a location of a
vector on which `FilterState` is built:

- for a `data.frame` `id` concatenates `dataname` and `varname`.

- for a `MultiAssayExperiment` `id` concatenates `dataname`, `varname`,
  `experiment` and `arg`, so that one can add `teal_slice` for a
  `varname` which exists in multiple `SummarizedExperiment`s or exists
  in both `colData` and `rowData` of given experiment. For such a vector
  `teal.slice` doesn't allow to activate more than one filters. In case
  of `teal_slice_expr` `id` is mandatory and must be unique.
