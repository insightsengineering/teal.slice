# Topological graph sort

Graph is a `list` which for each node contains a vector of child nodes
in the returned list, parents appear before their children.

## Usage

``` r
topological_sort(graph)
```

## Arguments

- graph:

  (`named list`) with node vector elements

## Details

Implementation of `Kahn` algorithm with a modification to maintain the
order of input elements.

## Examples

``` r
# use non-exported function from teal.slice
topological_sort <- getFromNamespace("topological_sort", "teal.slice")

topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
#> [[1]]
#> [1] "A"
#> 
#> [[2]]
#> [1] "B"
#> 
#> [[3]]
#> [1] "C"
#> 
#> [[4]]
#> [1] "D"
#> 
topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
#> [[1]]
#> [1] "A"
#> 
#> [[2]]
#> [1] "D"
#> 
#> [[3]]
#> [1] "B"
#> 
#> [[4]]
#> [1] "C"
#> 
topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
#> [[1]]
#> [1] "A"
#> 
#> [[2]]
#> [1] "D"
#> 
#> [[3]]
#> [1] "B"
#> 
#> [[4]]
#> [1] "C"
#> 
```
