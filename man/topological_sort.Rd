% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilteredData-utils.R
\name{topological_sort}
\alias{topological_sort}
\title{Topological graph sort}
\usage{
topological_sort(graph)
}
\arguments{
\item{graph}{(\verb{named list}) with node vector elements}
}
\description{
Graph is a \code{list} which for each node contains a vector of child nodes
in the returned list, parents appear before their children.
}
\details{
Implementation of \code{Kahn} algorithm with a modification to maintain the order of input elements.
}
\examples{
# use non-exported function from teal.slice
topological_sort <- getFromNamespace("topological_sort", "teal.slice")

topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
}
\keyword{internal}
