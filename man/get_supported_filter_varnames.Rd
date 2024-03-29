% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilterStates-utils.R
\name{get_supported_filter_varnames}
\alias{get_supported_filter_varnames}
\title{Gets supported filterable variable names}
\usage{
get_supported_filter_varnames(data)
}
\arguments{
\item{data}{the \code{R} object containing elements which class can be checked through \code{vapply} or \code{apply}.}
}
\value{
\code{character} vector of variable names.
}
\description{
Gets filterable variable names from a given object. The names match variables
of classes in an vector \code{teal.slice:::.filterable_class}.
}
\examples{
# use non-exported function from teal.slice
get_supported_filter_varnames <- getFromNamespace("get_supported_filter_varnames", "teal.slice")

df <- data.frame(
  a = letters[1:3],
  b = 1:3,
  c = Sys.Date() + 1:3,
  d = Sys.time() + 1:3,
  z = complex(3)
)
get_supported_filter_varnames(df)
}
\keyword{internal}
