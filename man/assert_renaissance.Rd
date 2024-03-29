% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{assert_renaissance}
\alias{assert_renaissance}
\title{Check whether data resembles typical measurement results.}
\usage{
assert_renaissance(.input, .var.name = vname(.data), add = NULL, ...)
}
\arguments{
\item{.input}{Measurement results to test.}

\item{.var.name}{Internal.}

\item{add}{Internal.}

\item{...}{Parameters to \code{\link[=check_renaissance]{check_renaissance()}}.}
}
\value{
Invisible .data or an exception.
}
\description{
Check whether data resembles typical measurement results.
}
\seealso{
\link{check_renaissance}
}
