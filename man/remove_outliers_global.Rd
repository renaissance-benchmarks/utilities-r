% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{remove_outliers_global}
\alias{remove_outliers_global}
\title{Remove outliers.}
\usage{
remove_outliers_global(.input, .column, ...)
}
\arguments{
\item{.input}{Data.}

\item{.column}{Column to identify outliers in.}

\item{...}{Parameters to \code{\link[=identify_vector_outliers_global]{identify_vector_outliers_global()}}.}
}
\value{
Tibble with rows filtered.
}
\description{
Uses \code{\link[=identify_vector_outliers_global]{identify_vector_outliers_global()}} to identify outliers in given column. Then, removes the outlier rows.
}
