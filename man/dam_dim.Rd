% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dam_data.R
\name{dam_dim}
\alias{dam_dim}
\title{Retrieve dam Dimensions from a Dam Table}
\usage{
dam_dim(dam_table, dim_regex, ignore_case = TRUE, vec = FALSE)
}
\arguments{
\item{dam_table}{\code{(tbl)} from which to pull the metrics}

\item{dim_regex}{\code{(chr)} regex on which to match the dimension name}

\item{ignore_case}{Should case differences be ignored in the match?}

\item{vec}{\code{(chr)} whether to return a vector \code{TRUE} or the filtered \code{tbl}}
}
\value{
\code{(named vector)}
}
\description{
Retrieve dam Dimensions from a Dam Table
}
