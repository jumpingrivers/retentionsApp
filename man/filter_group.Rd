% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_calculate_retention.R
\name{filter_group}
\alias{filter_group}
\title{Filter group}
\usage{
filter_group(df, filter_by, group, values)
}
\arguments{
\item{df}{Tibble containing retention data}

\item{filter_by}{Which filters are selected e.g. gender}

\item{group}{Current filter e.g. gender}

\item{values}{Current values e.g. Male}
}
\description{
Filter values based on user inputs, only if
group is actually selected.
}
