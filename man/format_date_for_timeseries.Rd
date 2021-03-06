% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/format-date-for-timeseries.R
\name{format_date_for_timeseries}
\alias{format_date_for_timeseries}
\title{Format year and month for timeseries}
\usage{
format_date_for_timeseries(year, month, day = 1, format = "\%Y-\%m")
}
\arguments{
\item{year}{numeric vector for year}

\item{month}{numeric vector for month}

\item{day}{numeric vecotr for day}

\item{format}{character string used with \code{\link{format.Date}}}
}
\description{
Format year and month for timeseries
}
\details{
\code{year}, \code{month}, and \code{day} must be of same length, otherwise they will be recycled to match the longest of them. See the details section of \code{\link{sprintf}}.
}

