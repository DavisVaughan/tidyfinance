% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_return.R
\name{calculate_return}
\alias{calculate_return}
\title{Calculate returns at specified intervals}
\usage{
calculate_return(.tbl_time, ..., type = "arithmetic", period = "daily",
  start_date = NULL, suffix = "return")
}
\arguments{
\item{.tbl_time}{A \code{tbl_time} object}

\item{...}{The columns to calculate returns for.
One or more unquoted column names separated by commas.}

\item{type}{Either \code{"arithmetic"} or \code{"log"} returns.}

\item{period}{A character specification used for time-based grouping. The
general format to use is \code{"frequency period"} where frequency is a number
like 1 or 2, and period is an interval like weekly or yearly. There must be
a space between the two.

Note that you can pass the specification in a flexible way:
\itemize{
\item 1 Year: \code{'1 year'} / \code{'1 Y'} / \code{'1 yearly'} / \code{'yearly'}
}

This shorthand is available for year, quarter, month, day, hour, minute,
second, millisecond and microsecond periodicities.

Additionally, you have the option of passing in a vector of dates to
use as custom and more flexible boundaries.}

\item{start_date}{Optional argument used to
specify the start date for the
first group. The default is to start at the closest period boundary
below the minimum date in the supplied index.}

\item{suffix}{For each column specified in \code{...}, this is the suffix that
is appended onto the name of the new column that corresponds to the return.}
}
\description{
Calculate arithmetic or log returns at specified periods.
}
\details{
These functions make no attempt to ensure that you have a full period in
your return calculations. This means that if you calculate monthly returns
from daily returns but you do not have a complete
month of returns for your first month, you may get a value that does not
make much sense. It is up to the user to keep this in mind!
}
