% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawdown.R
\name{drawdown}
\alias{drawdown}
\alias{drawdown_max}
\title{Calculate the drawdown for a vector of returns}
\usage{
drawdown(.r, type = "arithmetic")

drawdown_max(.r, type = "arithmetic")
}
\arguments{
\item{.r}{A vector of returns.}

\item{type}{Either \code{"arithmetic"} or \code{"log"} returns.}
}
\description{
\code{drawdown()} calculates a vector of drawdowns. \code{drawdown_max()} calculates
the single largest negative drawdown over the period of returns specified.
}
\details{
Drawdown is a measure of how long it takes to recoup losses after a fall
from a previous high. The more negative the drawdown, the larger the loss.
The longer it takes to get back to 0 drawdown, the longer you are
"underwater."
}
