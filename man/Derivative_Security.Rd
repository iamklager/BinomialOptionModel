% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Derivative_Security.R
\name{Derivative_Security}
\alias{Derivative_Security}
\title{Derivative Security}
\usage{
Derivative_Security(S, K, N, r, d, u, type)
}
\arguments{
\item{S}{Stock Price.}

\item{K}{Strike Price.}

\item{N}{Timepoint until the derivative price will be calculated.}

\item{r}{Interest Rate of Bank account.}

\item{d}{Down-Factor of stock price decline.}

\item{u}{Up-Factor of stock price increase.}

\item{type}{Type of Option, possible values = European_Call/European_Put, Asian_Call/Asian_Put, American_Call/American_Put.}
}
\value{
Returns a list with two matrixes, one for derivative prices and one for delta amounts.
}
\description{
Description
}
\examples{
Derivative_Security(S = 4, K = 4, N = 4, r = 0.25, d = 0.5, u = 2, type = "European_put")
}
