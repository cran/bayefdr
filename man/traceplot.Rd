% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traceplot.R
\name{traceplot}
\alias{traceplot}
\title{Trace, marginal density histogram, and autocorrelation plot of MCMC draws.}
\usage{
traceplot(x, ylab = NULL, log = FALSE)
}
\arguments{
\item{x}{A vector of MCMC draws.}

\item{ylab}{An optional y-axis label.}

\item{log}{Logical scalar controlling whether the y-axis should be logged.}
}
\value{
A plot created using \code{\link[cowplot]{plot_grid}} showing
the trace, marginal density histogram, and autocorrelation function of the
MCMC draws in \code{x}.
}
\description{
Trace, marginal density histogram, and autocorrelation plot of MCMC draws.
}
\examples{
x <- rnorm(1000)
traceplot(x)
}
