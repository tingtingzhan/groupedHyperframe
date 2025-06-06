% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kerndens.R
\name{kerndens}
\alias{kerndens}
\title{Kernel Density via \link[stats]{density.default}}
\usage{
kerndens(...)
}
\arguments{
\item{...}{parameters of function \link[stats]{density.default}}
}
\value{
Function \code{\link[=kerndens]{kerndens()}} returns
a \link[base]{numeric} \link[base]{vector}.
}
\description{
Kernel density estimates only.
}
\details{
Function \code{\link[=kerndens]{kerndens()}} finds
the estimated density values,
i.e., element \verb{$y} of function \link[stats]{density.default} return.
}
\examples{
d = density(faithful$eruptions, bw = 'sj')
stopifnot(identical(d$y, kerndens(faithful$eruptions, bw = 'sj')))
}
\keyword{internal}
