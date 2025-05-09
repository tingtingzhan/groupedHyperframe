% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nncross.R
\name{.nncross}
\alias{.nncross}
\title{Alternative Interface of \link[spatstat.geom]{nncross.ppp}}
\usage{
.nncross(X, i, j, ...)
}
\arguments{
\item{X}{see \strong{Details}}

\item{i, j}{\link[base]{character} or \link[base]{integer} scalars.
See functions \link[spatstat.explore]{Gcross}, etc. for more details}

\item{...}{additional parameters of \link[spatstat.geom]{nncross.ppp}}
}
\value{
Function \code{\link[=.nncross]{.nncross()}} returns
a \link[base]{numeric} \link[base]{vector}
if \code{i} and \code{j} are valid mark values of \link[spatstat.geom]{ppp.object} \code{X};
otherwise returns a \code{NULL} value.
}
\description{
An alternative interface of function \link[spatstat.geom]{nncross.ppp}.
}
\details{
Function \code{\link[=.nncross]{.nncross()}} creates an interface similar to
functions \link[spatstat.explore]{Gcross}, etc.,
which takes an \link[spatstat.geom]{is.multitype} \link[spatstat.geom]{ppp.object}
and two mark values \code{i} and \code{j},
then calls the workhorse function
\link[spatstat.geom]{nncross.ppp} with parameter \code{what = 'dist'}.
If mark values \code{i} and \code{j} does not exist in the \link[spatstat.geom]{ppp.object},
a \code{NULL} value will be returned.
}
\examples{
library(spatstat.data)
library(spatstat.geom)

(xs = split.ppp(amacrine))
(a1 = nncross(X = xs$off, Y = xs$on, what = 'dist'))
a2 = .nncross(amacrine, i = 'off', j = 'on')
a3 = .nncross(amacrine, i = 1L, j = 2L)
stopifnot(identical(a1, a2), identical(a1, a3))

.nncross(amacrine, i = 'a', j = 'b') # exception handling
}
\keyword{internal}
