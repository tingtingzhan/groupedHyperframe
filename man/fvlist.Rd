% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fvlist.R
\name{fvlist}
\alias{fvlist}
\alias{check_fvlist}
\alias{key1val.fvlist}
\alias{cumtrapz.fvlist}
\title{Functions for \link[stats]{listof} \link[spatstat.explore]{fv.object}s}
\usage{
check_fvlist(X)

key1val.fvlist(X, check = TRUE)

cumtrapz.fvlist(
  X,
  check = TRUE,
  mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores())
)
}
\arguments{
\item{X}{a \link[stats]{listof} \link[spatstat.explore]{fv.object}s}

\item{check}{\link[base]{logical} scalar, an option to suppress
function \code{\link[=check_fvlist]{check_fvlist()}} in a batch process.
Default \code{TRUE}}

\item{mc.cores}{\link[base]{integer} scalar, see function \link[parallel]{mclapply}.
Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
CRAN requires \code{mc.cores <= 2L} in examples.}
}
\value{
Function \code{\link[=check_fvlist]{check_fvlist()}} does not have a returned value.

Function \code{\link[=cumtrapz.fvlist]{cumtrapz.fvlist()}} returns a \link[stats]{listof} \link[base]{double} \link[base]{vector}s.
}
\description{
Functions for \link[stats]{listof} \link[spatstat.explore]{fv.object}s
}
\details{
Function \code{\link[=check_fvlist]{check_fvlist()}} checks that
\itemize{
\item {if \eqn{x}-axis of all \link[spatstat.explore]{fv.object}s are all same}
\item {\code{attr(,'fname')} of all \link[spatstat.explore]{fv.object}s are all same}
}

Note that
\itemize{
\item {function \code{\link[=key1.fv]{key1.fv()}} returns of all \link[spatstat.explore]{fv.object}s are not required to be all same}
}

Function \code{\link[=key1val.fvlist]{key1val.fvlist()}} gathers the primary outcome
of the \link[spatstat.explore]{fv.object}s.

Function \code{\link[=cumtrapz.fvlist]{cumtrapz.fvlist()}} is a batch process of function \code{\link[=cumtrapz.fv]{cumtrapz.fv()}}.
}
\keyword{internal}
