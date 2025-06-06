% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grouped_ppp.R
\name{grouped_ppp}
\alias{grouped_ppp}
\title{\link{groupedHyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}}
\usage{
grouped_ppp(
  formula,
  data,
  coords = ~x + y,
  window = owin(xrange = range(.x), yrange = range(.y)),
  ...
)
}
\arguments{
\item{formula}{\link[stats]{formula} in the format of
\code{m1+m2 ~ y+x1+x2 | g1/g2},
where \eqn{m_i}'s are one or more \link[spatstat.geom]{marks},
\eqn{y} and \eqn{x_j}'s are the endpoint and predictor(s) for downstream analysis,
and \eqn{g_k} are one or more nested grouping structure}

\item{data}{\link[base]{data.frame}}

\item{coords}{\link[stats]{formula}, variable names
of \eqn{x}- and \eqn{y}-coordinates in \code{data}.
Default \code{~x+y}.}

\item{window}{an observation window \link[spatstat.geom]{owin},
default is the \eqn{x}- and \eqn{y}-span of \code{coords} in \code{data}.}

\item{...}{additional parameters, currently not in use}
}
\value{
Function \code{\link[=grouped_ppp]{grouped_ppp()}} returns a \link{groupedHyperframe}
with \emph{\strong{one-and-only-one}}
\link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
}
\description{
..
}
\keyword{internal}
