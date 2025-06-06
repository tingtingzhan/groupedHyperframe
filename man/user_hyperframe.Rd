% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_hyperframe.R
\name{user_hyperframe}
\alias{user_hyperframe}
\alias{Emark_}
\alias{Vmark_}
\alias{Kmark_}
\alias{markcorr_}
\alias{markvario_}
\alias{Gcross_}
\alias{Jcross_}
\alias{Kcross_}
\alias{Lcross_}
\alias{nncross_}
\title{User Interface of Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}}
\usage{
Emark_(X, correction = "none", ...)

Vmark_(X, correction = "none", ...)

Kmark_(X, correction = "none", ...)

markcorr_(X, correction = "none", ...)

markvario_(X, correction = "none", ...)

Gcross_(X, correction = "none", ...)

Jcross_(X, correction = "none", ...)

Kcross_(X, correction = "none", ...)

Lcross_(X, correction = "none", ...)

nncross_(X, ...)
}
\arguments{
\item{X}{a \link[spatstat.geom]{hyperframe}}

\item{correction}{\link[base]{character} scalar,
see functions
\link[spatstat.explore]{markcorr},
\link[spatstat.explore]{Gcross},
etc.
Default \code{'none'} to save computing time.}

\item{...}{additional parameters of user operation}
}
\value{
See explanations in workhorse function \code{\link[=op_hyperframe]{op_hyperframe()}}.
}
\description{
See workhorse function \code{\link[=op_hyperframe]{op_hyperframe()}}.
}
\details{
See explanations in workhorse function \code{\link[=op_hyperframe]{op_hyperframe()}}.
}
\examples{
library(spatstat.data)
library(spatstat.geom)
# no good example for [Emark_.hyperframe]
# no hyperframe with ppp-hypercolumn with numeric marks

flu$pattern[] = flu$pattern |> 
 lapply(FUN = `mark_name<-`, value = 'stain') # read ?flu carefully
 
r = seq.int(from = 0, to = 100, by = 5)
flu |>
 subset(stain == 'M2-M1') |>
 Gcross_(i = 'M1', j = 'M2', r = r, correction = 'best', mc.cores = 1L)
 
flu |>
 subset(stain == 'M2-M1') |>
 nncross_(i = 'M1', j = 'M2', mc.cores = 1L)
}
\keyword{internal}
