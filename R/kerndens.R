
# @note
# TZhan does not think any function from package \CRANpkg{spatstat.univar}
# has the same functionality as [kerndens()]. She could be wrong.


#' @title Kernel Density via \link[stats]{density.default}
#' 
#' @description
#' Kernel density estimates only.
#' 
#' @param ... parameters of function \link[stats]{density.default}
#' 
#' @details
#' Function [kerndens()] finds 
#' the estimated density values,
#' i.e., element `$y` of function \link[stats]{density.default} return.
#' 
#' @returns 
#' Function [kerndens()] returns
#' a \link[base]{numeric} \link[base]{vector}.
#' 
#' @examples
#' d = density(faithful$eruptions, bw = 'sj')
#' stopifnot(identical(d$y, kerndens(faithful$eruptions, bw = 'sj')))
#' @keywords internal
#' @importFrom stats density.default
#' @export
kerndens <- function(...) density.default(...)$y


