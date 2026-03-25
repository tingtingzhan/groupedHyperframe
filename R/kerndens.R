
# @note
# TZhan does not think any function from package \CRANpkg{spatstat.univar}
# has the same functionality as [kerndens()]. She could be wrong.

#' @title Kernel Density via \link[stats]{density.default}
#' 
#' @description
#' Kernel density estimates only.
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of the function \link[stats]{density.default}
#' 
#' @note
#' Do *not* overwrite function `spatstat.explore::density.ppp()` !!
#' 
#' @details
#' The `S3` generic function [kerndens()] finds 
#' the estimated density values,
#' i.e., element `$y` of the function \link[stats]{density.default} return.
#' 
#' @returns 
#' The `S3` generic function [kerndens()] returns
#' a \link[base]{numeric} \link[base]{vector}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name kerndens
#' @export
kerndens <- function(x, ...) UseMethod(generic = 'kerndens')

#' @rdname kerndens
#' @export
kerndens.numeric <- function(x, ...) density.default(x, ...)$y


#' @rdname kerndens
#' @export
kerndens.ppp <- function(x, ...) {
  d <- x |>
    density_marks.ppp(...)
  if (!length(d)) return(invisible())
  if (inherits(d, what = 'density')) return(d$y)
  if (length(d) == 1L) return(d[[1L]]$y)
  d |> 
    lapply(FUN = \(i) i$y)
}

