

#' @title \link[stats]{loess} of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key \link[base]{character} scalar, see function [keyval.fv()]
#' 
#' @param ... additional parameters of functions 
#' \link[stats]{loess}, except for `formula` and `data`
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames as.data.frame.fv
#' @importFrom stats loess
#' @export
loess.fv <- function(x, key = fvnames(fv, a = '.y'), ...) {
  fv.nm <- substitute(x)
  fv <- x; x <- NULL # make code more readable
  .x <- fvnames(fv, a = '.x')
  force(key)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  fom <- eval(call(name = '~', as.symbol(key), as.symbol(.x)))
  lo <- loess(formula = fom, data = as.data.frame.fv(fv), ...)
  lo$call$formula <- fom
  lo$call$data <- call(name = 'as.data.frame', fv.nm) # a dumb user can use it after library(spatstat)
  lo$yname <- fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s loess')
  return(lo)
}

