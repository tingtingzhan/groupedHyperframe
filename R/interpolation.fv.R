

#' @title \link[stats]{approxfun} of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key \link[base]{character} scalar, see function [keyval.fv()]
#' 
#' @param ... additional parameters of functions 
#' \link[stats]{approxfun} and \link[stats]{splinefun}.
#' 
#' @keywords internal
#' @name interpolation_fv
#' @importFrom spatstat.explore fvnames
#' @importFrom stats approxfun
#' @export
approxfun.fv <- function(x, key = fvnames(fv, a = '.y'), ...) {
  fv <- x; x <- NULL # make code more readable
  .x <- fvnames(fv, a = '.x')
  force(key)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  fn <- approxfun(x = fv[[.x]], y = fv[[key]], ...)
  fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s linear interpolation') |>
    assign(x = 'yname', value = _, envir = environment(fn))
  assign(x = 'xname', value = .x, envir = environment(fn))
  return(fn)
}



#' @rdname interpolation_fv
#' @importFrom spatstat.explore fvnames
#' @importFrom stats splinefun
#' @export
splinefun.fv <- function(x, key = fvnames(fv, a = '.y'), ...) {
  fv <- x; x <- NULL # make code more readable
  .x <- fvnames(fv, a = '.x')
  force(key)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  fn <- splinefun(x = fv[[.x]], y = fv[[key]], ...)
  if (FALSE) {
    # fn |> environment() |> ls(envir = _) # only 'z' !!!
    z <- fn |> environment() |> get('z', envir = _)
  }
  fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s spline interpolation') |>
    assign(x = 'yname', value = _, envir = environment(fn))
  assign(x = 'xname', value = .x, envir = environment(fn))
  return(fn)
}
