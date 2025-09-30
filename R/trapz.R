

#' @title Trapzoidal Integration of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key \link[base]{character} scalar, see function [keyval.fv()]
#' 
#' @details
#' Functions [trapz.fv()] and [cumtrapz.fv()] 
#' obtain the (cumulative) 
#' \link[pracma]{trapz}oidal integration of the area under the primary outcome 
#' of a function value \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Functions [trapz.fv()] and [vtrapz.fv()] return a \link[base]{numeric} scalar.
#' 
#' @keywords internal
#' @name trapz_fv
#' @importFrom pracma trapz
#' @importFrom spatstat.explore fvnames
#' @export
trapz.fv <- function(x, key = fvnames(x, a = '.y')) {
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  trapz(x = x[[.x]], y = x[[key]]) |>
    unname()
}


#' @rdname trapz_fv
#' @returns 
#' Functions [cumtrapz.fv()] and [cumvtrapz.fv()] return a \link[base]{numeric} \link[base]{vector}.
#' @importFrom pracma cumtrapz
#' @importFrom spatstat.explore fvnames
#' @export 
cumtrapz.fv <- function(x, key = fvnames(x, a = '.y')) {
  
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  
  n <- length(x[[.x]])
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- cumtrapz(x = x[[.x]], y = x[[key]])
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- x[[.x]][-1L]
  return(ret)
  
}



