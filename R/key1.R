
#' @title Black-Solid-Curve in \link[spatstat.explore]{plot.fv}
#' 
#' @description
#' Name and value of the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' i.e., the primary outcome of an \link[spatstat.explore]{fv.object}. 
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @keywords internal
#' @name key1
NULL


#' @rdname key1
#' 
#' @param key \link[base]{character} scalar, default value is `spatstat.explore::fvnames(x, a = '.y')`,
#' to speed up batch processes.
#' 
#' @details
#' Function [keyval.fv()] finds the value of the (primary) outcome
#' of an \link[spatstat.explore]{fv.object}.
#' @returns
#' Function [keyval.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' @importFrom spatstat.explore fvnames
#' @export
keyval.fv <- function(x, key = fvnames(x, a = '.y')) {
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  ret <- x[[key]]
  names(ret) <- x[[.x]]
  return(ret)
}


#' @rdname key1
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
#' @importFrom pracma trapz
#' @importFrom spatstat.explore fvnames
#' @export
trapz.fv <- function(x, key = fvnames(x, a = '.y')) {
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  trapz(x = x[[.x]], y = x[[key]]) |>
    unname()
}

#' @rdname key1
#' @importFrom spatstat.explore fvnames
#' @export
vtrapz.fv <- function(x, key = fvnames(x, a = '.y')) {
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  vtrapz(x = x[[.x]], y = x[[key]]) |>
    unname()
}




#' @rdname key1
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

#' @rdname key1
#' @importFrom spatstat.explore fvnames
#' @export 
cumvtrapz.fv <- function(x, key = fvnames(x, a = '.y')) {
  
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  
  n <- length(x[[.x]])
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- cumvtrapz(x = x[[.x]], y = x[[key]])
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- x[[.x]][-1L]
  return(ret)
  
}






