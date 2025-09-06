
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






# plot(x <- pcfcross(btc, i = 'off', j = 'on')) 
# head(x$iso) # why first is Inf ??
# trapz.fv(x)
# x |> cumtrapz.fv() |> tail()




#' @rdname key1
#' @details
#' Function [key1.fv()] finds the name of the primary outcome
#' of an \link[spatstat.explore]{fv.object}.  
#' Note that function [key1.fv()] is very slow, so avoid it as much as possible in batch processes!
#' @returns
#' Function [key1.fv()] returns a \link[base]{character} scalar.
#' @importFrom grDevices dev.off png
#' @importFrom spatstat.explore plot.fv
#' @export
key1.fv <- function(x) {
  x |>
    plot.fv(do.plot = FALSE) |>  # 'data.frame'
    with(expr = key[lty == 1L & col == 1L])
  # must not use `with.default`; ?devtools::check warns on unknown `key` and `lty`
}




#' @rdname key1
#' 
#' @param key \link[base]{character} scalar, default value is `key1.fv(x)`,
#' to speed up batch processes.
#' 
#' @details
#' Function [keyval.fv()] finds the value of the (primary) outcome
#' of an \link[spatstat.explore]{fv.object}.
#' @returns
#' Function [keyval.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' @export
keyval.fv <- function(x, key = key1.fv(x)) {
  # ?spatstat.explore::roc.ppp returns an `'roc'` object, inherits from `'fv'`, first argument being `p` instead of `r`!!!
  if (key == names(x)[1L]) stop('first column of `x` is not the output of `fv.object`')
  ret <- x[[key]]
  names(ret) <- x[[1L]]
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
#' @export
trapz.fv <- function(x, key = key1.fv(x)) {
  # ?spatstat.explore::roc.ppp returns an `'roc'` object, inherits from `'fv'`, first argument being `p` instead of `r`!!!
  x |> 
    keyval.fv(key = key) |>
    trapz(x = x[[1L]], y = _) |>
    unname()
}

#' @rdname key1
#' @export
vtrapz.fv <- function(x, key = key1.fv(x)) {
  # ?spatstat.explore::roc.ppp returns an `'roc'` object, inherits from `'fv'`, first argument being `p` instead of `r`!!!
  x |> 
    keyval.fv(key = key) |>
    vtrapz(x = x[[1L]], y = _) |>
    unname()
}




#' @rdname key1
#' @returns 
#' Functions [cumtrapz.fv()] and [cumvtrapz.fv()] return a \link[base]{numeric} \link[base]{vector}.
#' @importFrom pracma cumtrapz
#' @export 
cumtrapz.fv <- function(x, key = key1.fv(x)) {
  
  # 'fv' inherits from 'data.frame', as of 2025-02-04 # packageDate('spatstat.explore')
  r <- x[[1L]]
  n <- length(r)
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- x |> 
    keyval.fv(key = key) |> 
    unclass() |>
    cumtrapz(x = r, y = _)
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- r[-1L]
  return(ret)
  
}

#' @rdname key1
#' @export 
cumvtrapz.fv <- function(x, key = key1.fv(x)) {
  
  # 'fv' inherits from 'data.frame', as of 2025-02-04 # packageDate('spatstat.explore')
  r <- x[[1L]]
  n <- length(r)
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- x |> 
    keyval.fv(key = key) |> 
    unclass() |>
    cumvtrapz(x = r, y = _)
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- r[-1L]
  return(ret)
  
}






