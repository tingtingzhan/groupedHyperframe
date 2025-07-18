
#' @title Black-Solid-Curve in \link[spatstat.explore]{plot.fv}
#' 
#' @description
#' Name and value of the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' i.e., the primary outcome of an \link[spatstat.explore]{fv.object}. 
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom) # ?spatstat.geom::subset.ppp
#' library(spatstat.explore)
#' 
#' foo = function(x) {
#'  x |> plot()
#'  'Trapzoid\n' |> cat()
#'  x |> trapz.fv() |> print()
#'  '\nCumulative Trapzoid\n' |> cat()
#'  x |> cumtrapz.fv() |> tail() |> print()
#' }
#' 
#' # numeric mark
#' spruces |> Emark() |> foo()
#' spruces |> Vmark() |> foo()
#' spruces |> markcorr() |> foo()
#' spruces |> markvario() |> foo()
#' 
#' # multitype mark
#' (btc = subset.ppp(betacells, select = 'type'))
#' btc |> Gcross(i = 'off', j = 'on') |> foo()
#' btc |> Kcross(i = 'off', j = 'on') |> foo()
#' btc |> Lcross(i = 'off', j = 'on') |> foo()
#' btc |> Jcross(i = 'off', j = 'on') |> foo()
#' btc |> markconnect(i = 'off', j = 'on') |> foo()
#' 
#' # swedishpines |> roc.ppp(covariate = 'x') |> foo() 
#' # bug in devtools::check()
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
#' Function [key1val.fv()] finds the value of the primary outcome
#' of an \link[spatstat.explore]{fv.object}.
#' @returns
#' Function [key1val.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' @export
key1val.fv <- function(x, key = key1.fv(x)) {
  ret <- x[[key]]
  names(ret) <- x[['r']] # `r` being hard-coded here
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
#' Function [trapz.fv()] returns a \link[base]{numeric} scalar.
#' 
#' @importFrom pracma trapz
#' @export
trapz.fv <- function(x, key = key1.fv(x)) {
  x |> 
    key1val.fv(key = key) |>
    trapz(x = x[[1L]], y = _) |> # which way is more robust?
    #trapz(x = x[['r']], y = _) |> # which way is more robust?
    unname()
}







#' @rdname key1
#' @returns 
#' Function [cumtrapz.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' @importFrom pracma cumtrapz
#' @export 
cumtrapz.fv <- function(x, key = key1.fv(x)) {
  
  # 'fv' inherits from 'data.frame', as of 2025-02-04 # packageDate('spatstat.explore')
  r <- x[[1L]]
  n <- length(r)
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- x |> 
    key1val.fv(key = key) |> 
    unclass() |>
    cumtrapz(x = r, y = _)
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- r[-1L]
  return(ret)
  
}





#' @rdname key1
#' @details
#' Function [is.finite.fv()] (dispatch of S3 generic \link[base]{is.finite}) 
#' finds the \link[base]{finite},
#' i.e., non-`NA`, non-`NaN` and non-`Inf`,
#' indices of the primary outcome of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns 
#' Function [is.finite.fv()] returns a \link[base]{logical} \link[base]{vector}.
#' 
#' @method is.finite fv
#' @export is.finite.fv
#' @export
is.finite.fv <- function(x) {
  x |> key1val.fv() |> is.finite()
}






