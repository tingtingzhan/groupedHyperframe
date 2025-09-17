

#' @title Quantiles of \link[base]{numeric} \link[spatstat.geom]{marks} in \link[spatstat.geom]{ppp.object}
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param ... additional parameters of function `stats:::quantile.default()`
#' 
#' @keywords internal
#' @importFrom stats quantile
#' @export quantile.ppp
#' @export
quantile.ppp <- function(x, ...) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      m[id] |> 
        lapply(FUN = quantile, ...)
    }, 'vector' = {
      if (is.numeric(m)) return(quantile(m, ...))
      return(invisible())
    }, 'none' = {
      return(invisible())
    })
  
}



#' @title Quantiles of \link[base]{numeric} \link[spatstat.geom]{marks} in `'ppplist'`
#' 
#' @param x a `'ppplist'`
#' 
#' @param ... additional parameters of function `stats:::quantile.default()`
#' 
#' @keywords internal
#' @importFrom stats quantile
#' @export quantile.ppplist
#' @export
quantile.ppplist <- function(x, ...) {
  
  z <- x |>
    lapply(FUN = quantile.ppp, ...)
  
  nm. <- z |>
    lapply(FUN = names)
  if (!all(duplicated.default(nm.)[-1L])) stop('ppp.objects not having same numeric marks?')
  
  ret <- z |> 
    .mapply(FUN = list, dots = _, MoreArgs = NULL)
  names(ret) <- nm.[[1L]]
  return(ret)
  
}


#' @title Quantiles of \link[base]{numeric} \link[spatstat.geom]{marks} in \link[spatstat.geom]{anylist}
#' 
#' @param x an \link[spatstat.geom]{anylist}
#' 
#' @param ... additional parameters of function `stats:::quantile.default()`
#' 
#' @keywords internal
#' @importFrom stats quantile
#' @export quantile.anylist
#' @export
quantile.anylist <- function(x, ...) {
  
  x_num <- x |>
    vapply(FUN = is.vector, mode = 'numeric', FUN.VALUE = NA)
  if (!all(x_num)) return(invisible()) # exception handling
  
  x |> 
    lapply(FUN = quantile, ...)
  
}




#' @title Quantiles of \link[base]{numeric} elements in \link[spatstat.geom]{hyperframe}
#' 
#' @param x a \link[spatstat.geom]{hyperframe}
#' 
#' @param ... additional parameters of function `stats:::quantile.default()`
#' 
#' @keywords internal
#' @importFrom stats quantile
#' @export quantile.hyperframe
#' @export
quantile.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  # 'numeric' 'marks' in 'ppp'-`hypercolumns`
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  n_ppp <- sum(hc_ppp)
  if (n_ppp > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  if (n_ppp == 1L) {
    mark. <- hc[[which(hc_ppp)]] |>
      quantile.ppplist(...)
  } else mark. <- NULL
  
  # 'numeric'-`hypercolumns`
  numlist_ <- hc |>
    lapply(FUN = quantile.anylist, ...)
  numlist. <- numlist_[lengths(numlist_) > 0L]
  
  z <- c(numlist., mark.)
  names(z) <- names(z) |>
    sprintf(fmt = '%s.quantile')
  
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), z)
  ))

}



