
#' @title Augment Hypercolumn(s) for \link[mgcv]{gam}
#' 
#' @description
#' Augment *all* \link[base]{numeric} \link[base]{vector} hypercolumns in a \link[spatstat.geom]{hyperframe} 
#' for \link[mgcv]{gam}.
#' 
#' 
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @author 
#' Tingting Zhan, Erjia Cui
#' 
#' @name aug4gam
#' @export
aug4gam <- function(x, ...) UseMethod(generic = 'aug4gam')

#' @rdname aug4gam
#' @export
aug4gam.data.frame <- function(x, ...) return(x)


#' @rdname aug4gam
#' @export
aug4gam.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  id <- hc |> 
    vapply(FUN = is.vectorlist, mode = 'numeric', FUN.VALUE = NA)
  if (!any(id)) stop('no vector-list hypercolumn')

  z <- hc[id] |> 
    lapply(FUN = aug4gam.vectorlist) |>
    unlist(recursive = FALSE)
  
  dat <- unclass(x)$df
  # seems that only
  # base::`$<-.data.frame`
  # base::`[[<-.data.frame`
  # respects 'matrix'-columns
  
  for (i in seq_along(z)) {
    dat[[names(z)[i]]] <- z[[i]]
  }
  
  return(dat)
  
}

#' @rdname aug4gam
#' @export
aug4gam.vectorlist <- function(x, ...) {
  
  Y <- x |> 
    do.call(what = rbind)
  
  cnm <- Y |> 
    colnames()
  x. <- if (all(grepl(pattern = '%$', x = cnm))) {
    # returned from ?stats:::quantile.default
    tmp <- cnm |>
      gsub(pattern = '%$', replacement = '') |>
      as.double()
    tmp / 1e2
  } else cnm |> as.double() 
  
  if (!length(x.) || !is.numeric(x.) || anyNA(x.) || is.unsorted(x., strictly = TRUE)) {
    stop('must have names convertible to strictly-increasing numerics')
  }
  
  colnames(Y) <- x.
  
  X <- tcrossprod(rep(1, times = length(x)), x.)
  nc <- length(x.)
  L <- array(1/nc, dim = dim(Y))
  
  list(x = X, y = Y, L = L)
  
}
