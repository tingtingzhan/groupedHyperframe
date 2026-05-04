
# ?hyper.gam::augdata will be replaced by aug4gam


#' @title Augment Hypercolumn(s) for \link[mgcv]{gam}
#' 
#' @param data \link[spatstat.geom]{hyperframe}
#' 
# @param formula one-sided \link[stats]{formula} - this parameter will be removed!!
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' All \link[base]{numeric} \link[base]{vector} hypercolumns will be augmented.
#' 
#' 
#' @author 
#' Tingting Zhan, Erjia Cui
#' 
#' @name aug4gam
#' @export
aug4gam <- function(data, ...) UseMethod(generic = 'aug4gam')

#' @rdname aug4gam
#' @export
aug4gam.data.frame <- function(data, ...) return(data)


.aug4gam_numeric_vectorlist <- \(x, nm) {
  X <- x |> 
    do.call(what = rbind)
  
  cnm <- X |> 
    colnames()
  x. <- if (all(grepl(pattern = '%$', x = cnm))) {
    # returned from ?stats:::quantile.default
    tmp <- cnm |>
      gsub(pattern = '%$', replacement = '') |>
      as.double()
    tmp / 1e2
  } else cnm |> as.double() 
  
  if (!length(x.) || !is.numeric(x.) || anyNA(x.) || is.unsorted(x., strictly = TRUE)) {
    stop(nm |> col_blue(), ' must have names convertible to strictly-increasing numerics')
  }
  
  colnames(X) <- x.
  
  X.x <- tcrossprod(rep(1, times = length(x)), x.)
  nc <- length(x.)
  X.L <- array(1/nc, dim = dim(X))
  
  list(X, X.x, X.L) |>
    setNames(nm = c(
      nm, 
      paste(nm, 'x', sep = '.'),
      paste(nm, 'L', sep = '.')
    ))
  
}



#' @rdname aug4gam
#' @export
aug4gam.hyperframe <- function(data, ...) {
  
  hc <- unclass(data)$hypercolumns
  
  id <- hc |> 
    vapply(FUN = is.vectorlist, mode = 'numeric', FUN.VALUE = NA)
  if (!any(id)) stop('no vector-list hypercolumn')

  z <- mapply(
    FUN = .aug4gam_numeric_vectorlist, 
    x = hc[id], nm = names(hc)[id], 
    SIMPLIFY = FALSE
  ) |>
    unname() |>
    unlist(recursive = FALSE)
  
  dat <- unclass(data)$df
  # seems that only
  # base::`$<-.data.frame`
  # base::`[[<-.data.frame`
  # respects 'matrix'-columns
  
  for (i in seq_along(z)) {
    dat[[names(z)[i]]] <- z[[i]]
  }
  
  return(dat)
  
}
