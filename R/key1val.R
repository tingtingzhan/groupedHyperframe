
#' @title Black-Solid-Curve in \link[spatstat.explore]{plot.fv}
#' 
#' @description
#' Name and value of the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' i.e., the primary outcome of an \link[spatstat.explore]{fv.object}. 
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' Function [keyval.fv()] finds the value of the (primary) outcome
#' of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Function [keyval.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' 
#' @keywords internal
#' @name keyval
#' @export
keyval <- function(x, ...) UseMethod(generic = 'keyval')

#' @rdname keyval
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @importFrom spatstat.explore fvnames
#' @export keyval.fv
#' @export
keyval.fv <- function(
    x, 
    key = fvnames(x, a = '.y'),
    .x = fvnames(x, a = '.x'),
    ...
) {
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  ret <- x[[key]] # no need to drop additional attributes (I think..)
  names(ret) <- x[[.x]] # additional attributes (since \pkg{spatstat.explore} v3.5.3.9) kept
  return(ret)
}
# read ?spatstat.explore::eval.fv more carefully!!



#' @rdname keyval
#' 
#' @importFrom spatstat.geom anylapply
#' @export keyval.fvlist
#' @export
keyval.fvlist <- function(x, ...) {
  
  tmp <- x |>
    is.fvlist()
  .y <- tmp |>
    attr(which = '.y', exact = TRUE)
  .x <- tmp |>
    attr(which = '.x', exact = TRUE)
  
  x |> 
    anylapply(FUN = \(i) keyval.fv(i, key = .y, .x = .x))
  
}



#' @rdname keyval
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
#' @export keyval.hyperframe
#' @export
keyval.hyperframe <- function(x, ...) {
  
  if (!any(id <- (unclass(x)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(x)[id]
  
  ret0 <- (as.list.hyperframe(x)[nm]) |>
    lapply(FUN = keyval.fvlist, ...)
  
  names(ret0) <- names(ret0) |>
    sprintf(fmt = '%s.y')
  
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), ret0)
  ))
  
}









