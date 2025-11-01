
#' @title Black-Solid-Curve in \link[spatstat.explore]{plot.fv}
#' 
#' @description
#' Name and value of the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' i.e., the primary outcome of an \link[spatstat.explore]{fv.object}. 
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of (internal) function `trunc_id.fvlist()`
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
  ret <- x[[key]]
  names(ret) <- x[[.x]]
  return(ret)
}
# read ?spatstat.explore::eval.fv more carefully!!



#' @rdname keyval
#' 
#' @importFrom spatstat.geom anylapply
#' @export keyval.fvlist
#' @export
keyval.fvlist <- function(x, ...) {
  
  #x <- trunc_id.fvlist(x, ...)
  #id <- x |>
  #  attr(which = 'id', exact = TRUE)
  tmp <- x |>
    is.fvlist()
  .y <- tmp |>
    attr(which = '.y', exact = TRUE)
  .x <- tmp |>
    attr(which = '.x', exact = TRUE)
  
  x |> 
    anylapply(FUN = \(i) keyval.fv(i, key = .y, .x = .x)[id])
  
}



#' @rdname keyval
#' @param rmax \link[base]{numeric} scalar, user-specified truncation point \eqn{r_\text{max}}
#' 
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
#' @export keyval.hyperframe
#' @export
keyval.hyperframe <- function(x, rmax, ...) {
  
  if (!any(id <- (unclass(x)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(x)[id]
  
  if (!missing(rmax)) {
    if (!is.numeric(rmax) || length(rmax) != 1L || is.na(rmax) || (rmax <= 0)) stop('illegal user-specified `rmax`')
  } else rmax <- numeric() # cannot use ?base::missing inside ?base::mapply
  
  ret0 <- (as.list.hyperframe(x)[nm]) |>
    mapply(
      FUN = keyval.fvlist, 
      x = _, data.name = nm, 
      MoreArgs = list(rmax = rmax, ...), SIMPLIFY = FALSE
    ) #|> 
    #unlist(recursive = FALSE, use.names = TRUE) # removed!!! beautiful!!!
  
  names(ret0) <- names(ret0) |>
    sprintf(fmt = '%s.y')
  
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), ret0)
  ))
  
}









