

#' @title .slice
#' 
#' @param x a \link[stats]{listof} \link[base]{vector}s of same \link[base]{length}
#' 
#' @param j \link[base]{integer} or \link[base]{character} scalar or \link[base]{vector}
#' 
#' @returns
#' Function [.slice()] returns a \link[base]{vector}
#' 
#' @keywords internal
#' @export
.slice <- function(x, j) {
  
  if (!inherits(x, what = 'listof')) stop('`x` must be `listof`')
  if (!all(vapply(x, FUN = is.vector, FUN.VALUE = NA))) stop('each of `x` must be `vector`')
  
  nx <- lengths(x, use.names = FALSE)
  if (!all(duplicated(nx)[-1L])) stop('each of `x` must be of same `length`')
  
  nms <- lapply(x, FUN = names)
  if (any(lengths(nms) == 0L)) stop('each of `x` must have names')
  if (!all(duplicated(nms)[-1L])) stop('each of `x` must have same names')
  nm <- nms[[1L]]
  
  tmp <- x |> do.call(what = rbind)
  tmp[, j, drop = TRUE]
  
}