
#' @title Black-Solid-Curve in \link[spatstat.explore]{plot.fv}
#' 
#' @description
#' Name and value of the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' i.e., the primary outcome of an \link[spatstat.explore]{fv.object}. 
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key \link[base]{character} scalar, default value is `spatstat.explore::fvnames(x, a = '.y')`,
#' to speed up batch processes.
#' 
#' @details
#' Function [keyval.fv()] finds the value of the (primary) outcome
#' of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Function [keyval.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames
#' @export
keyval.fv <- function(x, key = fvnames(x, a = '.y')) {
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  ret <- x[[key]]
  names(ret) <- x[[.x]]
  return(ret)
}
# read ?spatstat.explore::eval.fv more carefully!!





