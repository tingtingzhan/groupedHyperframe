

#' @title check_fvlist
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @details
#' Function [check_fvlist()] checks that
#' \itemize{
#' \item {if \eqn{x}-axis of all \link[spatstat.explore]{fv.object}s are all same}
#' \item {`attr(,'fname')` of all \link[spatstat.explore]{fv.object}s are all same}
#' }
#' 
#' Note that
#' \itemize{
#' \item {function [key1.fv()] returns of all \link[spatstat.explore]{fv.object}s are not required to be all same}
#' }
#' 
#' @returns 
#' Function [check_fvlist()] does not have a returned value.
#' 
#' @keywords internal
#' @export
check_fvlist <- function(X) {
  
  r. <- X |>
    lapply(FUN = `[[`, 1L) |>
    duplicated.default()
  if (!all(r.[-1L])) stop('x-axis of all fv.objects are not the same')
  
  fname. <- X |>
    lapply(FUN = attr, which = 'fname', exact = TRUE) |>
    duplicated.default()
  if (!all(fname.[-1L])) stop('fname of all fv.objects are not the same')
  
  # I do not require [key1.fv] to be the same!!!!
  
}


