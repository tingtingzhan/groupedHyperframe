

#' @title Functions for \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @keywords internal
#' @name fvlist


#' @rdname fvlist
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
#' @export
check_fvlist <- function(X) {
  
  x <- lapply(X, FUN = `[[`, 1L)
  if (!all(duplicated.default(x)[-1L])) stop('x-axis of all fv.objects are not the same')
  
  fname <- lapply(X, FUN = attr, which = 'fname', exact = TRUE)
  if (!all(duplicated.default(fname)[-1L])) stop('fname of all fv.objects are not the same')
  
  # I do not require [key1.fv] to be the same!!!!
  
}


key1.fvlist <- function(X) {
  .Defunct(msg = 'currently not using')
  vapply(X, FUN = key1.fv, FUN.VALUE = NA_character_)
}





#' @rdname fvlist
#' @param check \link[base]{logical} scalar, an option to suppress 
#' function [check_fvlist()] in a batch process.
#' Default `TRUE`
#' 
#' @details
#' Function [key1val.fvlist()] gathers the primary outcome
#' of the \link[spatstat.explore]{fv.object}s.
#' 
#' @export
key1val.fvlist <- function(X, check = TRUE) {
  if (check) check_fvlist(X)
  X |> lapply(FUN = key1val.fv)
}





#' @rdname fvlist
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
#' CRAN requires `mc.cores <= 2L` in examples.
#' 
#' @details 
#' Function [cumtrapz.fvlist()] is a batch process of function [cumtrapz.fv()].
#' 
#' @returns
#' Function [cumtrapz.fvlist()] returns a \link[stats]{listof} \link[base]{double} \link[base]{vector}s.
#' 
#' @importFrom parallel mclapply detectCores
#' @export
cumtrapz.fvlist <- function(
    X, 
    check = TRUE, 
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores())
) {
  if (check) check_fvlist(X)
  X |> mclapply(mc.cores = mc.cores, FUN = cumtrapz.fv)
}





# @title Area Under Curve of `'fvlist'` Object
# 
# @description
# Defunct!  Do this manually
# 
# @param X an `'fvlist'` object
# 
# @param ... additional parameters, currently not in use
# 
# @returns 
# Function [trapz.fvlist()] returns a \link[base]{numeric} \link[base]{vector}.
# 
# @keywords internal
# @export trapz.fvlist
# @export
#trapz.fvlist <- function(X, ...) vapply(X, FUN = trapz.fv, ..., FUN.VALUE = NA_real_)


# do NOT define `[.fvlist`
# tzh does not want to override `[` for 'list'
# @export
#subset.fvlist <- function(x, subset, ...) {
#}



