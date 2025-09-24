
#' @title Operations on \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' Create
#' \link[spatstat.explore]{fv.object}s
#' or 
#' distances
#' from a \link[spatstat.geom]{ppp.object}.
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param fn a distance \link[base]{function}, 
#' or a \link[base]{function} that returns an \link[spatstat.explore]{fv.object}, 
#' see **Details**
#' 
#' @param ... additional parameters of function `fn`
#' 
#' @returns 
#' Function [ppp2fv()] returns a \link[stats]{listof} 
#' \link[spatstat.explore]{fv.object}s.
#' 
#' Function [ppp2dist()] returns a \link[stats]{listof} 
#' \link[base]{double} \link[base]{vector}s.
#' 
#' @keywords internal
#' @name ppp2.
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp
#' @export
ppp2dist <- function(x, fn, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    #stop('unstacked `x` must be named, see ?mark_names')
    names(x.) <- 'm'
  }
  
  mtp <- vapply(x., FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  fn_mtp_ <- c(
    nncross = '.nncross'
  )
  fn_mtp <- lapply(fn_mtp_, FUN = get)
  id_mtp <- vapply(fn_mtp, FUN = identical, x = fn, FUN.VALUE = NA)
  
  if (!any(id_mtp)) stop('fn not supported')

  ret <- lapply(x.[mtp], FUN = fn, ...)
  names(ret) <- paste(names(ret), names(fn_mtp_)[id_mtp], sep = '.')
  
  return(ret[lengths(ret) > 0L])
  
}





#' @rdname ppp2.
#' @importFrom spatstat.explore Emark Vmark markcorr markvario
#' @importFrom spatstat.explore Kmark
#' @importFrom spatstat.explore Gcross Jcross Kcross Lcross markconnect
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
#' @export
ppp2fv <- function(x, fn, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- x. |> 
    vapply(FUN = is.multitype.ppp, FUN.VALUE = NA)
  num <- x |>
    is.numeric.ppp()
  
  fn_num <- list(
    Emark, Vmark, markcorr, markvario, # using workhorse ?spatstat.explore::markcorr
    Kmark
  ) |>
    vapply(FUN = identical, x = fn, FUN.VALUE = NA) |> 
    any()
  
  fn_mtp <- list(
    Gcross, Jcross, Kcross, Lcross, markconnect
  ) |>
    vapply(FUN = identical, x = fn, FUN.VALUE = NA) |>
    any()
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  if (!xor(fn_num, fn_mtp)) stop('unknown fv-function to tzh?')
  
  x.. <- if (fn_num) {
    if (!any(num)) return(invisible())
    x.[num]
  } else if (fn_mtp) {
    if (!any(mtp)) return(invisible())
    x.[mtp]
  }
  
  ret <- x.. |> 
    # lapply(FUN = fn, ...) # um..
    anylapply(FUN = fn, ...) # after 2025-09-24
  
  # restore names of `fv`-hypercolumns from the result
  # attr(,'fname') is determined by `fn`
  fname1 <- attr(ret[[1L]], which = 'fname', exact = TRUE)[1L]
  names(ret) <- paste(names(ret), fname1, sep = '.')
  
  return(ret)
  
}





