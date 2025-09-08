
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
#' @details
#' First, the input \link[spatstat.geom]{ppp.object} is \link[spatstat.geom]{unstack.ppp}-ed.
#' 
#' Each of the \link[base]{numeric} \link[spatstat.geom]{marks} could be analyzed by   
#' following functions that return an \link[spatstat.explore]{fv.object},
#' \itemize{
#' \item {\link[spatstat.explore]{Emark}}
#' \item {\link[spatstat.explore]{Vmark}}
#' \item {\link[spatstat.explore]{markcorr}} 
#' \item {\link[spatstat.explore]{markvario}}
#' }
#' If one of the functions above are provided 
#' but there is no \link[base]{numeric} \link[spatstat.geom]{marks} in the input,
#' a `NULL` value will be returned.
#' 
#' Each of the \link[spatstat.geom]{marks} that \link[spatstat.geom]{is.multitype}
#' could be analyzed 
#' by following functions that return an \link[spatstat.explore]{fv.object},
#' \itemize{
#' \item {\link[spatstat.explore]{Gcross}}
#' \item {\link[spatstat.explore]{Jcross}}
#' \item {\link[spatstat.explore]{Kcross}}
#' \item {\link[spatstat.explore]{Lcross}}
#' \item {\link[spatstat.explore]{markconnect}}
#' }
#' or by following functions that return a distance,
#' \itemize{
#' \item {[.nncross()]}
#' }
#' If one of the functions above are provided 
#' but there is no \link[spatstat.geom]{marks} \link[spatstat.geom]{is.multitype} in the input,
#' a `NULL` value will be returned.
#' 
#' @returns 
#' Function [ppp2fv()] returns a \link[stats]{listof} 
#' \link[spatstat.explore]{fv.object}s, 
#' one per each eligible \link[spatstat.geom]{marks}.
#' 
#' Function [ppp2dist()] returns a \link[stats]{listof} 
#' \link[base]{double} \link[base]{vector}s,
#' one per each eligible \link[spatstat.geom]{marks}.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.explore)
#' 
#' ppp2fv(betacells, fn = Emark) # applicable to numeric mark
#' ppp2fv(betacells, fn = Kmark) # applicable to numeric mark
#' ppp2fv(betacells, fn = Gcross, i = 'off', j = 'on') # applicable to multitype mark
#' 
#' ppp2dist(betacells, fn = .nncross, i = 'off', j = 'on')
#' ppp2dist(gorillas, fn = .nncross, i = 'major', j = 'minor')
#' ppp2dist(gorillas, fn = .nncross, i = 'rainy', j = 'dry')
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
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp
#' @export
ppp2fv <- function(x, fn, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- x. |> 
    vapply(FUN = is.multitype.ppp, FUN.VALUE = NA)
  num <- x. |>
    vapply(FUN = \(i) is.numeric(i$marks), FUN.VALUE = NA)
  # stopifnot(is.double(POSIXct), !is.numeric(POSIXct))
  
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
    lapply(FUN = fn, ...)
  
  # restore names of `fv`-hypercolumns from the result
  # attr(,'fname') is determined by `fn`
  fname1 <- attr(ret[[1L]], which = 'fname', exact = TRUE)[1L]
  names(ret) <- paste(names(ret), fname1, sep = '.')
  
  return(ret)
  
}





