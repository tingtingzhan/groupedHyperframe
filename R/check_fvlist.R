

#' @title check_fvlist
#' 
#' @description
#' A helper function to check the validity of a \link[stats]{listof} \link[spatstat.explore]{fv.object}s.
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @param data.name \link[base]{character} scalar, name of `X`, for console message output
#' 
#' @details
#' Function [check_fvlist()] checks that whether all \link[spatstat.explore]{fv.object}s
#' in the input has the same
#' \itemize{
#' \item {\eqn{x}-axis, or the \eqn{r}-values}
#' \item {`attr(,'fname')`, see explanation of this \link[base]{attributes} in function \link[spatstat.explore]{fv}}
#' \item {[key1.fv()] returns}
#' }
#' 
#' !!! 'Legal' not documented yet!!!
#' 
#' @returns 
#' Function [check_fvlist()] returns an \link[base]{invisible} \link[base]{list} with elements
#' \describe{
#' \item{`$r`}{\link[base]{numeric} \link[base]{vector}, the \eqn{r}-values}
#' \item{`$key1`}{\link[base]{character} scalar, the return of function [key1.fv()]}
#' \item{`$rmax`}{\link[base]{numeric} scalar or \link[base]{vector}, the \link[base]{unique} values of the legal \eqn{r_\text{max}} of each \link[spatstat.explore]{fv.object}, if any one of them is less than the user-specified \eqn{r_\text{max}}}
#' }
#' 
#' @keywords internal
#' @export
check_fvlist <- function(X, data.name = deparse1(substitute(X))) {
  
  is.fv <- X |>
    vapply(FUN = inherits, what = 'fv', FUN.VALUE = NA)
  
  if (!any(is.fv)) return(invisible())
  if (!all(is.fv)) stop('error!')
  
  r. <- X |>
    lapply(FUN = `[[`, 1L)
    
  if (!all(duplicated.default(r.)[-1L])) stop('x-axis of all fv.objects are not the same')
  
  fname. <- X |>
    lapply(FUN = attr, which = 'fname', exact = TRUE) |>
    duplicated.default()
  if (!all(fname.[-1L])) stop('fname of all fv.objects are not the same')
  
  key1. <- X |> 
    vapply(FUN = key1.fv, FUN.VALUE = '')
  if (!all(duplicated.default(key1.)[-1L])) stop('all fv-object must have same key1')
  
  r <- r.[[1L]]
  nr <- length(r)
  
  ret <- list(
    r = r,
    key1 = key1.[[1L]]
  )
  
  id <- X |> 
    vapply(FUN = \(x) {
      x[[key1.[1L]]] |> # do NOT use [key1val.fv()] here, slow!
        lastLegal()
    }, FUN.VALUE = NA_integer_)
  
  if (any(id < nr)) {
    id0 <- id[id != nr]
    tb <- id0 |> table()
    uid <- id0 |> unique.default() |> sort.int()
    loc <- uid |>
      vapply(FUN = \(u) {
        which(id == u) |>
          paste0('L', collapse = ', ') |>
          col_magenta() |> style_bold()
      }, FUN.VALUE = '')
    paste0(
      'Legal ', 
      'rmax' |> col_magenta() |> style_bold(),
      '(', 
      data.name |> col_blue() |> style_bold(), 
      sprintf(fmt = '), smaller than user input of rmax = %.1f, are\n', max(r)), 
      sprintf(fmt = '%d\u2a2f ', tb) |> col_br_magenta() |> style_bold() |>
        paste0('rmax=', r[uid], ' at location ', loc, collapse = '\n')
    ) |>
      message()
    ret[['rmax']] <- min(r[uid]) # minimum legal rmax of 'fvlist'
  } else ret[['rmax']] <- max(r)
  
  return(invisible(ret))
  
}




#' @title Last Legal Index of [key1val.fv()]
#' 
#' @param v \link[base]{numeric} \link[base]{vector}, return of function [key1val.fv()] 
#' 
#' @examples
#' c(1, 1, 1) |> lastLegal()
#' c(1, 1, 1, NaN) |> lastLegal()
#' c(1, 1, 1, NaN, 1, 0, Inf) |> lastLegal()
#' # all return `3`
#' @keywords internal
#' @export
lastLegal <- \(v) {
  
  vok <- is.finite(v) & (abs(v) > .Machine$double.eps) # not 0, not NaN, not Inf
  if (all(vok)) return(length(vok)) # faster than [.diff()]
  
  .diff <- \(x) {
    x[-1L] - x[-length(x)]
  } # faster than ?base::diff.default
  
  z <- vok |> 
    which() |>
    .diff()
  if (all(z == 1L)) return(length(z) + 1L)
  return(min(which(z != 1L))) # smart!
  
}

