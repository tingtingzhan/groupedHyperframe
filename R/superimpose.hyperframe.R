

#' @title superimpose.hyperframe
#' 
#' @param ... one or more \link[spatstat.geom]{hyperframe}s
#' 
#' @returns
#' Function [superimpose.hyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom superimpose dim.hyperframe
#' @method superimpose hyperframe
#' @export superimpose.hyperframe
#' @export
superimpose.hyperframe <- function(...) {
  
  dots <- list(...)
  if (!length(dots)) return(invisible())
  if (length(dots) == 1L) return(dots[[1L]]) 
  
  z <- dots |>
    vapply(FUN = inherits, what = 'hyperframe', FUN.VALUE = NA) |>
    all()
  if (!z) stop('all input must be hyperframe')
  
  z <- dots |>
    lapply(FUN = dim.hyperframe)
  if (!all(duplicated.default(z)[-1L])) stop('all input must have same dimensions')
  
  d0 <- dots |>
    lapply(FUN = unclass)
  
  z <- d0 |> 
    lapply(FUN = `[[`, 'df')
  if (!all(duplicated(z)[-1L])) stop('all input must have same column(s)')
  
  hc <- d0 |>
    lapply(FUN = `[[`, 'hypercolumns')
  z <- hc |>
    lapply(FUN = names)
  if (!all(duplicated(z)[-1L])) stop('all input must have same names of hypercolumn(s)')
  
  hc_class <- hc |> 
    lapply(FUN = \(i) {
      i |> lapply(FUN = class)
    })
  if (!all(duplicated(hc_class)[-1L])) stop('all input must have same classes of hypercolumn(s)')
  
  hc_out <- hc |> 
    c(list(
      .class = hc_class[[1L]],
      FUN = \(..., .class) {
        out <- mapply(FUN = superimpose, ..., SIMPLIFY = FALSE)
        # use S3 generic ?spatstat.geom::superimpose !! super smart!!
        # tzh does not know what ?spatstat.geom::superimpose.ppp does - see if (FALSE) at the bottom of this file
        class(out) <- .class
        return(out)
      },
      SIMPLIFY = FALSE
    )) |> 
    do.call(what = mapply, args = _)

  # identical(hc_out, unclass(fluM)$hypercolumns) # no, tzh does not care why :)
  # stopifnot(identical(hc_out$pattern, fluM$pattern)) # :))
  
  ret <- d0[[1L]]
  # tzh does not understand `$hyperatoms` yet..
  ret$hypercolumns <- hc_out
  class(ret) <- class(dots[[1L]])
  return(ret)

}




#' @title superimpose.groupedHyperframe
#' 
#' @param ... one or more [groupedHyperframe]s
#' 
#' @returns
#' Function [superimpose.groupedHyperframe()] returns a [groupedHyperframe].
#' 
#' @keywords internal
#' @importFrom spatstat.geom superimpose
#' @export superimpose.groupedHyperframe
#' @export
superimpose.groupedHyperframe <- function(...) {
  
  dots <- list(...)
  if (!length(dots)) return(invisible())
  if (length(dots) == 1L) return(dots[[1L]]) 
  
  z <- dots |>
    vapply(FUN = inherits, what = 'groupedHyperframe', FUN.VALUE = NA) |>
    all()
  if (!z) stop('all input must be groupedHyperframe')
  
  z <- dots |> 
    lapply(FUN = attr, which = 'group')
  if (!all(duplicated(z)[-1L])) stop('all input must have same grouping structure')
  
  # NextMethod(generic = 'superimpose') # do NOT activate [superimpose.hyperframe()]; why??
  
  return(superimpose.hyperframe(...))
  
}
  
  
  

if (FALSE) {
  
  hc[[1L]]$pattern |> sapply(FUN = spatstat.geom::npoints)
  hc[[2L]]$pattern |> sapply(FUN = spatstat.geom::npoints)
  
  hc[[1L]]$pattern$`wt M2-M1 13`$marks # unused level dropped correctly
  
  # ?spatstat.geom::superimpose.ppplist **not** what we need!
  # already documented in vignette :)

  x1 = mapply(
    FUN = spatstat.geom::superimpose.ppp,
    hc[[1L]]$pattern, hc[[2L]]$pattern, 
    SIMPLIFY = FALSE
  ) # correct!!
    
  x2 = .mapply(
    FUN = spatstat.geom::superimpose.ppp,
    dots = list(hc[[1L]]$pattern, hc[[2L]]$pattern),
    MoreArgs = NULL
  ) # does not work!!!  names-attr dropped by .mapply !!!
  
  class(x1) = class(x2) = class(fluM$pattern)
  
  stopifnot(identical(fluM$pattern, x1))
  identical(fluM$pattern, x2) # no!!!
  
}

