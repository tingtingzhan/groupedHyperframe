
#' @title Default \eqn{r_\text{max}} of Function \link[spatstat.explore]{markcorr}
#' 
#' @param X a \link[spatstat.geom]{ppp.object} with one \link[base]{numeric} \link[spatstat.geom]{marks}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' 
#' Code snippet stolen from 
#' function \link[spatstat.explore]{markcorr} (`fun = 'K'`)
#' and making use of undocumented functions
#' \link[spatstat.explore]{rmax.rule} and \link[spatstat.geom]{handle.r.b.args}
#' 
#' @examples
#' library(spatstat.data)
#' # ppp
#' spruces |> rmax_(fun = 'K') # rectangle window
#' urkiola |> rmax_(fun = 'K') # polygonal boundary 
#' swedishpines |> rmax_(fun = 'K') # not-marked, exception handling
#' # hyperframe
#' flu |> rmax_(fun = 'K') # rectangle window
#' cetaceans |> rmax_(fun = 'K') # polygonal boundary, marked and unmarked
#' pyramidal |> rmax_(fun = 'K') # not-marked
#' @keywords internal
#' @name rmax
#' @importFrom spatstat.explore rmax.rule
#' @importFrom spatstat.geom area is.marked is.ppp npoints handle.r.b.args
#' @export
rmax_ <- function(X, fun, ...) UseMethod(generic = 'rmax_')

#' @rdname rmax
#' @export rmax_.ppp
#' @export
rmax_.ppp <- function(X, fun = 'K', ...) {
  
  if (!is.ppp(X)) stop('input `X` must be ppp.')
  if (!is.marked(X)) return(NA_real_) # exception handling
    
  npts <- npoints(X)
  W <- X$window
  rmaxdefault <- rmax.rule(fun = fun, W = W, lambda = npts/area(W))
  breaks <- handle.r.b.args(window = W, rmaxdefault = rmaxdefault)
  r <- breaks$r
  rmax <- breaks$max
  # alim <- c(0, min(rmax, rmaxdefault)) # to remind tzh-self
  return(min(rmax, rmaxdefault))
  
}


#' @rdname rmax
#' @export rmax_.ppplist
#' @export
rmax_.ppplist <- function(X, fun = 'K', ...) {
  X |>
    vapply(FUN = rmax_.ppp, fun = fun, FUN.VALUE = NA_real_)
}


#' @rdname rmax
#' @export rmax_.hyperframe
#' @export
rmax_.hyperframe <- function(X, fun = 'K', ...) {
  
  # may handle multiple ppp-hypercolumns!!!
  
  hc <- unclass(X)$hypercolumns
  
  hc_ppp <- hc |>
    vapply(FUN = \(x) {
      x |>
        vapply(FUN = is.ppp, FUN.VALUE = NA) |>
        all()
    }, FUN.VALUE = NA) |>
    which() 
  
  if (!length(hc_ppp)) return(invisible()) # exception handling
  
  ret <- hc[hc_ppp] |>
    lapply(FUN = rmax_.ppplist, fun = fun)
  
  cat('\n')
  mapply(FUN = \(r, nm) {
    tb <- table(r)
    paste(
      'Recommended', 
      'rmax' |> col_red() |> style_bold(),
      'for', 
      nm |> col_blue() |> style_bold(),
      'are',
      sprintf(fmt = '%d\u2a2f ', tb) |> col_br_magenta() |> style_bold() |>
        paste0(names(tb), collapse = '; ')
    ) |>
      message()
  }, r = ret, nm = names(ret))
  
  return(invisible(ret))
  
}




