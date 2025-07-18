
#' @title Default \eqn{r_\text{max}} of Various Functions in Package \CRANpkg{spatstat.explore}
#' 
#' @param X a \link[spatstat.geom]{ppp.object} with one \link[base]{numeric} or multi-type \link[spatstat.geom]{marks}
#' 
#' @param fun \link[base]{character} scalar, see (the un-documented) function \link[spatstat.explore]{rmax.rule}
#' 
#' @param i,j \link[base]{character} scalars, see functions 
#' \link[spatstat.explore]{Gcross}, 
#' \link[spatstat.explore]{Kcross}, 
#' \link[spatstat.explore]{Jcross}, etc.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' # ppp, fun = 'K'
#' spatstat.data::spruces |> rmax_(fun = 'K') # rectangle window
#' spatstat.data::urkiola |> rmax_(fun = 'K') # polygonal boundary 
#' spatstat.data::swedishpines |> rmax_(fun = 'K') # not-marked, exception handling
#' 
#' # hyperframe, fun = 'K'
#' spatstat.data::flu |> rmax_(fun = 'K') # rectangle window
#' spatstat.data::cetaceans |> rmax_(fun = 'K') # polygonal boundary, marked and unmarked
#' spatstat.data::pyramidal |> rmax_(fun = 'K') # not-marked
#' 
#' # hyperframe, fun = 'G'
#' spatstat.data::flu |> rmax_(fun = 'G')
#' flu0 = spatstat.data::flu |> subset(subset = (stain == 'M2-M1'))
#' flu0 |> rmax_(fun = 'G', i = 'M1', j = 'M2')
#' flu0 |> rmax_(fun = 'G', i = 'M2', j = 'M1')
#' @keywords internal
#' @name rmax
#' @export
rmax_ <- function(X, ...) UseMethod(generic = 'rmax_')

# fv.object |> attr(, 'alim') depends on user-input `r`!!
# use tzh's [rmax_] to get default `r` !!


#' @rdname rmax
#' @importFrom spatstat.explore rmax.rule
#' @importFrom spatstat.geom area intensity marks.ppp is.marked.ppp is.ppp npoints.ppp unstack.ppp is.multitype.ppp handle.r.b.args ppsubset
#' @export rmax_.ppp
#' @export
rmax_.ppp <- function(X, fun, i, j, ...) {
  
  # S3 call to [area] is probably [area.owin]
  # S3 call to [intensity] is probably [intensity.ppp]
  
  if (!is.ppp(X)) stop('input `X` must be ppp.')
  if (!is.marked.ppp(X)) return(NA_real_) # exception handling
  
  npts <- npoints.ppp(X)
  W <- X$window
  
  has_i <- !missing(i)
  has_j <- !missing(j)
  
  if (has_i && has_j) {
    
    # fun = 'G'; ?spatstat.explore::Gmulti, i.e., ?spatstat.explore::Gcross when i!=j
    # fun = 'K'; ?spatstat.explore::Kmulti, i.e., ?spatstat.explore::Kcross when i!=j (naively use `area(W)`)
    # fun = 'J'; ?spatstat.explore::Jmulti, i.e., ?spatstat.explore::Jcross when i!=j
    # original: marx <- marks(X, dfok = FALSE) # need a little twick..
    xs <- unstack.ppp(X)
    id_mtt <- vapply(xs, FUN = is.multitype.ppp, FUN.VALUE = NA)
    if (!any(id_mtt)) stop('no multitype marks')
    marx_ <- xs[id_mtt] |> 
      lapply(FUN = marks.ppp, dfok = FALSE)
    id <- marx_ |> 
      vapply(FUN = `%in%`, x = j, FUN.VALUE = NA)
    if (sum(id) != 1L) stop('not programed, yet')
    J <- ppsubset(X = X, I = (marx_[[id]] == j), Iname = 'J')
    rmaxdefault <- rmax.rule(
      fun = fun, 
      W = W, 
      lambda = switch(fun, K =, G = sum(J)/area(W), J = intensity(X[J]))
    )

  } else if (!has_i && !has_j) {
    
    # fun = 'K'; ?spatstat.explore::markcorr 
    # fun = 'G'; ?spatstat.explore::Gest, i.e., ?spatstat.explore::Gcross when i==j
    # fun = 'K'; ?spatstat.explore::Kest, i.e., ?spatstat.explore::Kcross when i==j
    # fun = 'J'; ?spatstat.explore::Jest, i.e., ?spatstat.explore::Jcross when i==j
    rmaxdefault <- rmax.rule(
      fun = fun, 
      W = W, 
      lambda = switch(fun, K =, G = npts/area(W), J = intensity(X))
    )
    
  } else stop('wont happen')
  
  breaks <- handle.r.b.args(window = W, rmaxdefault = rmaxdefault)
  # rmax <- breaks$max
  # alim <- c(0, min(rmax, rmaxdefault)) # to remind tzh-self
  return(min(breaks$max, rmaxdefault))
    
}


#' @rdname rmax
#' @export rmax_.ppplist
#' @export
rmax_.ppplist <- function(X, ...) {
  X |>
    vapply(FUN = rmax_.ppp, ..., FUN.VALUE = NA_real_)
}


#' @rdname rmax
#' @export rmax_.hyperframe
#' @export
rmax_.hyperframe <- function(X, ...) {
  
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
    lapply(FUN = rmax_.ppplist, ...)
  
  cat('\n')
  mapply(FUN = \(r, nm) {
    tb <- table(r)
    r0 <- r |> unique.default() |> sort.int()
    prt <- if (length(r0) == length(r)) {
      r0 |> 
        range.default() |>
        sprintf(fmt = '%.2f') |>
        paste(collapse = ' ~ ')
    } else {
      sprintf(fmt = '%d\u2a2f ', tb) |> col_br_magenta() |> style_bold() |>
        paste0(sprintf(fmt = '%.2f', r0), collapse = '; ')
    }
    paste(
      'Default', 
      'rmax' |> col_red() |> style_bold(),
      'for', 
      nm |> col_blue() |> style_bold(),
      'are',
      prt
    ) |> message()
  }, r = ret, nm = names(ret))
  
  return(invisible(ret))
  
}




