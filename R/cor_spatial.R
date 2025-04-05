

#' @title Pairwise Tjostheim's Coefficient
#' 
#' @param x see **Usage**
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [pairwise_cor_spatial()] returns a \link[stats]{dist} object.
#' 
#' @keywords internal
#' @name pairwise_cor_spatial
#' @export
pairwise_cor_spatial <- function(x, ...) UseMethod(generic = 'cor_spatial')

#' @rdname pairwise_cor_spatial
#' @examples
#' data(finpines, package = 'spatstat.data')
#' finpines |> pairwise_cor_spatial.ppp(formula = ~ diameter + height)
#'
#' @importFrom SpatialPack cor.spatial
#' @export pairwise_cor_spatial.ppp
#' @export
pairwise_cor_spatial.ppp <- function(x, formula, ...) {
  
  if (!is.call(formula) || formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be one-sided formula')
  
  v <- formula[[2L]] |> all.vars()
  nv <- v |> length()
  if (nv <= 1L) stop('must specify 2+ numeric marks')
  
  m <- x |> marks()
  if (!all(v %in% names(m))) stop('unknown marker specified in `formula`')  
  m[v] |> vapply(FUN = is.numeric, FUN.VALUE = NA) |> all() |> stopifnot() # not compute intensive anyway
  
  #co <- x |> spatstat.geom::coords.ppp() |> as.matrix.data.frame() # nah..
  co <- cbind(x$x, x$y)

    ret <- numeric()
  for (i in seq_len(nv-1L)) {
    for (j in (i+1):nv) {
      ret <- c(ret, cor.spatial(x = m[[v[i]]], y = m[[v[j]]], coords = co))
    }
  }
  attr(ret, which = 'Size') <- nv
  attr(ret, which = 'Labels') <- v
  attr(ret, which = 'Diag') <- FALSE
  class(ret) <- c('pairwise_cor_spatial', 'dist') # ?stats::dist
  return(ret)
  
}

