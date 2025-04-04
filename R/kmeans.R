

#' @title Extension of \link[stats]{kmeans}
#' 
#' @param x see **Usage**
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param ... additional parameters of function \link[stats]{kmeans}
#' 
#' @returns 
#' Function [.kmeans()] returns a \link[stats]{kmeans} object.
#' 
#' @keywords internal
#' @name kmeans
#' @export
.kmeans <- function(x, formula, ...) UseMethod(generic = '.kmeans')


#' @rdname kmeans
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' shapley |> plot()
#' km = shapley |> .kmeans(formula = ~ x + y + Mag, centers = 3L)
#' km |> class()
#' @importFrom spatstat.geom marks.ppp markformat.ppp
#' @export
.kmeans.ppp <- function(x, formula, ...) {
  
  if (!is.call(formula) || formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be one-sided formula')
  
  v <- formula[[2L]] |> all.vars()
  
  if (markformat.ppp(x) != 'dataframe') stop('markformat must be dataframe')
  
  m <- x |> 
    marks.ppp(drop = FALSE)
  
  num_ <- m |>
    vapply(FUN = is.numeric, FUN.VALUE = NA) |>
    which() |>
    names()
  
  v_m <- v |>
    setdiff(y = c('x', 'y'))
  
  if (!all(v_m %in% num_)) stop('some terms in formula are not numeric mark')
  
  tmp <- cbind(
    x = if ('x' %in% v) x$x, # else NULL
    y = if ('y' %in% v) x$y,
    (m[v_m]) |> # !length(v_m) compatible
      as.list.data.frame() |>
      do.call(what = cbind)
  )
  
  tmp |> kmeans(...)
  
}



