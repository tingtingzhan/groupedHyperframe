
#' @title Alternative Interface of \link[spatstat.geom]{nncross.ppp}
#' 
#' @description
#' An alternative interface of function \link[spatstat.geom]{nncross.ppp}.
#' 
#' @param X see **Details**
#' 
#' @param i,j \link[base]{character} or \link[base]{integer} scalars. 
#' See functions \link[spatstat.explore]{Gcross}, etc. for more details
#' 
#' @param ... additional parameters of \link[spatstat.geom]{nncross.ppp}
#' 
#' @details
#' Function [.nncross()] creates an interface similar to 
#' functions \link[spatstat.explore]{Gcross}, etc.,
#' which takes an \link[spatstat.geom]{is.multitype} \link[spatstat.geom]{ppp.object}
#' and two mark values `i` and `j`, 
#' then calls the workhorse function 
#' \link[spatstat.geom]{nncross.ppp} with parameter `what = 'dist'`.
#' If mark values `i` and `j` does not exist in the \link[spatstat.geom]{ppp.object},
#' a `NULL` value will be returned.
#' 
#' @returns
#' Function [.nncross()] returns
#' a \link[base]{numeric} \link[base]{vector} 
#' if `i` and `j` are valid mark values of \link[spatstat.geom]{ppp.object} `X`;
#' otherwise returns a `NULL` value.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' 
#' (xs = split.ppp(amacrine))
#' (a1 = nncross(X = xs$off, Y = xs$on, what = 'dist'))
#' a2 = .nncross(amacrine, i = 'off', j = 'on')
#' a3 = .nncross(amacrine, i = 1L, j = 2L)
#' stopifnot(identical(a1, a2), identical(a1, a3))
#' 
#' .nncross(amacrine, i = 'a', j = 'b') # exception handling
#' @keywords internal
#' @importFrom spatstat.geom nncross.ppp is.marked.ppp is.multitype.ppp marks.ppp split.ppp
#' @export
.nncross <- function(X, i, j, ...) {
  
  # see ?spatstat.explore::Gcross carefully
  if (!is.marked.ppp(X, dfok = FALSE)) stop(paste('point pattern has no', sQuote('marks')))
  if (!is.multitype.ppp(X)) stop('nncross_ requires multitype') 
  
  x_ <- split.ppp(X) # using default `f`
  
  # end user named non-existing levels?
  if (!length(i_ <- x_[[i]])) return(invisible())
  if (!length(j_ <- x_[[j]])) return(invisible())
  
  return(nncross.ppp(X = i_, Y = j_, what = 'dist', ...))
  
}


