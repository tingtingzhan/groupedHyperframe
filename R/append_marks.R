

#' @title Append to Existing \link[spatstat.geom]{marks}
#' 
#' @description
#' Append an additional `mark` to existing \link[spatstat.geom]{marks}.
#' 
#' @param x currently only \link[spatstat.geom]{ppp.object} is supported
#' 
#' @param value a \link[base]{factor} or \link[base]{numeric} \link[base]{vector}
#' 
#' @returns 
#' The `S3` method dispatch [`append_marks<-.ppp`] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @examples
#' library(spatstat.geom)
#' 
#' # no existing marks
#' data(vesicles, package = 'spatstat.data')
#' vesicles
#' set.seed(12); append_marks(vesicles) = rlnorm(n = npoints(vesicles))
#' vesicles
#' 
#' # existing `numeric` marks
#' data(waka, package = 'spatstat.data')
#' waka
#' set.seed(23); append_marks(waka) = rlnorm(n = npoints(waka))
#' waka
#' 
#' # existing `multitype` marks
#' data(urkiola, package = 'spatstat.data')
#' urkiola
#' set.seed(42); append_marks(urkiola) = rlnorm(n = npoints(urkiola))
#' urkiola
#' 
#' # existing `dataframe` marks
#' data(stonetools, package = 'spatstat.data')
#' stonetools
#' set.seed(33); append_marks(stonetools) = rlnorm(n = npoints(stonetools))
#' stonetools
#' 
#' @keywords internal
#' @name append_marks_set
#' @export
`append_marks<-` <- function(x, value) UseMethod(generic = 'append_marks<-')
  
  
#' @rdname append_marks_set
#' @importFrom spatstat.geom markformat.ppp npoints.ppp
#' @export append_marks<-.ppp
#' @export
`append_marks<-.ppp` <- function(x, value) {
  
  if (length(value) != npoints.ppp(x)) stop('length not match')
  
  switch(markformat.ppp(x), none = {
    x$markformat <- 'vector'
    x$marks <- value

  }, vector = {
    x$markformat <- 'dataframe'
    x$marks <- data.frame(m1 = x$marks, m2 = value)
    
  }, dataframe = {
    newid <- length(x$marks) + 1L
    x$marks <- data.frame(x$marks, value)
    names(x$marks)[newid] <- paste0('m', newid)
      
  }, stop('incorrect markformat?'))
  
  return(x)
  
} 




if (FALSE) {
  spatstat.geom::`marks<-`
  library(spatstat.geom); methods(`marks<-`)
  spatstat.geom::`marks<-.ppp` # not *exactly* what Tingting need
  spatstat.geom::append.psp # no
  base::append # not S3 generic
}

