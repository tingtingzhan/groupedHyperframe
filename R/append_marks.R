

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
#' @keywords internal
#' @name append_marks_set
#' @export
`append_marks<-` <- function(x, value) UseMethod(generic = 'append_marks<-')
  
  
#' @rdname append_marks_set
#' @importFrom spatstat.geom markformat.ppp npoints.ppp
#' @export append_marks<-.ppp
#' @export
`append_marks<-.ppp` <- function(x, value) {
  
  value. <- substitute(value)
  
  #v <- tryCatch(expr = eval(value., envir = parent.frame()), error = identity)
  v <- eval(value., envir = parent.frame()) # let err; `language` `eval`uate correctly.
  if (is.language(v)) {
    .Defunct(new = 'spatstat.geom::cut.ppp')
    v <- x |> 
      marks.ppp() |>
      eval(v, envir = _)
  } # else do nothing

  npt <- npoints.ppp(x)
    
  if (!is.recursive(v)) { # 'list' is also ?base::is.vector !
    
    if (length(v) != npt) stop('length not match')
    
    switch(markformat.ppp(x), none = {
      x$markformat <- 'vector'
      x$marks <- v
      
    }, vector = {
      x$markformat <- 'dataframe'
      x$marks <- data.frame(m1 = x$marks, m2 = v)
      
    }, dataframe = {
      newid <- length(x$marks) + 1L
      x$marks <- data.frame(x$marks, v)
      names(x$marks)[newid] <- paste0('m', newid)
      
    }, stop('incorrect markformat?'))
    
    return(x)
    
  }

  # else if (is.recursive(v));
  if (!all(lengths(v) == npt)) stop('list `v` must have all lengths as `npt`')
  
  for (iv in v) {
    append_marks(x) <- iv # lazy and beautiful!
  }
  
  vnm <- names(v)
  if (length(vnm) && !anyNA(vnm) && all(nzchar(vnm))) {
    nv <- length(v)
    nm <- length(x$marks)
    names(x$marks)[(nm-nv+1):nm] <- vnm
  }
  
  return(x)
  
} 




if (FALSE) {
  spatstat.geom::`marks<-`
  library(spatstat.geom); methods(`marks<-`)
  spatstat.geom::`marks<-.ppp` # not *exactly* what Tingting need
  spatstat.geom::append.psp # no
  base::append # not S3 generic
}

