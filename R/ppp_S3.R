

#' @title Is \link[spatstat.geom]{marks} of \link[spatstat.geom]{ppp.object} \link[base]{numeric} ?
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @keywords internal
#' @export is.numeric.ppp
#' @export
is.numeric.ppp <- function(x) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
    }, 'vector' = {
      is.numeric(m)
    }, 'none' = {
      logical()
    })
  
}




#' @title `Math` \link[base]{groupGeneric} of \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' ...
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param ... additional parameters for `Math` \link[base]{groupGeneric}
#' 
#' @details
#' Function [Math.ppp()] performs `Math` operations on \link[base]{numeric} \link[spatstat.geom]{marks}
#' of a \link[spatstat.geom]{ppp.object}.
#' 
#' @return 
#' Functions [Math.ppp()] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom markformat marks marks<-
#' @export Math.ppp
#' @export
Math.ppp <- function(x, ...) {
  
  # see ?spatstat.geom::Math.im for programing tricks!
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      marks(x, dfok = TRUE, drop = FALSE)[id] <- m[id] |>
        lapply(FUN = \(i) {
          do.call(what = .Generic, args = list(x = i, ...))
        })
    }, 'vector' = {
      if (is.numeric(m)) {
        marks(x) <- do.call(what = .Generic, args = list(x = m, ...))
      } # else do nothing
    }, 'none' = {
      # do nothing
    })
  
  return(x)
  
}




#' @title Handle Missing \link[spatstat.geom]{marks} in \link[spatstat.geom]{ppp.object}.
#' 
#' @param object a \link[spatstat.geom]{ppp.object}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [na.omit.ppp()] omits missing \link[spatstat.geom]{marks} in a \link[spatstat.geom]{ppp.object}.
#' 
#' @returns
#' Function [na.omit.ppp()] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @note
#' tzh suppose missing `$x` and `$y` are 
#' forbidden in \link[spatstat.geom]{ppp.object} anyway.
#' 
#' @keywords internal
#' @name na_fail_ppp
#' @importFrom stats na.omit
#' @importFrom spatstat.geom subset.ppp markformat.ppp
#' @method na.omit ppp
#' @export na.omit.ppp
#' @export
na.omit.ppp <- function(object, ...) {

  switch(markformat.ppp(object), none = {
    return(object) # exception handling
    
  }, {
    
    tmp <- na.omit(object$marks)
    # ?stats:::na.omit.data.frame; if (markformat.ppp(object) == 'dataframe')
    # ?stats:::na.omit.default; if (markformat.ppp(object) == 'vector')
    
    id <- attr(tmp, which = 'na.action', exact = TRUE)
    
    if (!length(id)) return(object) # nothing to omit
    
    ret <- subset.ppp(object, subset = -id)
    attr(ret, which = 'na.action') <- id
    return(ret)
    
  })
  
}


#' @name na_fail_ppp
#' @importFrom stats na.exclude
#' @importFrom spatstat.geom subset.ppp markformat.ppp
#' @method na.exclude ppp
#' @export na.exclude.ppp
#' @export
na.exclude.ppp <- function(object, ...) {
  
  switch(markformat.ppp(object), none = {
    return(object) # exception handling
    
  }, {
    
    tmp <- na.exclude(object$marks)
    # ?stats:::na.exclude.data.frame; if (markformat.ppp(object) == 'dataframe')
    # ?stats:::na.exclude.default; if (markformat.ppp(object) == 'vector')
    
    id <- attr(tmp, which = 'na.action', exact = TRUE)
    
    if (!length(id)) return(object) # nothing to omit
    
    ret <- subset.ppp(object, subset = -id)
    attr(ret, which = 'na.action') <- id
    return(ret)
    
  })
  
}

