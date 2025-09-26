

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




#' @title \link[base]{log} of \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' ...
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param base \link[base]{numeric} scalar
#' 
#' @details
#' Function [log.ppp()] takes a \link[base]{log} of continuous marks 
#' of a \link[spatstat.geom]{ppp.object}.
#' 
#' Functions [log1p.ppp()], [log10.ppp()] and [log2.ppp()] are similar.
#' 
#' @return 
#' Functions [log.ppp()], [log1p.ppp()], [log10.ppp()] and [log2.ppp()] 
#' all return a \link[spatstat.geom]{ppp.object}.
#' 
#' @keywords internal
#' @name log_ppp
#' @importFrom spatstat.geom markformat marks marks<-
#' @export log.ppp
#' @export
log.ppp <- function(x, base = exp(1)) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      marks(x, dfok = TRUE, drop = FALSE)[id] <- m[id] |> lapply(FUN = log, base = base)
    }, 'vector' = {
      if (is.numeric(m)) marks(x) <- m |> log(base = base)
      # else do nothing
    }, 'none' = {
      # do nothing
    })
  
  return(x)
  
}


# base::log1p is S3 generic!!
#' @rdname log_ppp
#' @importFrom spatstat.geom markformat marks marks<-
#' @export log1p.ppp
#' @export
log1p.ppp <- function(x) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        #vapply(FUN = is.numeric, FUN.VALUE = NA) # ?base::is.numeric will pass 'POSIXct'
        vapply(FUN = inherits, what = 'numeric', FUN.VALUE = NA)
      marks(x, dfok = TRUE, drop = FALSE)[id] <- m[id] |> lapply(FUN = log1p)
    }, 'vector' = {
      if (is.numeric(m)) marks(x) <- m |> log1p()
      # else do nothing
    }, 'none' = {
      # do nothing
    })
  
  return(x)
  
}


#' @rdname log_ppp
#' @importFrom spatstat.geom markformat marks marks<-
#' @export log10.ppp
#' @export
log10.ppp <- function(x) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      marks(x, dfok = TRUE, drop = FALSE)[id] <- m[id] |> lapply(FUN = log10)
    }, 'vector' = {
      if (is.numeric(m)) marks(x) <- m |> log10()
      # else do nothing
    }, 'none' = {
      # do nothing
    })
  
  return(x)
  
}


#' @rdname log_ppp
#' @importFrom spatstat.geom markformat marks marks<-
#' @export log2.ppp
#' @export
log2.ppp <- function(x) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      marks(x, dfok = TRUE, drop = FALSE)[id] <- m[id] |> lapply(FUN = log2)
    }, 'vector' = {
      if (is.numeric(m)) marks(x) <- m |> log2()
      # else do nothing
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

