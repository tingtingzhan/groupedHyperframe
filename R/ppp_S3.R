

#' @title log.ppp
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
#' @return 
#' Function [log.ppp()] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @examples
#' data(longleaf, package = 'spatstat.data')
#' longleaf |> plot()
#' longleaf |> log() |> plot()
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
      if (is.numeric(m)) marks(x) <- log(m, base = base)
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
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      marks(x, dfok = TRUE, drop = FALSE)[id] <- m[id] |> lapply(FUN = log1p)
    }, 'vector' = {
      if (is.numeric(m)) marks(x) <- m |> log1p()
      # else do nothing
    }, 'none' = {
      # do nothing
    })
  
  return(x)
  
}



# nobs.ppp <- function(object, ...) .Defunct(new = 'spatstat.geom::npoints.ppp')






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
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' 
#' npoints(nbfires)
#' npoints(na.omit.ppp(nbfires))
#' 
#' npoints(amacrine)
#' npoints(na.omit.ppp(amacrine)) # no missing marks to be removed
#' 
#' nbfires_julian = unstack.ppp(nbfires)$out.julian
#' suppressWarnings(print.ppp(nbfires_julian))
#' suppressWarnings(plot.ppp(nbfires_julian))
#' na.omit.ppp(nbfires_julian)
#' @keywords internal
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

