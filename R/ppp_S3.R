
#' @method is.numeric ppp
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







# @details
# The `S3` method [na.omit.ppp()] omits missing \link[spatstat.geom]{marks} in a \link[spatstat.geom]{ppp.object}.
# 
# @returns
# The `S3` method [na.omit.ppp()] returns a \link[spatstat.geom]{ppp.object}.
# 
# @note
# tzh suppose missing `$x` and `$y` are 
# forbidden in \link[spatstat.geom]{ppp.object} anyway.

#' @importFrom spatstat.geom subset.ppp markformat.ppp
#' @method na.omit ppp
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


#' @importFrom spatstat.geom subset.ppp markformat.ppp
#' @method na.exclude ppp
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

