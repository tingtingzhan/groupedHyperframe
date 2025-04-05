

#' @title Mark Names
#' 
#' @description
#' Get the names of \link[spatstat.geom]{marks}.  
#' Assign a name to \link[spatstat.geom]{marks}, when \link[spatstat.geom]{markformat} is `'vector'`.
#' 
#' 
#' @param x only \link[spatstat.geom]{ppp.object} is tested, for now
#' 
#' @returns 
#' Function [mark_names()] returns the names of the \link[spatstat.geom]{marks}
#' of a \link[spatstat.geom]{ppp.object}, when its \link[spatstat.geom]{markformat} is `'dataframe'`.
#' Otherwise, an error is returned.
#' 
#' @examples
#' library(spatstat.data)
#' mark_names(betacells)
#' @keywords internal
#' @importFrom spatstat.geom markformat marks
#' @name mark_names
#' @export
mark_names <- function(x) { # only for `ppp`, as for now
  # trying to use S3 generic as much as possible
  if (markformat(x) != 'dataframe') stop('only applicable to `markformat == dataframe`')
  x |> 
    marks(dfok = TRUE, drop = FALSE) |> 
    names()
}


#' @rdname mark_names
#' 
#' @details
#' Syntactic sugar [mark_name<-()] converts a \link[spatstat.geom]{ppp.object}
#' of `'vector'` \link[spatstat.geom]{markformat} into `'dataframe'` \link[spatstat.geom]{markformat},
#' and name this \link[base]{ncol}-`1L` \link[base]{data.frame} \link[spatstat.geom]{marks} 
#' by `value`.
#' 
#' @returns 
#' Syntactic sugar [mark_name<-()] returns a \link[spatstat.geom]{ppp.object}
#' of `'dataframe'` \link[spatstat.geom]{markformat}.
#' 
#' @examples
#' # ?waka
#' tryCatch(mark_names(waka), error = identity)
#' waka2 = waka
#' mark_name(waka2) = 'dbh' # tree diameter at breast height `dbh`
#' mark_names(waka2)
#' unstack(waka) # no name
#' unstack(waka2) # has name
#' @importFrom spatstat.geom marks marks<-
#' @export
`mark_name<-` <- function(x, value) {

  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) stop('illegal `value`')
  if (!identical(make.names(value), value)) stop('`value` is not syntactically valid')

  x |> 
    markformat() |>
    switch('dataframe' = {
      m <- marks(x, dfok = TRUE, drop = FALSE)
      if (!length(m)) stop('shouldnt happen')
      if (length(m) > 1L) stop('original `marks` has >1 columns')
    }, 'vector' = {
      m <- marks(x) |> data.frame()
    }, 'none' = {
      stop('input `x` has no `marks`')
    })
  
  names(m) <- value
  
  marks(x, dfok = TRUE, drop = FALSE) <- m 
  # already updates `x[['markformat']]`
  # do not **need** syntactic sugar spatstat.geom:::`markformat<-` !!!
  
  return(x)
  
} 


