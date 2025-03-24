

#' @title Mark Names
#' 
#' @description
#' Get the names of \link[spatstat.geom]{marks}.  
#' Assign a name to \link[spatstat.geom]{marks}, when \link[spatstat.geom]{markformat} is `'vector'`.
#' 
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
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
#' @importFrom spatstat.geom markformat
#' @name mark_names
#' @export
mark_names <- function(x) { # only for `ppp`, as for now
  # trying to use S3 generic as much as possible
  if (markformat(x) != 'dataframe') stop('only applicable to `markformat == dataframe`')
  names(x[['marks']])
}


#' @rdname mark_names
#' 
#' @details
#' Syntactic sugar [mark_name<-()] converts a \link[spatstat.geom]{ppp.object}
#' of \link[spatstat.geom]{markformat} `'vector'` into \link[spatstat.geom]{markformat} `'dataframe'`,
#' and name this \link[base]{ncol}-`1L` \link[base]{data.frame} \link[spatstat.geom]{marks} 
#' by the user-specified `value`.
#' 
#' @returns 
#' Syntactic sugar [mark_name<-()] returns a \link[spatstat.geom]{ppp.object}
#' of \link[spatstat.geom]{markformat} `'dataframe'`.
#' 
#' @examples
#' # ?waka
#' tryCatch(mark_names(waka), error = identity)
#' waka2 = waka
#' mark_name(waka2) = 'dbh' # tree diameter at breast height `dbh`
#' mark_names(waka2)
#' unstack(waka) # no name
#' unstack(waka2) # has name
#' @export
`mark_name<-` <- function(x, value) {
  if (markformat(x) == 'dataframe') stop('never try to rename `dataframe` `marks`!')
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) stop('illegal `value`')
  if (!identical(make.names(value), value)) stop('`value` is not syntactically valid')
  # not sure if I want to use spatstat.geom::marks.ppp
  x[['marks']] <- data.frame(x[['marks']])
  names(x[['marks']]) <- value
  x[['markformat']] <- 'dataframe'
  # we do **not** have syntactic sugar spatstat.geom:::`markformat<-`
  return(x)
} 


