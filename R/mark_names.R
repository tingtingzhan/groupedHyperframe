

#' @title Mark Names
#' 
#' @param x \link[spatstat.geom]{ppp.object}
#' 
#' @examples
#' library(spatstat.data)
#' mark_names(betacells)
#' 
#' # ?waka
#' tryCatch(mark_names(waka), error = identity)
#' waka2 = waka
#' mark_name(waka2) = 'dbh' # tree diameter at breast height `dbh`
#' mark_names(waka2)
#' unstack(waka) # no name
#' unstack(waka2) # has name
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


