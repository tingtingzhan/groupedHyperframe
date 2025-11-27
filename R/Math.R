


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




#' @title `Math` \link[base]{groupGeneric} of `'ppplist'`
#' 
#' @description
#' ...
#' 
#' @param x an `'ppplist'`
#' 
#' @param ... additional parameters for `Math` \link[base]{groupGeneric}
#' 
#' @return 
#' Functions [Math.ppplist()] returns a `'ppplist'`.
#' 
#' @keywords internal
#' @importFrom spatstat.geom solapply
#' @export Math.ppplist
#' @export
Math.ppplist <- function(x, ...) {
  x |> 
    solapply(FUN = .Generic, ...)
}



#' @title `Math` \link[base]{groupGeneric} of `'fvlist'`
#' 
#' @description
#' ...
#' 
#' @param x an `'fvlist'`
#' 
#' @param ... additional parameters for `Math` \link[base]{groupGeneric}
#' 
#' @return 
#' Functions [Math.fvlist()] returns an `'fvlist'`.
#' 
#' @keywords internal
#' @importFrom spatstat.explore Math.fv
#' @export Math.fvlist
#' @export
Math.fvlist <- function(x, ...) {
  x |> 
    lapply(FUN = .Generic, ...) |>
    as.fvlist()
}




