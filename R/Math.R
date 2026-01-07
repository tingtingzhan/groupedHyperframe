


#' @title \link[base]{groupGeneric} of \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' ...
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @param ... additional parameters for the \link[base]{groupGeneric} functions
#' 
#' @details
#' Function [Math.ppp()] performs `Math` operations on the \link[base]{numeric} \link[spatstat.geom]{marks}
#' of a \link[spatstat.geom]{ppp.object}.
#' 
#' Function [Summary.ppp()] ..
#' 
#' @return 
#' Function [Math.ppp()] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' Function [Summary.ppp()] ..
#' 
#' @keywords internal
#' @name groupGeneric_ppp
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

#' @rdname groupGeneric_ppp
#' @importFrom spatstat.geom markformat marks marks<-
#' @export Summary.ppp
#' @export
Summary.ppp <- function(..., na.rm = FALSE) {
  stop('comming soon')
}



#' @title \link[base]{groupGeneric} of \link[spatstat.geom]{tess}ellation
#' 
#' @description
#' ...
#' 
#' @param x a \link[spatstat.geom]{tess}ellation
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @param ... additional parameters for the \link[base]{groupGeneric} functions
#' 
#' @details
#' Function [Math.tess()] performs `Math` operations on the \link[base]{numeric} \link[spatstat.geom]{marks}
#' of a \link[spatstat.geom]{tess}ellation.
#' 
#' Function [Summary.tess()] ..
#' 
#' @return 
#' Function [Math.tess()] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' Function [Summary.tess()] ..
#' 
#' @keywords internal
#' @name groupGeneric_tess
#' @importFrom spatstat.geom is.hyperframe markformat marks.tess marks<-
#' @export Math.tess
#' @export
Math.tess <- function(x, ...) {
  
  # see ?spatstat.geom::Math.im for programing tricks!
  # we do not have function [markformat.tess]..  ask Dr. Baddeley?
  
  # ?spatstat.geom::tess
  # parameter `marks`: Optional vector, data frame or hyperframe of marks associated with the tiles.

  m <- x |>
    marks.tess()
  
  if (!length(m)) {
    # do nothing
  } else if (is.vector(m)) {
    stop('example?')
  } else if (is.data.frame(m)) {
    id <- m |>
      vapply(FUN = is.numeric, FUN.VALUE = NA)
    m[id] <- m[id] |>
      lapply(FUN = \(i) {
        do.call(what = .Generic, args = list(x = i, ...))
      })
    marks(x) <- m # spatstat.geom::`marks<-.tess`
  } else if (is.hyperframe(m)) {
    stop('example?')
  } else {
    stop('not programed yet!!!')
  }
  
  return(x)
  
}


#' @rdname groupGeneric_tess
#' @importFrom spatstat.geom markformat marks marks<-
#' @export Summary.tess
#' @export
Summary.tess <- function(..., na.rm = FALSE) {
  stop('comming soon')
}





#' @title \link[base]{groupGeneric} of \link[spatstat.geom]{psp.object}
#' 
#' @description
#' ...
#' 
#' @param x a \link[spatstat.geom]{psp.object}
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @param ... additional parameters for the \link[base]{groupGeneric} functions
#' 
#' @details
#' Function [Math.psp()] performs `Math` operations on the \link[base]{numeric} \link[spatstat.geom]{marks}
#' of a \link[spatstat.geom]{psp.object}.
#' 
#' Function [Summary.psp()] ..
#' 
#' @return 
#' Function [Math.psp()] returns a \link[spatstat.geom]{psp.object}.
#' 
#' Function [Summary.psp()] ..
#' 
#' @keywords internal
#' @name groupGeneric_psp
#' @export Math.psp
#' @export
Math.psp <- function(x, ...) {
  stop('comming soon')
}
  
#' @rdname groupGeneric_psp
#' @export Summary.psp
#' @export
Summary.psp <- function(..., na.rm = FALSE) {
  stop('comming soon')
}




#' @title \link[base]{groupGeneric} of `'ppplist'`
#' 
#' @description
#' ...
#' 
#' @param x an `'ppplist'`
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @param ... additional parameters for the \link[base]{groupGeneric} functions
#' 
#' @return 
#' Function [Math.ppplist()] returns a `'ppplist'`.
#' 
#' Function [Summary.ppplist()] ...
#' 
#' @keywords internal
#' @name groupGeneric_ppplist
#' @importFrom spatstat.geom solapply
#' @export Math.ppplist
#' @export
Math.ppplist <- function(x, ...) {
  x |> 
    solapply(FUN = .Generic, ...)
}

#' @rdname groupGeneric_ppplist
#' @importFrom spatstat.geom solapply
#' @export Summary.ppplist
#' @export
Summary.ppplist <- function(..., na.rm = FALSE) {
  stop('comming soon')
}


#' @title \link[base]{groupGeneric} of `'fvlist'`
#' 
#' @description
#' ...
#' 
#' @param x an `'fvlist'`
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @param ... additional parameters for the \link[base]{groupGeneric} functions
#' 
#' @return 
#' Function [Math.fvlist()] returns an `'fvlist'`.
#' 
#' Function [Summary.fvlist()] ..
#' 
#' @keywords internal
#' @name groupGeneric_fvlist
#' @importFrom spatstat.explore Math.fv
#' @export Math.fvlist
#' @export
Math.fvlist <- function(x, ...) {
  x |> 
    lapply(FUN = .Generic, ...) |>
    as.fvlist()
}

#' @rdname groupGeneric_fvlist
#' @importFrom spatstat.explore Math.fv
#' @export Summary.fvlist
#' @export
Summary.fvlist <- function(..., na.rm = FALSE) {
  stop('comming soon')
}



