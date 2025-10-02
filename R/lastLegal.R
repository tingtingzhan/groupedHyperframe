
#' @title Last Legal Index
#' 
#' @param v \link[base]{double} \link[base]{vector}
#' 
#' @details
#' Legal, meaning not `0`, not `NaN` and not `Inf`.
#' 
#' @keywords internal
#' @export
lastLegal <- function(v) {
  
  vok <- is.finite(v) & (abs(v) > .Machine$double.eps) # not 0, not NaN, not Inf
  
  if (all(vok)) {
    
    id <- length(vok) # faster than [.diff()]
    
  } else {
    
    .diff <- \(x) {
      x[-1L] - x[-length(x)]
    } # faster than ?base::diff.default
    
    # if `vok` starts with `FALSE`
    if (!vok[1L]) {
      # tolerate `0`
      tmp <- (abs(v) < .Machine$double.eps)
      .notsame <- (cumsum(tmp) != cumsum(rep(TRUE, times = length(tmp))))
      vok[seq_len(min(which(.notsame)) - 1L)] <- TRUE
    }
    
    z <- vok |> 
      which() |>
      .diff()
    
    id <- if (all(z == 1L)) {
      length(z) + 1L
    } else min(which(z != 1L)) # smart!
    
  }
  
  attr(id, which = 'value') <- v[id]
  return(id)
  
}


#' @title Tentative Fix for Illegal Function Value in `fv.object`
#' 
#' @param X an \link[spatstat.explore]{fv.object}
#' 
#' @keywords internal
#' @name illegal2theo
#' @export
.illegal2theo <- function(X, ...) UseMethod(generic = '.illegal2theo')

#' @rdname illegal2theo
#' @importFrom spatstat.explore fvnames
#' @export .illegal2theo.fv
#' @export
.illegal2theo.fv <- function(X, ...) {
  
  key <- X |> 
    fvnames(a = '.y')
  
  theo <- X$theo
  if (is.null(theo)) stop('this fv.object does not have theo ?')
  
  .y <- X[[key]]
  
  id <- .y |>
    lastLegal()
  sq <- id:length(.y)
  X[[key]][sq] <- theo[sq]
  
  return(X)
  
}

#' @rdname illegal2theo
#' @export .illegal2theo.fvlist
#' @export
.illegal2theo.fvlist <- function(X, ...) {
  X |> 
    lapply(FUN = .illegal2theo.fv, ...) |>
    as.fvlist()
}

#' @rdname illegal2theo
#' @export .illegal2theo.hyperframe
#' @export
.illegal2theo.hyperframe <- function(X, ...) {
  stop('still working')
}