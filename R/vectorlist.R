

#' @title [is.vectorlist]
#' 
#' @param x an \link[spatstat.geom]{anylist}
#' 
#' @param ... additional parameter of \link[base]{is.vector}, currently only `mode`
#' 
#' @keywords internal
#' @export
is.vectorlist <- function(x, ...) {
  
  if (!inherits(x, what = 'anylist')) return(FALSE)

  id <- x |>
    vapply(FUN = is.vector, ..., FUN.VALUE = NA)
  if (any(!id)) return(FALSE)
  
  id <- x |> 
    lengths(use.names = FALSE) |>
    duplicated.default()
  if (!all(id[-1L])) return(FALSE)
    
  return(TRUE)
  
}





#' @title Transpose a `'vectorlist'`
#' 
#' @param x a `'vectorlist'` of equi-\link[base]{length}
#' 
#' @details
#' tzh defines a derived class `'vectorlist'`,
#' i.e., a \link[stats]{listof} \link[base]{vector}s,
#' which \link[base]{inherits} from 
#' \link[spatstat.geom]{anylist}. 
#' The implementation of `'vectorlist'` is 
#' inspired by class \link[spatstat.geom]{solist}.
#' 
#' The `S3` method dispatch [t.vectorlist()], 
#' of the generic function \link[base]{t},
#' transposes a `'vectorlist'` of equi-\link[base]{length}.
#' We illustrate this concept using data set 
#' \link[spatstat.data]{Kovesi} in **Examples**.
#' 
#' @note
#' The motivation of 
#' the derived class `'vectorlist'` and 
#' the method dispatch [t.vectorlist()] 
#' is that 
#' function \link[spatstat.geom]{with.hyperframe}
#' could be slow in a batch process.
#' 
#' @returns
#' The `S3` method dispatch [t.vectorlist()] returns
#' a `'vectorlist'` of equi-\link[base]{length}.
#' 
#' @keywords internal
#' @importFrom stats setNames
#' @export t.vectorlist
#' @export
t.vectorlist <- function(x) {
  
  # upstream definition to ensure this
  # if (!all(vapply(x, FUN = is.vector, FUN.VALUE = NA))) stop('each element of `x` must be `vector`')
  
  nx <- lengths(x, use.names = FALSE)
  if (!all(duplicated(nx)[-1L])) stop('each element of `x` must be of same `length`')
  
  nm <- lapply(x, FUN = names)
  if (!all(duplicated(nm)[-1L])) stop('each element of `x` must have the same names, or no name')
  
  .clist <- \(x) {
    # convert columns of 'matrix' to a 'list'
    x |>
      ncol() |>
      seq_len() |>
      lapply(FUN = \(i) x[, i, drop = TRUE]) |>
      setNames(nm = colnames(x)) # colnames-NULL compatible
  }
  
  ret <- x |> 
    do.call(what = rbind, args = _) |>
    .clist()
  class(ret) <- c('vectorlist', 'anylist', 'listof', 'list')
  return(ret)
  
}