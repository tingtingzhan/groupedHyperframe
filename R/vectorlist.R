
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
#' @examples
#' x = spatstat.data::Kovesi$values
#' class(x) = c('vectorlist', class(x)) 
#' z1 = x |> t.vectorlist()
#' z2 = spatstat.data::Kovesi |> 
#'   spatstat.geom::with.hyperframe(expr = values[1L])
#' stopifnot(identical(z1[[1L]], z2))
#' 
#' @keywords internal
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