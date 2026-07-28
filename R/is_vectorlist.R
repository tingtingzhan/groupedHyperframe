
#' @title Vector-List
#' 
#' @description
#' To determine if an R object is a \link[base]{list} of 
#' \link[base]{vector}s with the same 
#' \link[base]{storage.mode},
#' \link[base]{length} and
#' \link[base]{attributes}.
#' 
#' 
#' @param x a \link[stats]{listof}
#' 
#' @param mode \link[base]{character} scalar other than `'any'`, `'complex'` and '`raw`',
#' see the function \link[base]{is.vector}
#' 
#' @returns
#' The function [is.vectorlist()] returns a \link[base]{logical} scalar.
#' 
#' @examples
#' spatstat.data::Kovesi$values |>
#'  is.vectorlist(mode = 'character') |>
#'  stopifnot()
#' spatstat.data::Kovesi$values |>
#'  is.vectorlist(mode = 'numeric')
#' @export
is.vectorlist <- function(
    x, 
    mode = c('logical', 'integer', 'numeric', 'double', 'character')
) {
  
  if (missing(mode)) {
    mode <- x[[1L]] |> 
      storage.mode()
    # mode(1.2) # 'numeric'
    # storage.mode(1.2) # 'double'
  }
  mode <- match.arg(mode)
  
  if (!is.list(x)) return(FALSE)
  
  id <- x |>
    vapply(FUN = is.vector, mode = mode, FUN.VALUE = NA)
  if (any(!id)) return(FALSE)
  
  id <- x |> 
    lengths(use.names = FALSE) |>
    duplicated.default()
  if (!all(id[-1L])) return(FALSE)
  
  id <- x |>
    #lapply(FUN = names) |> # NULL-name compatible
    lapply(FUN = attributes) |>
    duplicated.default()
  if (!all(id[-1L])) return(FALSE)
  
  return(TRUE)
  
}