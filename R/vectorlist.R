

#' @title [is.vectorlist]
#' 
#' @param x a \link[stats]{listof}
#' 
#' @param mode \link[base]{character} scalar other than `'any'`, `'complex'` and '`raw`',
#' see function \link[base]{is.vector}
#' 
#' @examples
#' spatstat.data::Kovesi$values |>
#'  is.vectorlist(mode = 'character') |>
#'  stopifnot()
#' spatstat.data::Kovesi$values |>
#'  is.vectorlist(mode = 'numeric')
#' @keywords internal
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


#' @title [as.vectorlist]
#' 
#' @param x a \link[base]{list}
#' 
#' @param ... additional parameters of the function [is.vectorlist()]
#' 
#' @examples
#' list(rnorm(6L), rnorm(6L)) |>
#'  as.vectorlist()
#' 
#' @keywords internal
#' @export
as.vectorlist <- function(x, ...) {
  if (!is.vectorlist(x, ...)) stop('input does not qualify as a `vectorlist`')
  class(x) <- c('vectorlist', 'listof', 'list')
  return(x)
}


if (FALSE) {
  library(spatstat)
  methods(class = 'anylist') |>
    attr(which = 'info')
  # tzh don't want/need to use ?spatstat.geom::print.anylist
  # 'vectorlist' does **not** base::inherits from 'anylist'
}




#' @export
print.vectorlist <- function(x, ...) {
  
  '\'vectorlist\'' |>
    col_br_red() |> style_bold() |>
    cat('\n')
  
  x |>
    length() |>
    col_green() |> style_bold() |>
    sprintf(fmt = 'Number of Vectors: %s') |>
    cat('\n')
  
  nm <- x |>
    names()
  if (!all(nm == seq_along(x))) {
    nm |>
      col_cyan() |> style_bold() |>
      paste(collapse = ', ') |>
      sprintf(fmt = 'Name(s): %s') |>
      cat('\n')
  }
  
  x[[1L]] |> 
    storage.mode() |>
    col_blue() |> style_bold() |>
    sprintf(fmt = 'Storage Mode: %s') |>
    cat('\n')
  
  x[[1L]] |> 
    length() |>
    col_magenta() |> style_bold() |>
    sprintf(fmt = 'Individual Vector Length: %s') |>
    cat('\n')
  
}



# @note
# The motivation of 
# the derived class `'vectorlist'` and 
# the method dispatch [t.vectorlist()] 
# is that 
# the `S3` method \link[spatstat.geom]{with.hyperframe}
# could be slow in a batch process.
#' 
# @returns
# The `S3` method dispatch [t.vectorlist()] returns
# a `'vectorlist'` of equi-\link[base]{length}.

#' @importFrom stats setNames
#' @export
t.vectorlist <- function(x) {
  
  # upstream definition to ensure this
  # if (!all(vapply(x, FUN = is.vector, FUN.VALUE = NA))) stop('each element of `x` must be `vector`')
  
  nx <- lengths(x, use.names = FALSE)
  if (!all(duplicated(nx)[-1L])) stop('each element of `x` must be of same `length`')
  
  nm <- lapply(x, FUN = names)
  if (!all(duplicated(nm)[-1L])) stop('each element of `x` must have the same names, or no name')
  
  x |> 
    do.call(what = rbind, args = _) |>
    asplit(MARGIN = 2L, drop = TRUE) |>
    setNames(nm = names(x[[1L]])) |>
    as.vectorlist()

}
