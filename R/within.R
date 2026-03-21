
# very experimental!!



#' @importFrom spatstat.geom with.hyperframe
#' @export
within.hyperframe <- function(
    data, expr, 
    ee = substitute(expr), # learning from ?spatstat.geom::with.hyperframe
    ...
) {
  
  if (ee[[1L]] != '{') stop()
  
  for (cl in as.list(ee[-1L])) { # recursive!!!
    if (!(as.character(cl[[1L]]) %in% c('=', '<-'))) stop()
    if (!is.symbol(cl[[2L]])) stop()
    # spatstat.geom::`[[<-.hyperframe` # does not work!!
    # spatstat.geom::`$<-.hyperframe` # must use!!!
    call(name = '$', quote(data), cl[[2L]]) |>
      call(
        name = '<-', . = _, 
        call(name = 'with.hyperframe', data = quote(data), expr = cl[[3L]], simplify = FALSE)
        ) |>
      eval()
  }
  
  return(data)
  
}