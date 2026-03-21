
# very experimental!!

#' @importFrom spatstat.geom with.hyperframe
#' @export
within.hyperframe <- function(
    data, expr, 
    ee = substitute(expr), # from ?spatstat.geom::with.hyperframe
    ...
) {
  
  if (ee[[1L]] != '{') stop()
  
  for (cl in as.list(ee[-1L])) { # recursive!!!
    
    if (!(as.character(cl[[1L]]) %in% c('=', '<-'))) stop()
    if (!is.symbol(cl[[2L]])) stop()
    
    tmp <- 
      # with.hyperframe(
      with_hyperframe_mc(
        data, ee = cl[[3L]], simplify = FALSE
      )
    tmp_n <- tmp |>
      lengths(use.names = FALSE)
    tmp_vec <- tmp |>
      vapply(FUN = is.vector, FUN.VALUE = NA)
    if (all(tmp_vec) && all(tmp_n == 1L)) {
      tmp <- tmp |>
        unlist(use.names = FALSE)
    }
    
    # to create a **new** (hyper)column,
    # spatstat.geom::`[[<-.hyperframe` # does not work!!
    # spatstat.geom::`$<-.hyperframe` # must use!!!
    call(name = '$', quote(data), cl[[2L]]) |>
      call(name = '<-', . = _, quote(tmp)) |>
      eval()
    
  }
  
  return(data)
  
}