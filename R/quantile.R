

#' @export
quantile.ppp <- function(x, ...) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      if (sum(id) == 1L) {
        quantile(m[[which(id)]], ...)
      } else {
        m[id] |> 
          lapply(FUN = quantile, ...)
      }
    }, 'vector' = {
      if (is.numeric(m)) return(quantile(m, ...))
      return(invisible())
    }, 'none' = {
      return(invisible())
    })
  
}




