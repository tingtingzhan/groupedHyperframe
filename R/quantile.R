

#' @title Quantiles of \link[base]{numeric} \link[spatstat.geom]{marks} in \link[spatstat.geom]{ppp.object}
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param ... additional parameters of the function `stats:::quantile.default()`
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
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




