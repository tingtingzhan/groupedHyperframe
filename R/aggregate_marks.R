

#' @title [aggregate_marks]
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of function \link[stats]{aggregate.data.frame}
#' 
#' @keywords internal
#' @name aggregate_marks
#' @export
aggregate_marks <- function(x, ...) UseMethod(generic = 'aggregate_marks')
  

#' @rdname aggregate_marks
#' @importFrom stats aggregate.data.frame
#' @importFrom spatstat.geom marks.ppp
#' @export aggregate_marks.ppp
#' @export
aggregate_marks.ppp <- function(x, ...) {
  
  if (markformat.ppp(x) != 'dataframe') stop('input must have dataframe-markformat')
  
  x |> 
    marks.ppp(dfok = TRUE, drop = FALSE) |>
    aggregate.data.frame(x = _, ...)
  
}