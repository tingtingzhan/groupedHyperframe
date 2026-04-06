

#' @title Aggregate Hyper Data Frame
#' 
#' @description
#' To aggregate the aggregatable hyper columns of a 
#' \link[spatstat.geom]{hyperframe}.
#' 
#' @param x a \link[spatstat.geom]{hyperframe}
#' 
#' @param by a one-sided \link[stats]{formula}, 
#' containing regular-column names of the input `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The `S3` method [aggregate.hyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#'  
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom cbind.hyperframe
#' @export
aggregate.hyperframe <- function(
    x, 
    by,
    ...
) {
  
  x0 <- unclass(x)
  xdf <- x0$df
  xhc <- x0$hypercolumns
  
  if (!is.call(by) || by[[1L]] != '~' || length(by) != 2L) stop('`by` must be one-sided formula')
  if (!is.symbol(by. <- by[[2L]])) stop('right-hand-side of `by` must be a symbol')
  if (!(as.character(by.) %in% names(xdf))) stop('`.by` must be a column, not a hypercolumn')
  
  f <- xdf[[by.]] |> 
    as.factor()
  if (all(table(f) == 1L)) return(x) # exception handling
  
  xdf_ag <- xdf |>
    aggregate.data.frame(
      by = list(f), 
      FUN = unique, # hard-coded!!!!
      simplify = TRUE, drop = TRUE)
  xdf_ag <- xdf_ag[-1L]
  orig_class <- xdf |> lapply(FUN = class)
  ag_class <- xdf_ag |> lapply(FUN = class)
  id <- which(!mapply(FUN = identical, orig_class, ag_class)) # columns to be turned into hypercolumns
  if (length(id)) {
    names(id) |>
      col_blue() |> style_bold() |>
      paste(collapse = ', ') |>
      sprintf(fmt = 'Variable(s) %s removed from aggregation') |>
      message()
    xdf_ag[id] <- NULL 
  }
  
  xhc_ag <- xhc |>
    lapply(FUN = split.default, f = f)
  
  ret <- xdf_ag |>
    list() |>
    c(xhc_ag) |>
    do.call(
      what = cbind.hyperframe, 
      args = _
    )
  
  return(ret)
  
}






