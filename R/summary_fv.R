

#' @title Summarize \link[spatstat.explore]{fv}-`hypercolumns`
#' 
#' @param X a \link[spatstat.geom]{hyperframe} or [groupedHyperframe], 
#' containing one or more \link[spatstat.explore]{fv.object} column(s)
#' 
#' @param rmax \link[base]{numeric} scalar, user-specified truncation point \eqn{r_\text{max}}
#' 
#' @param ... additional parameters of function [summary.fvlist()], most importantly `mc.cores`.
#' 
#' @returns
#' Function [summary_fv()] returns a \link[spatstat.geom]{hyperframe} or [groupedHyperframe].
#' 
#' @keywords internal
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
#' @export
summary_fv <- function(X, rmax, ...) {
  
  if (!inherits(X, what = 'hyperframe')) stop('input must be hyperframe')
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `X` must contain at least one `fv` column')
  nm <- names.hyperframe(X)[id]
  
  if (!missing(rmax)) {
    if (!is.numeric(rmax) || length(rmax) != 1L || is.na(rmax) || (rmax <= 0)) stop('illegal user-specified `rmax`')
  } else rmax <- numeric() # cannot use ?base::missing inside ?base::mapply
  
  ret0 <- (as.list.hyperframe(X)[nm]) |>
    mapply(
      FUN = summary.fvlist, 
      object = _, data.name = nm, 
      MoreArgs = list(rmax = rmax, ...), SIMPLIFY = FALSE
    ) |> 
    unlist(recursive = FALSE, use.names = TRUE)
    
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(X), ret0)
  ))

}




