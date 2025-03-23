

#' @title Batch Operations of `'ppplist'` Object
#' 
#' @description
#' Batch operations of function [fv_ppp()] or [dist_ppp()], for a `'ppplist'` input.
#' 
#' @param x a `'ppplist'` object
#' 
#' @param op workhorse \link[base]{function}, either [fv_ppp()] or [dist_ppp()]
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
#' CRAN requires `mc.cores <= 2L` in examples.
#' 
#' @param ... additional parameters of workhorse functions 
#' [fv_ppp()] or [dist_ppp()]
#' 
#' @details
#' Function [fv_ppplist()] is a \pkg{parallel} batch process of the workhorse function [fv_ppp()].
#' 
#' Function [dist_ppplist()] is a \pkg{parallel} batch process of the workhorse function [dist_ppp()].
#' 
#' @returns 
#' Function [fv_ppplist()] returns a \link[base]{list} of function [fv_ppp()] returns.
#' 
#' Function [dist_ppplist()] returns a \link[base]{list} of function [dist_ppp()] returns.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom) # for ?spatstat.geom::split.ppp
#' library(spatstat.explore) # for ?spatstat.explore::Emark, etc.
#' 
#' Vc = with(shapley$marks, expr = {
#'  cut.default(V, breaks = quantile(V, probs = c(0, 1/3, 2/3, 1)), labels = c('L', 'M', 'H'))
#' })
#' x1 = shapley |> 
#'  subset.ppp(select = c('Mag', 'SigV')) |>
#'  split.ppp(f = Vc) |>
#'  fv_ppplist(fn = markcorr, mc.cores = 1L)
#' names(x1)
#' names(x1$L)
#' 
#' x2 = nbfires |> 
#'   subset.ppp(select = c('fire.type', 'cause', 'ign.src')) |>
#'   na.omit.ppp() |> 
#'   split.ppp(f = 'fire.type')
#' dist_ppplist(x2, fn = .nncross, i = 'rrds', j = 'ltning', mc.cores = 1L)
#' dist_ppplist(x2, fn = .nncross, i = 'unknown', j = 'burn.no.perm', mc.cores = 1L)
#' @keywords internal
#' @name op_ppplist
#' @export
fv_ppplist <- function(x, ...) op_ppplist(x, op = fv_ppp, ...)
  
#' @rdname op_ppplist
#' @export
dist_ppplist <- function(x, ...) op_ppplist(x, op = dist_ppp, ...)
  


#' @rdname op_ppplist
#' @importFrom parallel mclapply detectCores
#' @export
op_ppplist <- function(
    x, 
    op,
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()),
    ...
) {
  
  n <- length(x)
  
  ret <- mclapply(X = seq_len(n), mc.cores = mc.cores, FUN = function(i) {
  #ret <- lapply(X = seq_len(n), FUN = function(i) { # to debug inside
    # echo-command does not work with '\r' (carriage return)
    if (identical(Sys.getenv('RSTUDIO'), '1')) on.exit(system(command = sprintf(fmt = 'printf \'\r%d/%d done!    \'', i, n)))
    return(op(x = x[[i]], ...))
  })
  on.exit(message())
  
  names(ret) <- names(x)
  return(ret)
  
}


