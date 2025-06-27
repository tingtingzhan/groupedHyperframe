

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
#' 
#' @param ... additional parameters of workhorse functions 
#' [fv_ppp()] or [dist_ppp()]
#' 
#' @details
#' Function [op_ppplist()] is a \pkg{parallel} batch process of 
#' the workhorse function [fv_ppp()] or [dist_ppp()].
#' 
#' @returns 
#' Function [op_ppplist()] returns a \link[stats]{listof} 
#' \itemize{
#' \item function [fv_ppp()] returns, if `op = fv_ppp`.
#' \item function [dist_ppp()] returns, if `op = dist_ppp`.
#' }
#' 
#' @examples
#' \dontshow{options(mc.cores = 1L)}
#' library(spatstat.data)
#' library(spatstat.geom) # for ?spatstat.geom::split.ppp
#' library(spatstat.explore) # for ?spatstat.explore::Emark, etc.
#' 
#' Vc = with(shapley$marks, expr = {
#'  cut.default(V, breaks = quantile(V, probs = c(0, 1/3, 2/3, 1)), labels = c('L', 'M', 'H'))
#' })
#' \donttest{
#' x1 = shapley |> 
#'  subset.ppp(select = c('Mag', 'SigV')) |>
#'  split.ppp(f = Vc) |>
#'  op_ppplist(op = fv_ppp, fn = markcorr)
#' names(x1)
#' names(x1$L)
#' }
#' 
#' x2 = nbfires |> 
#'   subset.ppp(select = c('fire.type', 'cause', 'ign.src')) |>
#'   na.omit.ppp() |> 
#'   split.ppp(f = 'fire.type')
#' x2 |> op_ppplist(op = dist_ppp, fn = .nncross, i = 'rrds', j = 'ltning')
#' x2 |> op_ppplist(op = dist_ppp, fn = .nncross, i = 'unknown', j = 'burn.no.perm')
#' @keywords internal
#' @importFrom parallel mclapply detectCores
#' @export
op_ppplist <- function(
    x, 
    op,
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  n <- length(x)
  
  ret <- n |>
    seq_len() |>
    mclapply(mc.cores = mc.cores, FUN = \(i) {
    #lapply(FUN = \(i) { # when debugging
      # echo-command does not work with '\r' (carriage return)
      if (identical(Sys.getenv('RSTUDIO'), '1')) sprintf(fmt = 'printf \'\r%d/%d done!    \'', i, n) |> system() |> on.exit()
      x[[i]] |> op(...)
    })
  message() |> on.exit()
  
  names(ret) <- names(x)
  return(ret)
  
}


