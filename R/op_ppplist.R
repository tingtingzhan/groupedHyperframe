

#' @title Batch Operations of `'ppplist'` Object
#' 
#' @description
#' Batch operations of function [ppp2fv()] or [ppp2dist()], for a `'ppplist'` input.
#' 
#' @param x a `'ppplist'` object
#' 
#' @param op workhorse \link[base]{function}, either [ppp2fv()] or [ppp2dist()]
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
#' 
#' @param ... additional parameters of workhorse functions 
#' [ppp2fv()] or [ppp2dist()]
#' 
#' @details
#' Function [op_ppplist()] is a \pkg{parallel} batch process of 
#' the workhorse function [ppp2fv()] or [ppp2dist()].
#' 
#' @returns 
#' Function [op_ppplist()] returns a \link[stats]{listof} 
#' \itemize{
#' \item function [ppp2fv()] returns, if `op = ppp2fv`.
#' \item function [ppp2dist()] returns, if `op = ppp2dist`.
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
#' shapley |> 
#'  subset.ppp(select = c('Mag', 'SigV')) |>
#'  split.ppp(f = Vc) |>
#'  op_ppplist(op = ppp2fv, fn = markcorr)
#' }
#' 
#' @keywords internal
#' @importFrom parallel mclapply
#' @export
op_ppplist <- function(
    x, 
    op,
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  n <- length(x)
  
  .rstudio <- identical(Sys.getenv('RSTUDIO'), '1')
  # mclapply + system('printf ...') kills vanilla R 4.5.1
  
  ret0 <- n |>
    seq_len() |>
    mclapply(mc.cores = mc.cores, FUN = \(i) {
    #lapply(FUN = \(i) { # when debugging
      if (.rstudio) {
        sprintf(fmt = 'printf \'\r%d/%d done!    \'', i, n) |> 
          # echo-command does not work with '\r' (carriage return)
          system() |> 
          on.exit()
      }
      x[[i]] |> op(...)
    }) # `ret0`: 1st subject, 2nd mark
  message() |> on.exit()
  
  names(ret0) <- names(x)
  
  # re-organize the list!!
  # `ret`: 1st mark, 2nd subject
  ret <- .mapply(FUN = list, dots = ret0, MoreArgs = NULL)
  names(ret) <- names(ret0[[1L]])

  .mapply(FUN = check_fvlist, dots = list(
    X = ret, data.name = names(ret)
  ), MoreArgs = NULL)
  
  return(ret)
  
}


if (FALSE) {
  # requires extra error check, for non-existing factor levels
  x2 = spatstat.data::nbfires |> 
    spatstat.geom::subset.ppp(select = c('fire.type', 'cause', 'ign.src')) |>
    na.omit.ppp() |> 
    spatstat.geom::split.ppp(f = 'fire.type')
  x2 |> op_ppplist(op = ppp2dist, fn = .nncross, i = 'rrds', j = 'ltning')
  x2 |> op_ppplist(op = ppp2dist, fn = .nncross, i = 'unknown', j = 'burn.no.perm')
}

