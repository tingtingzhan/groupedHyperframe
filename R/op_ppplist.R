

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
#' @keywords internal
#' @importFrom spatstat.geom anylist
#' @export
op_ppplist <- function(
    x, 
    op,
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  n <- length(x)
  
  .rstudio <- identical(Sys.getenv('RSTUDIO'), '1')
  # Sys.getenv('RSTUDIO') returns '' in both vanilla R and Positron
  
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
  #ret <- .mapply(FUN = list, dots = ret0, MoreArgs = NULL)
  ret <- .mapply(FUN = anylist, dots = ret0, MoreArgs = NULL) # 2025-09-24
  # using `anylist` obviously correct and better, but does it actually improve anything?
  names(ret) <- names(ret0[[1L]])

  mapply(
    FUN = as.fvlist, 
    X = ret, data.name = names(ret), 
    MoreArgs = NULL, SIMPLIFY = FALSE
  )
  
}


