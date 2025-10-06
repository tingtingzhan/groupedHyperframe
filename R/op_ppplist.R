

#' @title Batch Operations of `'ppplist'` Object
#' 
#' @description
#' Batch operations for a `'ppplist'` input.
#' 
#' @param x a `'ppplist'` object
#' 
#' @param op workhorse \link[base]{function}, either [ppp_numeric2fv()], [ppp_multitype2fv()] or [ppp2dist()]
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is the return of function \link[parallel]{detectCores}.
#' 
#' @param ... additional parameters of workhorse functions 
#' [ppp_numeric2fv()], [ppp_multitype2fv()] or [ppp2dist()]
#' 
#' @details
#' Function [op_ppplist()] is a \pkg{parallel} batch process of 
#' the workhorse function [ppp_numeric2fv()], [ppp_multitype2fv()] or [ppp2dist()].
#' 
#' @returns 
#' Function [op_ppplist()] returns a \link[stats]{listof} 
#' \itemize{
#' \item function [ppp_numeric2fv()] returns, if `op = ppp_numeric2fv`.
#' \item function [ppp_multitype2fv()] returns, if `op = ppp_multitype2fv`.
#' \item function [ppp2dist()] returns, if `op = ppp2dist`.
#' }
#' 
#' @keywords internal
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel mclapply
#' @importFrom spatstat.geom anylist
#' @export
op_ppplist <- function(
    x, 
    op,
    mc.cores = getOption('cores'),
    ...
) {
  
  n <- length(x) # needed for progress-printing!
  sq <- n |>
    seq_len()
  
  foo <- if (identical(Sys.getenv('RSTUDIO'), '1') && (.Platform$OS.type == 'unix')) {
    # Sys.getenv('RSTUDIO') # returns '' in both vanilla R and Positron
    # Sys.getenv('POSITRON') # returns '' in both vanilla R and RStudio 
    # parameter name must be `.i` !!
    # [Gcross_.ppp()] carries parameter `i` in `...` !!!
    \(.i, x, ...) {
      sprintf(fmt = 'printf \'\r%d/%d done!    \'', .i, n) |> 
        # echo-command does not work with '\r' (carriage return)
        system() |> 
        on.exit()
      # this command 
      # .. kills vanilla R on Mac
      # .. does not show up in RStudio on Windows
      x[[.i]] |> op(...)
    }
  } else {
    \(.i, x, ...) {
      x[[.i]] |> op(...)
    }
  }
  
  ret0 <- switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = {
      sq |>
        mclapply(mc.cores = mc.cores, FUN = foo, x = x, ...) 
        #lapply(FUN = foo, x = x, ...) # when debugging
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      foreach(i = sq, .options.multicore = list(cores = mc.cores)) %dopar% foo(.i = i, x = x, ...)
    })
  # `ret0`: 1st subject, 2nd mark
  
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


