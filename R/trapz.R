

#' @title Trapzoidal Integration of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @param ... additional parameters
#' 
#' @details
#' Functions [trapz.fv()] and [cumtrapz.fv()] 
#' obtain the (cumulative) 
#' \link[pracma]{trapz}oidal integration of the area under the primary outcome 
#' of a function value \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Functions [trapz.fv()] and [vtrapz.fv()] return a \link[base]{numeric} scalar.
#' 
#' @keywords internal
#' @name trapz
#' @importFrom pracma trapz
#' @importFrom spatstat.explore fvnames
#' @export
trapz.fv <- function(
    x, 
    key = fvnames(x, a = '.y'),
    .x = fvnames(x, a = '.x'),
    ...
) {
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  trapz(x = x[[.x]], y = x[[key]]) |>
    unname()
}


#' @rdname trapz
#' @returns 
#' Functions [cumtrapz.fv()] and [cumvtrapz.fv()] return a \link[base]{numeric} \link[base]{vector}.
#' @importFrom pracma cumtrapz
#' @importFrom spatstat.explore fvnames
#' @export 
cumtrapz.fv <- function(
    x, 
    key = fvnames(x, a = '.y'),
    .x = fvnames(x, a = '.x'),
    ...
) {
  
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  
  n <- length(x[[.x]])
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- cumtrapz(x = x[[.x]], y = x[[key]])
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- x[[.x]][-1L]
  return(ret)
  
}



#' @rdname trapz
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is the return of function \link[parallel]{detectCores}.
#' 
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel mclapply makeCluster stopCluster
#' @export
cumtrapz.fvlist <- function(
    x, 
    mc.cores = getOption('cores'), 
    ...
) {
  
  x <- trunc_id.fvlist(x, ...)
  id <- x |>
    attr(which = 'id', exact = TRUE)
  .y <- x |>
    attr(which = '.y', exact = TRUE)
  .x <- x |>
    attr(which = '.x', exact = TRUE)
  
  fn <- \(i) cumtrapz.fv(i, key = .y, .x = .x)[id[-1L]]
  switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = {
      cumt <- x |> 
        mclapply(mc.cores = mc.cores, FUN = fn) # `-1L` super important!!!
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
      cumt <- foreach(i = x, .options.multicore = list(cores = mc.cores)) %dopar% fn(i)
      stopCluster(cl)
    })
  
  return(cumt)
  
}





#' @rdname trapz
#' @param rmax \link[base]{numeric} scalar, user-specified truncation point \eqn{r_\text{max}}
#' 
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
#' @export cumtrapz.hyperframe
#' @export
cumtrapz.hyperframe <- function(x, rmax, ...) {
  
  if (!any(id <- (unclass(x)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(x)[id]
  
  if (!missing(rmax)) {
    if (!is.numeric(rmax) || length(rmax) != 1L || is.na(rmax) || (rmax <= 0)) stop('illegal user-specified `rmax`')
  } else rmax <- numeric() # cannot use ?base::missing inside ?base::mapply
  
  ret0 <- (as.list.hyperframe(x)[nm]) |>
    mapply(
      FUN = cumtrapz.fvlist, 
      x = _, data.name = nm, 
      MoreArgs = list(rmax = rmax, ...), SIMPLIFY = FALSE
    )
  
  names(ret0) <- names(ret0) |>
    sprintf(fmt = '%s.cumtrapz')
  
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), ret0)
  ))
  
}

