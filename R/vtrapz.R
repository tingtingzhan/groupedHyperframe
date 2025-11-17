


#' @title Average Vertical Height of Trapezoidal Integration
#' 
#' @description
#' Trapezoidal integration divided by \eqn{x}-domain.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... additional parameters of function \link[pracma]{trapz}
#' 
#' @note
#' This is a tentative thought: the prefix `v` stands for 'vertical'.
#' 
#' @returns 
#' Function [vtrapz()] return a \link[base]{numeric} scalar.
#' 
#' @keywords internal
#' @importFrom pracma trapz
#' @export
vtrapz <- function(x, ...) {
  if (!is.vector(x, mode = 'numeric')) stop('`x` must be double numeric')
  if (anyDuplicated(x)) stop('`x` must not have duplicates')
  if (is.unsorted(x)) stop('`x` must be sorted')
  trapz(x, ...) / (max(x) - min(x))
}







#' @title Cumulative Average Vertical Height of Trapezoidal Integration
#' 
#' @description
#' Cumulative trapezoidal integration divided by \eqn{x}-domain.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param y see function \link[pracma]{cumtrapz}
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @param rm1 \link[base]{logical} scalar, whether to remove the first `NaN`-value from
#' function [cumvtrapz()] return, default `TRUE`
#' 
#' @param ... additional parameters of function \link[pracma]{trapz} and \link[pracma]{cumtrapz}
#' 
#' @note
#' This is a tentative thought: the prefix `v` stands for 'vertical'.
#' 
#' @returns 
#' Function `vtrapz.*()` return a \link[base]{numeric} scalar.
#' 
#' @keywords internal
#' @name cumvtrapz
#' @export
cumvtrapz <- function(x, ...) UseMethod(generic = 'cumvtrapz')




#' @rdname cumvtrapz
#' @importFrom pracma cumtrapz
#' @export cumvtrapz.default
#' @export
cumvtrapz.default <- function(x, y, ..., rm1 = TRUE) {
  if (!is.vector(x, mode = 'numeric')) stop('`x` must be double numeric')
  if (anyDuplicated(x)) stop('`x` must not have duplicates')
  if (is.unsorted(x)) stop('`x` must be sorted')
  if (length(x) == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  z <- cumtrapz(x, y) / (x - min(x)) # always a 'matrix', even if ncol-1L 
  attr(z, which = 'x') <- x
  attr(z, which = 'method') <- 'pracma::trapz'
  class(z) <- c('cumv', class(z)) |> 
    unique.default()
  
  # a trapz needs two points; therefore `[-1L]` by default
  if (rm1) return(z[-1L]) # `[.cumv`

  return(z)
}





#' @export
`[.cumv` <- function(x, i) {
  z <- unclass(x)[i, , drop = FALSE] # 'matrix'
  attr(z, which = 'x') <- attr(x, which = 'x', exact = TRUE)[i]
  attr(z, which = 'method') <- attr(x, which = 'method', exact = TRUE)
  class(z) <- class(x)
  return(z)
}


#' @export
print.cumv <- function(x, ...) {
  x0 <- unclass(x)
  attributes(x0)[c('x', 'method')] <- NULL
  print.default(x0)
}


#' @rdname cumvtrapz
#' @importFrom spatstat.explore fvnames
#' @export cumvtrapz.fv
#' @export 
cumvtrapz.fv <- function(
    x, 
    key = fvnames(x, a = '.y'), 
    .x = fvnames(x, a = '.x'),
    ...
) {
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  cumvtrapz.default(
    x = x[[.x]], 
    y = c(x[[key]]), # drop attributes since \pkg{spatstat.explore} v3.5.3.9
    ...)
}



#' @rdname cumvtrapz
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is the return of function \link[parallel]{detectCores}.
#' 
#' @keywords internal
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel mclapply makeCluster stopCluster
#' @export cumvtrapz.fvlist
#' @export
cumvtrapz.fvlist <- function(
    x, 
    mc.cores = getOption('cores'), 
    ...
) {
  
  tmp <- x |>
    is.fvlist()
  .y <- tmp |>
    attr(which = '.y', exact = TRUE)
  .x <- tmp |>
    attr(which = '.x', exact = TRUE)
  
  fn <- \(i, ...) {
    z <- cumvtrapz.fv(i, key = .y, .x = .x, ...)
    ret <- c(z)
    names(ret) <- attr(z, which = 'x', exact = TRUE)
    return(ret)
  }
  switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = {
      cumvt <- x |> 
        mclapply(mc.cores = mc.cores, FUN = fn) # `-1L` super important!!!
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
      cumvt <- foreach(i = x, .options.multicore = list(cores = mc.cores)) %dopar% fn(i, ...)
      stopCluster(cl)
    })
  
  return(cumvt)
  
}




#' @rdname cumvtrapz
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
#' @export cumvtrapz.hyperframe
#' @export
cumvtrapz.hyperframe <- function(x, ...) {
  
  if (!any(id <- (unclass(x)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(x)[id]
  
  ret0 <- (as.list.hyperframe(x)[nm]) |>
    lapply(FUN = cumvtrapz.fvlist, ...)
  
  names(ret0) <- names(ret0) |>
    sprintf(fmt = '%s.cumvtrapz')
  
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), ret0)
  ))
  
}










#' @title Visualize [vtrapz()] and [cumvtrapz()]
#' 
#' @description
#' Visualize (cumulative) average vertical height of trapezoidal integration.
#' 
#' @param x see **Usage**
#' 
#' @param y \link[base]{numeric} \link[base]{vector}
#' 
#' @param x_smooth,y_smooth \link[base]{numeric} \link[base]{vector}s,
#' smoothed \eqn{x} and \eqn{y} values, 
#' to beautify the \link[geomtextpath]{geom_textpath} of a \link[stats]{stepfun}
#' 
#' @param xlabs,ylabs \link[base]{character} scalars
#' 
#' @param yname (optional) \link[base]{character} scalar, name of function
#' 
#' @param draw.rect \link[base]{logical} scalar, 
#' whether to plot the rectangle, default `TRUE`
#' 
#' @param draw.v \link[base]{logical} scalar, 
#' whether to plot the average vertical height [vtrapz()], 
#' default is determined by parameter `draw.rect`.
#' 
#' @param draw.cumv \link[base]{logical} scalar, whether to plot the cumulative average vertical height [cumvtrapz()], default `TRUE`
#' 
#' @param label.v,label.cumv \link[base]{character} scalars
#' 
#' @param n \link[base]{integer}, number of \eqn{(x,y)}-values at which to evaluate, 
#' only applicable when the input `inherits` from the `S3` class \link[base]{function},
#' or has an `S3` method of `predict.*()` available.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns
#' The `S3` generic function [visualize_vtrapz()] returns a \link[ggplot2]{ggplot} object.
#' 
#' @keywords internal
#' @name visualize_vtrapz
#' @export
visualize_vtrapz <- function(
    x, y,
    x_smooth, y_smooth,
    xlabs, ylabs, 
    yname,
    draw.rect, draw.v, label.v,
    draw.cumv, label.cumv,
    ...
) { 
  UseMethod(generic = 'visualize_vtrapz')
}

#' @rdname visualize_vtrapz
#' @importFrom ggplot2 ggplot aes geom_path geom_rect scale_x_continuous scale_y_continuous labs
#' @importFrom geomtextpath geom_textpath
#' @importFrom stats median.default
#' @importFrom scales label_number
#' @importFrom utils citation
#' @export visualize_vtrapz.numeric
#' @export
visualize_vtrapz.numeric <- function(
    x, y,
    x_smooth = x, y_smooth = y,
    xlabs, ylabs, 
    yname,
    draw.rect = TRUE, draw.v = draw.rect, label.v = 'Average Vertical Height',
    draw.cumv = TRUE, label.cumv = 'Cumulative Average Vertical Height',
    ...
) {
  
  if (!is.vector(y, mode = 'numeric')) stop('`y` must be double numeric')
  if (length(y) != length(x)) stop('`x` and `y` must have same length')
  if (anyNA(y)) stop('`y` must not have missing value(s)')
  if (any(y < 0)) stop('for visualization, force `y > 0`')
  
  v <- vtrapz(x, y)
  cv <- cumvtrapz.default(x, y, rm1 = TRUE)
  method <- attr(cv, which = 'method', exact = TRUE)
  #label.v <- paste(label.v, method, sep = '; ')
  #label.cumv <- paste(label.cumv, method, sep = '; ')
  
  xmin <- min(x)
  xmax <- max(x)
  xmed <- median.default(x)
  x_lim <- c(xmin - (xmed - xmin) * .2, xmax + (xmax - xmed) * .2)
  y_lim <- if (draw.rect) c(0, max(y)*1.1) else c(min(y)*.95, max(y)*1.05)
  
  doi_pracma <- unclass(citation(package = 'pracma'))[[1L]]$doi
  
  lyr_path <- if (missing(yname) || !length(yname)) {
    geom_path(mapping = aes(x = x, y = y), alpha = .3, linewidth = 1.3)
  } else {
    geom_textpath(
      mapping = aes(x = x_smooth, y = y_smooth, label = yname),
      hjust = .1, alpha = .3, linewidth = 1.3, 
      #colour = 'blue', fontface = 'bold', alpha = .7
    )
  }
  
  lyr_rect <- if (draw.rect) geom_rect(mapping = aes(xmin = min(x), xmax = max(x), ymin = 0, ymax = v), alpha = .1) # else NULL
  
  lyr_v <- if (draw.v) {
    geom_textpath(
      mapping = aes(x = x, y = v, label = label.v),
      hjust = .1, text_only = TRUE, colour = 'red', fontface = 'bold', alpha = .7
    )
  } # else NULL
  
  lyr_cumv <- if (draw.cumv) {
    geom_textpath(
      mapping = aes(
        x = attr(cv, which = 'x', exact = TRUE), 
        y = c(cv), 
        label = label.cumv
      ),
      colour = 'blue', fontface = 'bold', alpha = .7
    )
  } # else NULL
  
  lyr_x <- if (length(x) <= 10L) {
    if (missing(xlabs) || !length(xlabs)) {
      scale_x_continuous(breaks = x, labels = label_number(accuracy = .1), limits = x_lim)
    } else scale_x_continuous(breaks = x, labels = xlabs, limits = x_lim)
  } else {
    if (missing(xlabs) || !length(xlabs)) {
      # do nothing
    } else scale_x_continuous(labels = xlabs)
  }
  
  lyr_y <- if (missing(ylabs) || !length(ylabs)) {
    # do nothing
  } else scale_y_continuous(labels = ylabs)
    
  ggplot() + 
    lyr_path +
    lyr_rect +
    lyr_v +
    lyr_cumv +
    lyr_x + 
    lyr_y +
    #labs(caption = doi_pracma |> sprintf(fmt = 'pracma::trapz() via doi:%s'))
    labs(caption = method)
  
}


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @importFrom spatstat.explore fvnames
#' @importFrom scales label_percent
#' @export visualize_vtrapz.fv
#' @export 
visualize_vtrapz.fv <- function(x, ...) {
  # ?spatstat.explore::plot.roc uses workhorse ?spatstat.explore::plot.fv
  fv <- x; x <- NULL # make code more readable
  .x <- fvnames(fv, a = '.x')
  .y <- fvnames(fv, a = '.y')
  x <- fv[[.x]]
  y <- c(fv[[.y]]) # drop attributes since \pkg{spatstat.explore} v3.5.3.9
  
  is_step <- inherits(fv, what = 'roc')
  if (is_step) {
    l <- lowess(x = x, y = y, f = min(1, 50/nrow(fv))) # read ?stats::lowess carefully for parameter `f`
  }

  yname <- fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1()
  is_roc <- inherits(fv, what = 'roc')
  
  visualize_vtrapz.numeric(
    x = x, y = y,
    x_smooth = if (is_step) l$x else x, 
    y_smooth = if (is_step) l$y else y, 
    xlabs = if (is_roc) label_percent(), #else NULL
    ylabs = if (is_roc) label_percent(), #else NULL
    yname = if (is_step) {
      yname |> sprintf(fmt = '%s (smoothed)') 
    } else yname,
    ...
  ) + 
    labs(x = .x, y = NULL)
}





#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @importFrom spatstat.explore fvnames
#' @export visualize_vtrapz.fvlist
#' @export 
visualize_vtrapz.fvlist <- function(x, ...) {
  x |>
    as.fvlist() |>
    lapply(FUN = visualize_vtrapz.fv, ...) |>
    Reduce(f = '+', x = _) # requires patchwork!!
}






#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @export visualize_vtrapz.density
#' @export
visualize_vtrapz.density <- function(x, ...) {
  visualize_vtrapz.numeric(
    x = x$x, 
    y = x$y, 
    yname = 'stats::density',
    ...
  ) + 
    labs(x = 'x', y = NULL)
}


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @importFrom stats lowess
#' @export visualize_vtrapz.ecdf
#' @export
visualize_vtrapz.ecdf <- function(x, ...) {
  fn <- x; x <- NULL # make code more readable
  ev <- environment(fn)
  # stopifnot(stats::is.stepfun(fn)) # !!!
  x <- get('x', envir = ev)
  y <- get('y', envir = ev)
  l <- lowess(x = x, y = y, f = min(1, 5/length(x))) # read ?stats::lowess carefully for parameter `f`
  visualize_vtrapz.numeric(
    x = x, y = y,
    x_smooth = l$x, y_smooth = l$y,
    yname = 'stats::ecdf (smoothed)',
    ...
  ) +
    labs(x = 'x', y = NULL)
}
  


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs geom_point aes
#' @export visualize_vtrapz.function
#' @export
visualize_vtrapz.function <- function(x, ..., n = 513L) {
  fn <- x; x <- NULL # make code more readable
  ev <- environment(fn)

  yname <- if (exists('yname', envir = ev)) {
    get('yname', envir = ev)
  } # else NULL
  xname <- if (exists('xname', envir = ev)) {
    get('xname', envir = ev)
  } # else NULL
  
  if (exists('x', envir = ev, inherits = FALSE)) { # returned from ?stats::approxfun
    fn_from <- 'approxfun'
    x0 <- get('x', envir = ev)
    y0 <- get('y', envir = ev)
  } else if (exists('z', envir = ev, inherits = FALSE)) { # returned from ?stats::splinefun
    fn_from <- 'splinefun'
    z <- get('z', envir = ev)
    x0 <- z$x
    y0 <- z$y
  } else stop('not supported')
  
  x <- x0 |>
    range() |>
    as.list() |>
    c(list(length.out = n)) |>
    do.call(what = seq.int, args = _)
  y <- fn(x)
  
  visualize_vtrapz.numeric(
    x = x, y = y,
    yname = yname,
    ...
  ) +
    switch(fn_from, splinefun =, approxfun = {
      geom_point(mapping = aes(x = x0, y = y0), color = 'firebrick')
    }) +
    labs(x = xname, y = NULL)
  
}




#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs geom_point aes
#' @importFrom stats predict
#' @export visualize_vtrapz.loess
#' @export
visualize_vtrapz.loess <- function(x, ..., n = 513L) {

  obj <- x; x <- NULL # make code more readable
  if (!is.matrix(obj$x)) stop('stats-package updated?')
  if (ncol(obj$x) != 1L) stop('one-and-only-one predictor in loess model!')
  
  xname <- colnames(obj$x)[1L]
  
  newx <- obj$x[, 1L] |>
    range() |>
    c(length.out = n) |>
    as.list() |>
    do.call(what = seq.int)
  newdata <- data.frame(newx)
  names(newdata) <- xname
  newy <- obj |>
    predict(newdata = newdata, se = FALSE) # ?stats:::predict.loess
  
  visualize_vtrapz.numeric(
    x = newx, y = newy, 
    yname = obj$yname %||% 'stats::loess',
    ...
  ) +
    geom_point(mapping = aes(x = obj$x[, 1L], y = obj$y), alpha = .1) +
    labs(x = xname, y = NULL)
  
}



