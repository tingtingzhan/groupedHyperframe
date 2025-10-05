
#' @title (Cumulative) Average Vertical Height of Trapezoidal Integration
#' 
#' @description
#' (Cumulative) trapezoidal integration divided by \eqn{x}-domain.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param key \link[base]{character} scalar, see function [keyval.fv()]
#' 
#' @param ... additional parameters of function \link[pracma]{trapz} and \link[pracma]{cumtrapz}
#' 
#' @note
#' This is a tentative thought: the prefix `v` stands for 'vertical'.
#' 
#' @keywords internal
#' @name vtrapz
#' @export
vtrapz <- function(x, ...) UseMethod(generic = 'vtrapz')

#' @rdname vtrapz
#' @export
cumvtrapz <- function(x, ...) UseMethod(generic = 'cumvtrapz')


#' @rdname vtrapz
#' @importFrom pracma trapz
#' @export vtrapz.default
#' @export
vtrapz.default <- function(x, ...) {
  if (!is.vector(x, mode = 'numeric')) stop('`x` must be double numeric')
  if (anyDuplicated(x)) stop('`x` must not have duplicates')
  if (is.unsorted(x)) stop('`x` must be sorted')
  trapz(x, ...) / (max(x) - min(x))
}

#' @rdname vtrapz
#' @importFrom spatstat.explore fvnames
#' @export vtrapz.fv
#' @export
vtrapz.fv <- function(x, key = fvnames(x, a = '.y'), ...) {
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  vtrapz.default(x = x[[.x]], y = x[[key]]) |>
    unname()
}


#' @rdname vtrapz
#' @importFrom pracma cumtrapz
#' @export cumvtrapz.default
#' @export
cumvtrapz.default <- function(x, ...) {
  if (!is.vector(x, mode = 'numeric')) stop('`x` must be double numeric')
  if (anyDuplicated(x)) stop('`x` must not have duplicates')
  if (is.unsorted(x)) stop('`x` must be sorted')
  cumtrapz(x, ...) / (x - min(x))
}

#' @rdname vtrapz
#' @importFrom spatstat.explore fvnames
#' @export cumvtrapz.fv
#' @export 
cumvtrapz.fv <- function(x, key = fvnames(x, a = '.y'), ...) {
  
  .x <- fvnames(x, a = '.x')
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  
  n <- length(x[[.x]])
  if (n == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs
  
  ret0 <- cumvtrapz.default(x = x[[.x]], y = x[[key]])
  # a trapz needs two points; therefore `[-1L]`
  ret <- c(ret0[-1L])
  names(ret) <- x[[.x]][-1L]
  return(ret)
  
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
    yname,
    draw.rect, draw.v, label.v,
    draw.cumv, label.cumv,
    ...
) { 
  UseMethod(generic = 'visualize_vtrapz')
}

#' @rdname visualize_vtrapz
#' @importFrom ggplot2 ggplot aes geom_path geom_rect scale_x_continuous ylim labs
#' @importFrom geomtextpath geom_textpath
#' @importFrom stats median.default
#' @importFrom scales label_number
#' @importFrom utils citation
#' @export visualize_vtrapz.numeric
#' @export
visualize_vtrapz.numeric <- function(
    x, y,
    x_smooth = x, y_smooth = y,
    yname,
    draw.rect = TRUE, draw.v = draw.rect, label.v = 'Average Vertical Height',
    draw.cumv = TRUE, label.cumv = 'Cumulative Average Vertical Height',
    ...
) {
  
  if (!is.vector(y, mode = 'numeric')) stop('`y` must be double numeric')
  if (length(y) != length(x)) stop('`x` and `y` must have same length')
  if (anyNA(y)) stop('`y` must not have missing value(s)')
  if (any(y < 0)) stop('for visualization, force `y > 0`')
  
  v <- vtrapz.default(x, y)
  cv <- cumvtrapz.default(x, y)
  
  xmin <- min(x)
  xmax <- max(x)
  xmed <- median.default(x)
  x_lim <- c(xmin - (xmed - xmin) * .2, xmax + (xmax - xmed) * .2)
  
  doi_pracma <- unclass(citation(package = 'pracma'))[[1L]]$doi
  
  ggplot() + 
    (if (missing(yname) || !length(yname)) {
      geom_path(mapping = aes(x = x, y = y), alpha = .3, linewidth = 1.3)
    } else {
      geom_textpath(
        mapping = aes(x = x_smooth, y = y_smooth, label = yname),
        hjust = .1, alpha = .3, linewidth = 1.3, 
        #colour = 'blue', fontface = 'bold', alpha = .7
      )
    }) +
    (if (draw.rect) geom_rect(mapping = aes(xmin = min(x), xmax = max(x), ymin = 0, ymax = v), alpha = .1)) +
    (if (draw.v) geom_textpath(
      mapping = aes(x = x, y = v, label = label.v),
      hjust = .1, text_only = TRUE, colour = 'red', fontface = 'bold', alpha = .7
    )) +
    (if (draw.cumv) geom_textpath(
      mapping = aes(x = x[-1L], y = cv[-1L], label = label.cumv),
      colour = 'blue', fontface = 'bold', alpha = .7
    )) +
    (if (length(x) <= 10L) scale_x_continuous(breaks = x, labels = label_number(accuracy = .1), limits = x_lim)) + 
    (if (draw.rect) ylim(0, max(y)*1.1) else ylim(min(y)*.95, max(y)*1.05)) +
    labs(caption = doi_pracma |> sprintf(fmt = 'pracma::trapz() via doi:%s'))
  
}


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @importFrom spatstat.explore fvnames
#' @export visualize_vtrapz.fv
#' @export 
visualize_vtrapz.fv <- function(x, ...) {
  # ?spatstat.explore::plot.roc uses workhorse ?spatstat.explore::plot.fv
  fv <- x; x <- NULL # make code more readable
  .x <- fvnames(fv, a = '.x')
  .y <- fvnames(fv, a = '.y')
  x <- fv[[.x]]
  y <- fv[[.y]]
  
  is_step <- inherits(fv, what = 'roc')
  if (is_step) {
    l <- lowess(x = x, y = y, f = min(1, 50/nrow(fv))) # read ?stats::lowess carefully for parameter `f`
  }

  yname <- fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1()
  visualize_vtrapz.numeric(
    x = x, y = y,
    x_smooth = if (is_step) l$x else x, 
    y_smooth = if (is_step) l$y else y, 
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
  


