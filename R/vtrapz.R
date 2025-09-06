
#' @title (Cumulative) Average Vertical Height of Trapezoidal Integration
#' 
#' @description
#' (Cumulative) trapezoidal integration divided by \eqn{x}-domain.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... additional parameters of function \link[pracma]{trapz} and \link[pracma]{cumtrapz}
#' 
#' @note
#' This is a tentative thought: the prefix `v` stands for 'vertical'.
#' 
#' @keywords internal
#' @name vtrapz
#' @importFrom pracma trapz
#' @export
vtrapz <- function(x, ...) {
  if (!is.vector(x, mode = 'numeric')) stop('`x` must be double numeric')
  if (anyDuplicated(x)) stop('`x` must not have duplicates')
  if (is.unsorted(x)) stop('`x` must be sorted')
  trapz(x, ...) / (max(x) - min(x))
}


#' @rdname vtrapz
#' @importFrom pracma cumtrapz
#' @export
cumvtrapz <- function(x, ...) {
  if (!is.vector(x, mode = 'numeric')) stop('`x` must be double numeric')
  if (anyDuplicated(x)) stop('`x` must not have duplicates')
  if (is.unsorted(x)) stop('`x` must be sorted')
  cumtrapz(x, ...) / (x - min(x))
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
#' @param draw.v \link[base]{logical} scalar, whether to plot the average vertical height [vtrapz()], default `TRUE`
#' 
#' @param draw.cumv \link[base]{logical} scalar, whether to plot the cumulative average vertical height [cumvtrapz()], default `TRUE`
#' 
#' @param draw.rect \link[base]{logical} scalar, whether to plot the rectangle, default `TRUE`
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns
#' The `S3` generic function [visualize_vtrapz()] returns a \link[ggplot2]{ggplot} object.
#' 
#' @keywords internal
#' @name visualize_vtrapz
#' @export
visualize_vtrapz <- function(x, ...) UseMethod(generic = 'visualize_vtrapz')


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 ggplot aes geom_path geom_rect scale_x_continuous ylim
#' @importFrom geomtextpath geom_textpath
#' @importFrom stats median.default
#' @importFrom scales label_number
#' @export visualize_vtrapz.numeric
#' @export
visualize_vtrapz.numeric <- function(
    x, y,
    draw.v = TRUE,
    draw.cumv = TRUE,
    draw.rect = TRUE, 
    ...
) {
  
  if (!is.vector(y, mode = 'numeric')) stop('`y` must be double numeric')
  if (length(y) != length(x)) stop('`x` and `y` must have same length')
  if (anyNA(y)) stop('`y` must not have missing value(s)')
  if (any(y < 0)) stop('for visualization, force `y > 0`')
  
  v <- vtrapz(x, y)
  cv <- cumvtrapz(x, y)
  
  xmin <- min(x)
  xmax <- max(x)
  xmed <- median.default(x)
  x_lim <- c(xmin - (xmed - xmin) * .2, xmax + (xmax - xmed) * .2)
  
  ggplot() + 
   geom_path(mapping = aes(x = x, y = y), alpha = .3, linewidth = 1.3) +
   (if (draw.rect) geom_rect(mapping = aes(xmin = min(x), xmax = max(x), ymin = 0, ymax = v), alpha = .1)) +
   (if (draw.v) geom_textpath(
    mapping = aes(x = x, y = v, label = 'Average Vertical Height'),
    hjust = .1, text_only = TRUE, colour = 'red', fontface = 'bold', alpha = .7
   )) +
   (if (draw.cumv) geom_textpath(
    mapping = aes(x = x[-1L], y = cv[-1L], label = 'Cumulative Average Vertical Height'),
    colour = 'blue', fontface = 'bold', alpha = .7
   )) +
   (if (length(x) <= 10L) scale_x_continuous(breaks = x, labels = label_number(accuracy = .1), limits = x_lim)) + 
   (if (draw.rect) ylim(0, max(y)*1.1) else ylim(min(y)*.95, max(y)*1.05))
  
}


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @export visualize_vtrapz.fv
#' @export 
visualize_vtrapz.fv <- function(x, ...) {
  visualize_vtrapz.numeric(x = x$r, y = keyval.fv(x), ...) + 
    labs(x = 'r', y = attr(x, which = 'ylab', exact = TRUE))
}


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @export visualize_vtrapz.roc
#' @export 
visualize_vtrapz.roc <- function(x, ...) {
  # c('roc', 'fv', 'data.frame')
  # ?spatstat.explore::plot.roc uses workhorse ?spatstat.explore::plot.fv
  visualize_vtrapz.numeric(x = x$p, y = keyval.fv(x), ...) + 
    labs(x = 'p', y = attr(x, which = 'ylab', exact = TRUE))
}





#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @export visualize_vtrapz.density
#' @export
visualize_vtrapz.density <- function(x, ...) {
  visualize_vtrapz.numeric(x = x$x, y = x$y, ...) + 
    labs(x = 'x', y = 'stats::density')
}


#' @rdname visualize_vtrapz
#' @importFrom ggplot2 labs
#' @export visualize_vtrapz.ecdf
#' @export
visualize_vtrapz.ecdf <- function(x, ...) {
  fn <- x; x <- NULL # make code more readable
  ev <- environment(fn)
  visualize_vtrapz.numeric(
    x = get('x', envir = ev),
    y = get('y', envir = ev),
    ...
  ) +
    labs(x = 'x', y = 'stats::ecdf')
}
  


