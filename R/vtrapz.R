
#' @title (Cumulative) Trapezoidal Integration Divided by \eqn{x}-Domain
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
#' @examples
#' (x = seq.int(from = 10L, to = 20L, by = 2L))
#' set.seed(12); (y = rnorm(n = length(x), mean = 1, sd = .2))
#' plot(x, y, type = 'b')
#' 
#' pracma::trapz(x, y)
#' vtrapz(x, y)
#' 
#' pracma::cumtrapz(x, y)
#' cumvtrapz(x, y)
#' 
#' visualize_vtrapz(x, y) + ggplot2::theme_minimal()
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
#' @param x,y \link[base]{numeric} \link[base]{vector}s
#' 
#' @param draw.v \link[base]{logical} scalar, whether to plot [vtrapz()], default `TRUE`
#' 
#' @param draw.cumv \link[base]{logical} scalar, whether to plot [cumvtrapz()], default `TRUE`
#' 
#' @param draw.rect \link[base]{logical} scalar, whether to plot the rectangle, default `TRUE`
#' 
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_path geom_rect scale_x_continuous ylim
#' @importFrom geomtextpath geom_textpath
#' @importFrom stats median.default
#' @export
visualize_vtrapz <- function(
    x, y,
    draw.v = TRUE,
    draw.cumv = TRUE,
    draw.rect = TRUE
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
   (if (length(x) <= 10L) scale_x_continuous(breaks = x, limits = x_lim)) + 
   ylim(0, max(y)*1.1)
  
}


#' @title Visualize [vtrapz()] and [cumvtrapz()] of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @examples
#' spatstat.data::spruces |>
#'  spatstat.explore::Emark() |>
#'  visualize_vtrapz.fv() + ggplot2::theme_minimal()
#'
#' spatstat.data::spruces |>
#'  spatstat.explore::Vmark() |>
#'  visualize_vtrapz.fv() + ggplot2::theme_minimal()
#' 
#' @keywords internal
#' @importFrom ggplot2 labs
#' @export 
visualize_vtrapz.fv <- function(x) {
  visualize_vtrapz(
    x = x$r, y = key1val.fv(x),
    draw.v = FALSE, # no vtrapz()
    draw.rect = FALSE # no rectangle
  ) + 
    labs(x = 'r', y = attr(x, which = 'ylab', exact = TRUE))
}
