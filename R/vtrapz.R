
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
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_path geom_rect scale_x_continuous ylim
#' @importFrom geomtextpath geom_textpath
#' @importFrom stats median.default
#' @export
visualize_vtrapz <- function(x, y) {
  
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
   geom_path(mapping = aes(x = x, y = y), alpha = .5) +
   geom_rect(mapping = aes(xmin = min(x), xmax = max(x), ymin = 0, ymax = v), alpha = .1) +
   geom_textpath(
    mapping = aes(x = x, y = v, label = 'Average Height'),
    hjust = .1, text_only = TRUE, colour = 'red', fontface = 'bold'
   ) +
   geom_textpath(
    mapping = aes(x = x[-1L], y = cv[-1L], label = 'Cumulative Average Height'),
    colour = 'blue', fontface = 'bold'
   ) +
   scale_x_continuous(breaks = x, limits = x_lim) + 
   ylim(0, max(y)*1.1)
  
}
