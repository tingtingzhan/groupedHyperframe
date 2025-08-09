
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
#' \donttest{
#' library(ggplot2)
#' library(geomtextpath)
#' ggplot() + 
#'  geom_path(mapping = aes(x = x, y = y)) +
#'  geom_rect(mapping = aes(xmin = min(x), xmax = max(x), ymin = 0, ymax = vtrapz(x, y)), alpha = .1) +
#'  geom_textpath(
#'   mapping = aes(x = x, y = vtrapz(x, y), label = 'Average Height of Trapezoidal Integration'),
#'   text_only = TRUE, colour = 'red', fontface = 'bold'
#'  ) +
#'  scale_x_continuous(breaks = x, limits = c(9, 21)) + 
#'  ylim(c(0, 1.5)) + 
#'  theme_bw()
#' }
#' 
#' @keywords internal
#' @name vtrapz
#' @importFrom pracma trapz
#' @export
vtrapz <- function(x, ...) {
  trapz(x, ...) / (max(x) - min(x))
}


#' @rdname vtrapz
#' @importFrom pracma cumtrapz
#' @export
cumvtrapz <- function(x, ...) {
  cumtrapz(x, ...) / (x - min(x))
}



