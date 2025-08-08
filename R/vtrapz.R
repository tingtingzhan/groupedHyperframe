
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
#' (x = seq.int(from = 0L, to = 10L, by = 2L))
#' set.seed(12); (y = rnorm(n = length(x)))
#' plot(x, y, type = 'b')
#' 
#' pracma::trapz(x, y)
#' vtrapz(x, y)
#' 
#' pracma::cumtrapz(x, y)
#' cumvtrapz(x, y)
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



