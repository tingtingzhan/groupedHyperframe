


#' @title [unmark.ppplist()]
#' 
#' @param X a `'ppplist'`
#' 
#' @details
#' Function [unmark.ppplist()] is a iteration of \link[spatstat.geom]{unmark.ppp},
#' but not as yet \link[spatstat.geom]{unmark.splitppp}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom unmark unmark.ppp solapply
#' @export unmark.ppplist
#' @export
unmark.ppplist <- function(X) {
  X |>
    solapply(FUN = unmark.ppp)
}