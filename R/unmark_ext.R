
#' @title Batch Process of \link[spatstat.geom]{unmark}
#' 
#' @param X see **Usage**
#' 
#' @keywords internal
#' @name unmark_
#' @export
unmark_ <- function(X) UseMethod(generic = 'unmark_')

#' @rdname unmark_
#' 
#' @details
#' Function [unmark_.ppplist()] is a iteration of \link[spatstat.geom]{unmark.ppp}.
#' 
#' Note that Dr. Baddeley has told me that \link[spatstat.geom]{unmark.splitppp} is a bandage fix.
#' 
#' @importFrom spatstat.geom unmark unmark.ppp solapply
#' @export unmark_.ppplist
#' @export
unmark_.ppplist <- function(X) {
  X |>
    solapply(FUN = unmark.ppp)
}