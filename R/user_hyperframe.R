

#' @title User Interface of Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' See workhorse function [op_hyperframe()].
#' 
#' @param X a \link[spatstat.geom]{hyperframe}
#' 
#' @param correction \link[base]{character} scalar,
#' see functions 
#' \link[spatstat.explore]{markcorr},
#' \link[spatstat.explore]{Gcross},
#' etc.
#' Default `'none'` to save computing time.
#' 
#' @param ... additional parameters of user operation
#' 
#' @details
#' See explanations in workhorse function [op_hyperframe()].
#' 
#' @returns 
#' See explanations in workhorse function [op_hyperframe()].
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' # no good example for [Emark_.hyperframe]
#' # no hyperframe with ppp-hypercolumn with numeric marks
#' 
#' flu$pattern[] = flu$pattern |> 
#'  lapply(FUN = `mark_name<-`, value = 'stain') # read ?flu carefully
#'  
#' r = seq.int(from = 0, to = 100, by = 5)
#' flu |>
#'  subset(stain == 'M2-M1') |>
#'  Gcross_(i = 'M1', j = 'M2', r = r, correction = 'best', mc.cores = 1L)
#'  
#' flu |>
#'  subset(stain == 'M2-M1') |>
#'  nncross_(i = 'M1', j = 'M2', mc.cores = 1L)
#' @keywords internal
#' @name user_hyperframe
#' @importFrom spatstat.explore Emark
#' @export
Emark_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = Emark, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Vmark
#' @export
Vmark_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = Vmark, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore markcorr
#' @export
markcorr_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = markcorr, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore markvario
#' @export
markvario_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = markvario, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Gcross
#' @export
Gcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = Gcross, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Jcross
#' @export
Jcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = Jcross, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Kcross
#' @export
Kcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = Kcross, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Lcross
#' @export
Lcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppplist, fn = Lcross, correction = correction, ...)

# Inside \link[spatstat.explore]{Gcross} and \link[spatstat.explore]{Kcross}
# @param i type of the points *from* which distances are measured,
# i.e., `X` (or \emph{of}) in \link[spatstat.geom]{nncross}.
# @param j type of the points *to* which distances are measured,
# i.e., `Y` (or \emph{in}) in \link[spatstat.geom]{nncross}.


#' @rdname user_hyperframe
#' @export
nncross_ <- function(X, ...) X |> op_hyperframe(op = dist_ppplist, fn = .nncross, ...)



