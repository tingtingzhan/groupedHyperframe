


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
#' # in \CRANpkg{spatstat.data}
#' # no good example for [Emark_]
#' # no hyperframe with ppp-hypercolumn with numeric marks
#' 
#' library(spatstat.geom)
#' fluM = spatstat.data::flu |>
#'  subset(subset = (stain == 'M2-M1') & (virustype == 'wt'))
#' fluM
#' r = seq.int(from = 0, to = 100, by = 5)
#' fluM |>
#'  Gcross_(i = 'M1', j = 'M2', r = r, mc.cores = 1L)
#' fluM |>
#'  nncross_(i = 'M1', j = 'M2', mc.cores = 1L)
#' @keywords internal
#' @name user_hyperframe
#' @importFrom spatstat.explore Emark
#' @export
Emark_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Emark, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Vmark
#' @export
Vmark_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Vmark, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Kmark
#' @export
Kmark_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Kmark, correction = correction, ...)


#' @rdname user_hyperframe
#' @importFrom spatstat.explore markcorr
#' @export
markcorr_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = markcorr, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore markvario
#' @export
markvario_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = markvario, correction = correction, ...)



#' @rdname user_hyperframe
#' @importFrom spatstat.explore Gcross
#' @export
Gcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Gcross, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Jcross
#' @export
Jcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Jcross, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Kcross
#' @export
Kcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Kcross, correction = correction, ...)

#' @rdname user_hyperframe
#' @importFrom spatstat.explore Lcross
#' @export
Lcross_ <- function(X, correction = 'none', ...) X |> op_hyperframe(op = fv_ppp, fn = Lcross, correction = correction, ...)

# Inside \link[spatstat.explore]{Gcross} and \link[spatstat.explore]{Kcross}
# @param i type of the points *from* which distances are measured,
# i.e., `X` (or \emph{of}) in \link[spatstat.geom]{nncross}.
# @param j type of the points *to* which distances are measured,
# i.e., `Y` (or \emph{in}) in \link[spatstat.geom]{nncross}.


#' @rdname user_hyperframe
#' @export
nncross_ <- function(X, ...) X |> op_hyperframe(op = dist_ppp, fn = .nncross, ...)










#' @title Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' Create hypercolumn(s) of
#' \link[spatstat.explore]{fv.object}s 
#' or 
#' distances
#' from 
#' the one-and-only-one \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' inside a \link[spatstat.geom]{hyperframe}.
#' 
#' @param X a \link[spatstat.geom]{hyperframe}, containing ***one-and-only-one*** \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @param op \link[base]{function}, currently functions [fv_ppp()] or [dist_ppp()] are accepted
#' 
#' @param ... additional parameters of workhorse functions 
#' [fv_ppp()] or [dist_ppp()]
#' 
#' @returns
#' Function [op_hyperframe()] returns a \link[spatstat.geom]{hyperframe} with additional
#' \itemize{
#' 
#' \item \link[spatstat.explore]{fv.object} \link[spatstat.geom:hyperframe]{hypercolumns} if `op = fv_ppp`. 
#' ***One hypercolumn per \link[base]{numeric} mark*** in the \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' \item \link[base]{numeric} \link[spatstat.geom:hyperframe]{hypercolumns} if `op = dist_ppp`.
#' ***One hypercolumn per \link[spatstat.geom]{is.multitype} mark*** 
#' in the \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' }
#' 
#'  
#' @keywords internal
#' @importFrom cli col_red col_blue col_br_magenta style_bold
#' @importFrom spatstat.geom is.ppplist as.list.hyperframe cbind.hyperframe
#' @importFrom utils tail
#' @export
op_hyperframe <- function(X, op, ...) {
  
  id <- vapply(X, FUN = is.ppplist, FUN.VALUE = NA)
  if (sum(id) != 1L) stop('allow one-and-only-one ppp-hypercolumn, which may contain one or more mark(s)')
  
  ret0 <- op_ppplist(x = as.list.hyperframe(X)[[which(id)]], op = op, ...)
  
  # re-organize the list!!
  # `ret0`: 1st subject, 2nd mark
  # `ret1`: 1st mark, 2nd subject
  ret1 <- .mapply(FUN = list, dots = ret0, MoreArgs = NULL)
  names(ret1) <- names(ret0[[1L]])
  
  .mapply(FUN = check_fvlist, dots = list(
    X = ret1, data.name = names(ret1)
  ), MoreArgs = NULL)
  
  ret <- do.call(
    what = cbind.hyperframe, 
    args = c(list(X), ret1)
  )
  
  if (inherits(X, what = 'groupedHyperframe')) {
    attr(ret, which = 'group') <- attr(X, which = 'group', exact = TRUE)
    class(ret) <- c('groupedHyperframe', class(X)) |> unique.default()
  } # a bandage fix, for now
  
  return(ret)
  
}






