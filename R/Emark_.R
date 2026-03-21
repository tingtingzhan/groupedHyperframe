


#' @title Batch Process
#' 
#' @param X see **Usage**
#' 
#' @param ... additional parameters of user operation
#' 
#' @details
#' User Interface of Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' 
#' @examples
#' # in \CRANpkg{spatstat.data}
#' # no good example for [Emark_]
#' # no hyperframe with ppp-hypercolumn with numeric marks
#' fluM = spatstat.data::flu |>
#'  spatstat.geom::subset.hyperframe(subset = (stain == 'M2-M1') & (virustype == 'wt'))
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#'  
#' @keywords internal
#' @name batch
#' @export
Emark_ <- function(X, ...) UseMethod(generic = 'Emark_')

#' @rdname batch
#' @importFrom spatstat.explore Emark
#' @export
Emark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Emark, ...)
}






#' @rdname batch
#' @export
Vmark_ <- function(X, ...) UseMethod(generic = 'Vmark_')

#' @rdname batch
#' @importFrom spatstat.explore Vmark
#' @export
Vmark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Vmark, ...)
}



#' @rdname batch
#' @export
Kmark_ <- function(X, ...) UseMethod(generic = 'Kmark_')

#' @rdname batch
#' @importFrom spatstat.explore Kmark
#' @export
Kmark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Kmark, ...)
}



#' @rdname batch
#' @export
markcorr_ <- function(X, ...) UseMethod(generic = 'markcorr_')


#' @rdname batch
#' @importFrom spatstat.explore markcorr
#' @export
markcorr_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = markcorr, ...)
}




#' @rdname batch
#' @export
markvario_ <- function(X, ...) UseMethod(generic = 'markvario_')

#' @rdname batch
#' @importFrom spatstat.explore markvario
#' @export
markvario_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = markvario, ...)
}



#' @rdname batch
#' @export
Gcross_ <- function(X, ...) UseMethod(generic = 'Gcross_')

#' @rdname batch
#' @importFrom spatstat.explore Gcross
#' @export
Gcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Gcross, ...)
}


#' @rdname batch
#' @export
Jcross_ <- function(X, ...) UseMethod(generic = 'Jcross_')

#' @rdname batch
#' @importFrom spatstat.explore Jcross
#' @export
Jcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Jcross, ...)
}




#' @rdname batch
#' @export
Kcross_ <- function(X, ...) UseMethod(generic = 'Kcross_')


#' @rdname batch
#' @importFrom spatstat.explore Kcross
#' @export
Kcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Kcross, ...)
}


#' @rdname batch
#' @export
Lcross_ <- function(X, ...) UseMethod(generic = 'Lcross_')

#' @rdname batch
#' @importFrom spatstat.explore Lcross
#' @export
Lcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Lcross, ...)
}




#' @rdname batch
#' @export
markconnect_ <- function(X, ...) UseMethod(generic = 'markconnect_')

#' @rdname batch
#' @importFrom spatstat.explore markconnect
#' @export
markconnect_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = markconnect, ...)
}


# Inside \link[spatstat.explore]{Gcross} and \link[spatstat.explore]{Kcross}
# @param i type of the points *from* which distances are measured,
# i.e., `X` (or \emph{of}) in \link[spatstat.geom]{nncross}.
# @param j type of the points *to* which distances are measured,
# i.e., `Y` (or \emph{in}) in \link[spatstat.geom]{nncross}.


#' @rdname batch
#' @export
nncross_ <- function(X, ...) UseMethod(generic = 'nncross_')


#' @rdname batch
#' @export
nncross_.ppp <- function(X, ...) {
  X |> 
    ppp2dist(fun = .nncross, ...)
}




