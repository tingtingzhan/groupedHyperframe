


#' @title On Eligible \link[spatstat.geom]{marks}
#' 
#' @param X see **Usage**
#' 
#' @param ... additional parameters of user operation
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#'  
#' @keywords internal
#' @name elig_marks
#' @export
Emark_ <- function(X, ...) UseMethod(generic = 'Emark_')

#' @rdname elig_marks
#' @importFrom spatstat.explore Emark
#' @export
Emark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Emark, ...)
}






#' @rdname elig_marks
#' @export
Vmark_ <- function(X, ...) UseMethod(generic = 'Vmark_')

#' @rdname elig_marks
#' @importFrom spatstat.explore Vmark
#' @export
Vmark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Vmark, ...)
}



#' @rdname elig_marks
#' @export
Kmark_ <- function(X, ...) UseMethod(generic = 'Kmark_')

#' @rdname elig_marks
#' @importFrom spatstat.explore Kmark
#' @export
Kmark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Kmark, ...)
}



#' @rdname elig_marks
#' @export
markcorr_ <- function(X, ...) UseMethod(generic = 'markcorr_')


#' @rdname elig_marks
#' @importFrom spatstat.explore markcorr
#' @export
markcorr_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = markcorr, ...)
}




#' @rdname elig_marks
#' @export
markvario_ <- function(X, ...) UseMethod(generic = 'markvario_')

#' @rdname elig_marks
#' @importFrom spatstat.explore markvario
#' @export
markvario_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = markvario, ...)
}



#' @rdname elig_marks
#' @export
Gcross_ <- function(X, ...) UseMethod(generic = 'Gcross_')

#' @rdname elig_marks
#' @importFrom spatstat.explore Gcross
#' @export
Gcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Gcross, ...)
}


#' @rdname elig_marks
#' @export
Jcross_ <- function(X, ...) UseMethod(generic = 'Jcross_')

#' @rdname elig_marks
#' @importFrom spatstat.explore Jcross
#' @export
Jcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Jcross, ...)
}




#' @rdname elig_marks
#' @export
Kcross_ <- function(X, ...) UseMethod(generic = 'Kcross_')


#' @rdname elig_marks
#' @importFrom spatstat.explore Kcross
#' @export
Kcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Kcross, ...)
}


#' @rdname elig_marks
#' @export
Lcross_ <- function(X, ...) UseMethod(generic = 'Lcross_')

#' @rdname elig_marks
#' @importFrom spatstat.explore Lcross
#' @export
Lcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Lcross, ...)
}




#' @rdname elig_marks
#' @export
markconnect_ <- function(X, ...) UseMethod(generic = 'markconnect_')

#' @rdname elig_marks
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


#' @rdname elig_marks
#' @export
nncross_ <- function(X, ...) UseMethod(generic = 'nncross_')


#' @rdname elig_marks
#' @export
nncross_.ppp <- function(X, ...) {
  X |> 
    ppp2dist(fun = .nncross, ...)
}




