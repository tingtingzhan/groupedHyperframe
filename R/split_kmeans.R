
#' @title \link[base]{split} by \link[stats]{kmeans} Clustering
#' 
#' @description
#' \link[base]{split} by \link[stats]{kmeans} clustering
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of function \link[stats]{kmeans}
#' 
#' @keywords internal
#' @name split_kmeans
#' @export
split_kmeans <- function(x, ...) UseMethod(generic = 'split_kmeans')



#' @rdname split_kmeans
#' 
#' @note
#' Function [split_kmeans.default()] is supposed to work with
#' \link[spatstat.geom]{ppp.object}.
#' 
#' @importFrom spatstat.geom split.ppp
#' @export split_kmeans.default
#' @export
split_kmeans.default <- function(x, ...) {
  
  # written with S3 generics on purpose!!
  km <- x |> .kmeans(...)
  
  cls <- km[['cluster']]
  attr(cls, which = 'levels') <- cls |> max() |> seq_len() |> as.character()
  class(cls) <- 'factor'
  
  x |> 
    split(f = cls, drop = FALSE)
  
}

#' @rdname split_kmeans
#' @export split_kmeans.listof
#' @export
split_kmeans.listof <- function(x, ...) {
  
  tmp <- x |>
    lapply(FUN = split_kmeans.default, ...)
  
  sq <- x |>
    seq_along()
  
  ns <- tmp |> 
    lengths(use.names = FALSE)
  
  ret <- tmp |> 
    do.call(what = c)
  attr(ret, which = 'id') <- rep(sq, times = ns)
  attr(ret, which = 'cluster') <- ns |> lapply(FUN = seq_len) |> unlist(use.names = FALSE)
  return(ret)
  
}


#' @rdname split_kmeans
#' @importFrom spatstat.geom is.ppp hyperframe cbind.hyperframe
#' @export split_kmeans.hyperframe
#' @export
split_kmeans.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumn
  x. <- unclass(x)$df
  
  hc_ppp <- hc |>
    vapply(FUN = \(x) {
      x |>
        vapply(FUN = is.ppp, FUN.VALUE = NA) |>
        all()
    }, FUN.VALUE = NA) |>
    which()
  n_ppp <- length(hc_ppp)
  
  if (!n_ppp) {
    # do nothing
  } else if (n_ppp == 1L) {
    
    tmp <- (hc[[hc_ppp]]) |>
      split_kmeans.listof(...)
    
    id <- tmp |> attr(which = 'id', exact = TRUE)
    
    ret <- hyperframe(
      tmp,
      .id = id,
      .cluster = tmp |> attr(which = 'cluster', exact = TRUE)
    ) |>
      cbind.hyperframe(x.[id, , drop = FALSE])
    
    names(ret)[1L] <- names(hc_ppp)
    
    if (inherits(x, what = 'groupedHyperframe')) {
      # haven't tested, but should be correct; very simple anyway!
      grp <- x |> 
        attr(which = 'group', exact = TRUE)
      
      # `.id` should be equivalent to the existing lowest cluster!!!
      ret$.id <- NULL
      
      grp[[2L]] <- call('/', grp[[2L]], quote(.cluster))
      attr(ret, which = 'group') <- grp
    } else attr(ret, which = 'group') <- '~ .id/.cluster' |> str2lang()
    
    class(ret) <- c('groupedHyperframe', class(ret)) |> unique.default()
    return(ret)
    
  } else stop('more than one ppp-hypercolumn, ambiguity!')
    
    
}


