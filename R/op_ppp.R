
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
ppp2dist <- \(x, fun, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- vapply(x., FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  fn_mtp_ <- c(
    nncross = '.nncross'
  )
  fn_mtp <- lapply(fn_mtp_, FUN = get)
  id_mtp <- vapply(fn_mtp, FUN = identical, x = fun, FUN.VALUE = NA)
  
  if (!any(id_mtp)) stop('fun not supported')

  ret <- x.[mtp] |> 
    anylapply(FUN = fun, ...)
  #names(ret) <- paste(names(ret), names(fn_mtp_)[id_mtp], sep = '.')
  
  id <- (lengths(ret) > 0L)
  if (!any(id)) return(invisible())
  
  return(ret[id])

}





#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
ppp_numeric2fv <- \(x, fun, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  num <- x |>
    is.numeric.ppp()
  if (!any(num)) return(invisible())
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  x.[num] |> 
    anylapply(FUN = fun, ...)
  
}






#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
ppp_multitype2fv <- \(x, fun, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- x. |> 
    vapply(FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  x.[mtp] |> 
    anylapply(FUN = fun, ...)
  
}




