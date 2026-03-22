
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
ppp2dist <- \(x, fun, unlist1 = TRUE, ...) {
  
  x. <- unstack.ppp(x)
  n <- length(x.)
  if (n == 1L && !unlist1 && !length(names(x.))) {
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
  
  z <- ret[id]
  
  if (n == 1L && unlist1) return(z[[1L]]) # to mimic spatstat.explore::Emark, etc.
  return(z)

}





#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
ppp_numeric2fv <- \(x, fun, unlist1 = TRUE, ...) {
  
  x. <- unstack.ppp(x)
  n <- length(x.) 
  if (n == 1L && !unlist1 && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  num <- x |>
    is.numeric.ppp()
  if (!any(num)) return(invisible())
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  z <- x.[num] |> 
    anylapply(FUN = fun, ...)
  
  if (n == 1L && unlist1) return(z[[1L]]) # to mimic spatstat.explore::Emark, etc.
  return(z)
  
}






#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
ppp_multitype2fv <- \(x, fun, unlist1 = TRUE, ...) {
  
  x. <- unstack.ppp(x)
  n <- length(x.)
  if (n == 1L && !unlist1 && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- x. |> 
    vapply(FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  z <- x.[mtp] |> 
    anylapply(FUN = fun, ...)
  
  if (n == 1L && unlist1) return(z[[1L]]) # to mimic spatstat.explore::Emark, etc.
  return(z)
  
}


if (FALSE) {
  s = wrobel_lung |>
    grouped_ppp(formula = hladr ~ OS + gender + age | patient_id/image_id, data = _, coords = ~ x + y)
  r = seq.int(from = 0, to = 250, by = 10)
  out = s |>
    within(expr = {
      hladr.E = ppp. |> 
        Emark_(r = r, correction = 'none') |>
        .disrecommend2theo()
    })
  out
  
  
}
 
