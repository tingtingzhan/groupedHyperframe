
# um..
# I may want to bring
# get_nested_factor.data.frame()
# back, and remove @import nlme


#' @importFrom nlme getGroups
getGroups_df_ <- \(object, form, ...) {
  # tzh does not know how to force
  # ?nlme:::getGroups.data.frame
  # to return a data.frame
  
  z <- getGroups( # ?nlme:::getGroups.data.frame
    object = object, form = form, ...
  )
  
  if (!is.data.frame(z)) {
    z <- data.frame(z)
    if (!is.symbol(form[[2L]])) stop('do not allow')
    names(z) <- as.character(form[[2L]])
  }
  
  return(z)
  
}






