
# um..
# I may want to bring
# get_nested_factor.data.frame()
# back, and remove @import nlme

# The `S3` generic function [get_nested_factor()] returns 
# a \link[base]{list} of \link[base]{factor}s, 
# ready to be passed into the parameter `by` of the function
# \link[stats]{aggregate.data.frame}.
# 

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






if (FALSE) {
  
  x1 = nlme::Wafer |>
    nlme:::getGroups.data.frame(form = ~Wafer/Site)
  x2 = nlme::Wafer |> 
    nlme::getGroups()
  stopifnot(identical(x1, x2))
  
  nlme::Wafer |>
    nlme::getGroups() |>
    head()
  
}







