

nested_ <- \(g) {
  if (is.symbol(g)) return(g)
  if (g[[1L]] == ':') return(g)
  if (g[[1L]] == '~') {
    if (length(g) == 2L) return(nested_(g[[2L]]))
    stop('only accept one-sided formula')
  }
  if (g[[1L]] == '/') { # recursive!!
    z <- as.list(g)[-1L] |> 
      lapply(FUN = nested_)
    return(z)
  }
  stop('should not come here')
}


#' @title Drop Lowest Nested Level
#' 
#' @param g an R \link[base]{language} object
#' 
#' @examples
#' drop_lowest_nested(~ g1/g2a:g2b/g3a:g3b:g3c)
#' drop_lowest_nested(~ g1)
#' drop_lowest_nested(~ g1:g2)
#' 
#' @keywords internal
#' @export
drop_lowest_nested <- \(g) {
  if (is.symbol(g)) return(invisible()) # dropped!!
  if (g[[1L]] == ':') return(invisible()) # dropped!!
  if (g[[1L]] == '/') return(as.list(g)[[2L]]) # beautiful!!!
  if (g[[1L]] == '~') { # recursive!!
    if (length(g) != 2L) stop('only accept one-sided formula')
    g0 <- g[[2L]] |>
      drop_lowest_nested()
    if (!length(g0)) return(invisible())
    g[[2L]] <- g0
    return(g)
  }
  stop('should not come here')
}



#' @title Get Nested Groups
#' 
#' @param by an R \link[base]{language} object, (nested) grouping structure
#' 
#' @returns 
#' The function [get_nested()] returns a \link[base]{list} of \link[base]{language} object(s).
#' 
#' @examples
#' get_nested(~ g1)
#' get_nested(~ g1:g2)
#' get_nested(~ g1/g2a:g2b/g3a:g3b:g3c)
#' 
#' library(nlme)
#' nlme::Wafer |>
#'  getGroupsFormula() |>
#'  get_nested()
#' 
#' @keywords internal
#' @export
get_nested <- function(by) {
  
  z <- by |>
    nested_() |> 
    unlist()
  if (!is.list(z)) z <- list(z)
  
  names(z) <- z |>
    vapply(FUN = deparse1, FUN.VALUE = NA_character_)
  
  # class(z) <- c('nested') # do I want to do this?
  return(z)
  
}


# The `S3` generic function [get_nested_factor()] returns 
# a \link[base]{list} of \link[base]{factor}s, 
# ready to be passed into the parameter `by` of the function
# \link[stats]{aggregate.data.frame}.
# 

if (FALSE) {
  
  x1 = nlme::Wafer |>
    nlme:::getGroups.data.frame(form = ~Wafer/Site)
  x2 = nlme::Wafer |> 
    nlme::getGroups()
  stopifnot(identical(x1, x2))
  

  nlme::Wafer |>
    nlme::getGroups() |> 
    structure(class = c('getGroups', 'data.frame'))
  
  spatstat.data::osteo |>
    setGroups(group = ~ id)
}







