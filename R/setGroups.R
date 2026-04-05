
#' @title Creation of `groupedHyperframe`
#' 
#' @description
#' To create a `groupedHyperframe` object.
#' 
#' @param x see Usage
#' 
#' @param group \link[stats]{formula}
#' 
#' @param ... additional parameters
#' 
#' @returns
#' The `S3` generic function [setGroups()] returns an object of derived class `'grouped*'`.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name setGroups
#' @export
setGroups <- function(x, group, ...) UseMethod(generic = 'setGroups')

#' @rdname setGroups
#' @export
setGroups.hyperframe <- function(x, group, ...) {
  
  if (!is.language(group) || group[[1L]] != '~') stop('`group` must be a formula')
  
  if (length(group) != 2L) stop('`group` must be one-sided formula')
  
  if (!all(all.vars(group) %in% names(unclass(x)$df))) stop('`group` must contain only column-names')
  
  attr(x, which = 'group') <- group
  class(x) <- c('groupedHyperframe', class(x)) |> 
    unique.default()
  return(x)
  
}


