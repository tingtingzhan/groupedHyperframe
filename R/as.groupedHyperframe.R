
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
#' The function [as.groupedHyperframe()] returns a `groupedHyperframe`.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name as.groupedHyperframe
#' @export
as.groupedHyperframe <- function(x, group, ...) UseMethod(generic = 'as.groupedHyperframe')


#' @rdname as.groupedHyperframe
#' @importFrom spatstat.geom names.hyperframe
#' @export
as.groupedHyperframe.hyperframe <- function(x, group, ...) {
  
  if (!is.language(group) || group[[1L]] != '~') stop('`group` must be a formula')
  
  if (length(group) != 2L) stop('`group` must be one-sided formula')
  
  if (!all(all.vars(group) %in% names.hyperframe(x))) stop('`group` contains unknown variable')
  
  attr(x, which = 'group') <- group
  class(x) <- c('groupedHyperframe', class(x)) |> unique.default()
  return(x)
  
}


