
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
#' The `S3` generic function [as.groupedHyperframe()] returns a `groupedHyperframe`.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name as.groupedHyperframe
#' @export
as.groupedHyperframe <- function(x, group, ...) UseMethod(generic = 'as.groupedHyperframe')


#' @rdname as.groupedHyperframe
#' @export
as.groupedHyperframe.hyperframe <- function(x, group, ...) {
  
  if (!is.language(group) || group[[1L]] != '~') stop('`group` must be a formula')
  
  if (length(group) != 2L) stop('`group` must be one-sided formula')
  
  if (!all(all.vars(group) %in% names(unclass(x)$df))) stop('`group` must contain only column-names')
  
  attr(x, which = 'group') <- group
  class(x) <- c('groupedHyperframe', class(x)) |> 
    unique.default()
  return(x)
  
}


