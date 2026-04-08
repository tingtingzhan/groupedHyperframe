
#' @title Handy `FUN`ctions for \link[stats]{aggregate}
#' 
#' @description
#' 
#' Handy functions to be passed to the parameter `FUN` of the function \link[stats]{aggregate}
#' and/or [aggregate2()].
#' 
#' @param x an R object
#' 
#' @returns
#' The function [unique_or_identity()] returns an R object.
#' 
#' @name aggregate_FUN
#' @export
unique_or_identity <- \(x) {
  u = unique(x)
  if (length(u) == 1L) return(u)
  return(x)
}

