
#' @title Creation of `groupedHyperframe`
#' 
#' @description
#' To create a `groupedHyperframe` object
#' 
#' @param x see Usage
#' 
#' @param group \link[stats]{formula}
#' 
#' @param ... additional parameters
#' 
#' @returns
#' Function [as.groupedHyperframe()] returns a `groupedHyperframe`.
#' 
#' @keywords internal
#' @name as.groupedHyperframe
#' @export
as.groupedHyperframe <- function(x, group, ...) UseMethod(generic = 'as.groupedHyperframe')


#' @rdname as.groupedHyperframe
#' @importFrom spatstat.geom names.hyperframe
#' @export as.groupedHyperframe.hyperframe
#' @export
as.groupedHyperframe.hyperframe <- function(x, group, ...) {
  
  if (!is.language(group) || group[[1L]] != '~') stop('`group` must be a formula')
  
  if (length(group) != 2L) stop('`group` must be one-sided formula')
  
  if (!all(all.vars(group) %in% names.hyperframe(x))) stop('`group` contains unknown variable')
  
  attr(x, which = 'group') <- group
  class(x) <- c('groupedHyperframe', class(x)) |> unique.default()
  return(x)
  
}


#' @rdname as.groupedHyperframe
#' @importFrom spatstat.geom hyperframe cbind.hyperframe
#' @export as.groupedHyperframe.data.frame
#' @export
as.groupedHyperframe.data.frame <- function(x, group, ...) {
  
  # copie as much as possible from [grouped_ppp()]
  
  g <- all.vars(group)
  x[g] <- lapply(x[g], FUN = \(i) {
    if (is.factor(i)) return(factor(i)) # drop empty levels!!
    factor(i, levels = unique(i))
  }) 
  
  fg <- interaction(x[g], drop = TRUE, sep = '.', lex.order = TRUE) # one or more hierarchy
  
  suppressMessages(x1 <- x |> mc_identical_by(f = fg, ...))
  
  hf <- x1 |>
    as.hyperframe.data.frame()
  
  nm <- x1 |> attr(which = 'non_identical', exact = TRUE)
  if (length(nm)) {
    hf <- x[nm] |> 
      split.data.frame(f = fg) |>
      c(FUN = list, SIMPLIFY = FALSE, USE.NAMES = TRUE) |> 
      do.call(what = mapply) |>
      do.call(what = hyperframe) |>
      cbind.hyperframe(hf) # crazy pipeline!!!
  }
  
  attr(hf, which = 'group') <- group # for ?nlme::getGroupsFormula
  class(hf) <- c('groupedHyperframe', class(hf)) |> unique.default()
  return(hf)

}
