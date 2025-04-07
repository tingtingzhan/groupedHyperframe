


#' @title [groupedHyperframe] with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' ..
#' 
#' @param formula \link[stats]{formula} in the format of 
#' `m1+m2 ~ y+x1+x2 | g1/g2`,
#' where \eqn{m_i}'s are one or more \link[spatstat.geom]{marks},
#' \eqn{y} and \eqn{x_j}'s are the endpoint and predictor(s) for downstream analysis,
#' and \eqn{g_k} are one or more nested grouping structure
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param coords \link[stats]{formula}, variable names
#' of \eqn{x}- and \eqn{y}-coordinates in `data`.
#' Default `~x+y`.  
#' 
#' @param window an observation window \link[spatstat.geom]{owin}, 
#' default is the \eqn{x}- and \eqn{y}-span of `coords` in `data`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [grouped_ppp()] returns a [groupedHyperframe]
#' with ***one-and-only-one*** 
#' \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom owin ppp as.hyperframe.data.frame
#' @importFrom stats runif
#' @export
grouped_ppp <- function(
    formula, 
    data, 
    coords = ~ x + y, 
    window = owin(xrange = range(.x), yrange = range(.y)),
    ...
) {	
  
  # Step 1: grouped hyperframe (may consider writing into a function)
  
  group <- formula[[3L]][[3L]]
  g <- all.vars(group)
  data[g] <- lapply(data[g], FUN = \(i) {
    if (is.factor(i)) return(factor(i)) # drop empty levels!!
    factor(i, levels = unique(i))
  }) 
  
  fg <- interaction(data[g], drop = TRUE, sep = '.', lex.order = TRUE) # one or more hierarchy

  hf <- data[all.vars(formula[[3L]])] |>
    mc_identical_by(f = fg, ...) |>
    as.hyperframe.data.frame()
  
  # Step 2: grouped ppp
  
  #if (isFALSE(coords)) {
  #  # .Deprecated(new = 'as.groupedHyperframe.data.frame')
  #  # tzh's downstream package not using this functionality
  #  .x <- runif(n = nrow(data))
  #  .y <- runif(n = nrow(data))
  #} else {
    xy_ <- as.list.default(coords[[2L]])
    if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
    if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
    if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
    if (!length(.x <- data[[x]]) || anyNA(.x)) stop('Do not allow missingness in x-coordinates')
    if (!length(.y <- data[[y]]) || anyNA(.y)) stop('Do not allow missingness in y-coordinates')
  #}
  
  force(window)
  
  tmp <- ppp(x = .x, y = .y, window = window, marks = data[all.vars(formula[[2L]])], checkdup = FALSE, drop = FALSE) # `drop = FALSE` important!!!
  hf$ppp. <- tmp |> 
    split_ppp_dataframe(f = fg)
  
  # additional attributes to mimic ?nlme::groupedData
  # also see example 'groupedData's from package datasets
  attr(hf, which = 'group') <- call('~', group) # for ?nlme::getGroupsFormula
  # let `attr(,'group')` be ?base::call instead of ?stats::formula
  # formula's environment is very annoying!!
  # end of additional attributes
  
  class(hf) <- c('groupedHyperframe', class(hf)) |> unique.default()
  return(hf)
  
}



# tzh is not ready to suggest changing ?spatstat.geom::split.ppp to Dr. Baddeley, yet..
# [split_ppp_dataframe()] is a bandage-fix which respects ncol-1 dataframe
# haha tzh now knows the real problem is ?spatstat.geom::`[.ppp`
# and has written to Dr. Baddeley :)
#' @importFrom spatstat.geom markformat.ppp
split_ppp_dataframe <- function(x, f) {
  # `f` must be 'factor'
  mapply(FUN = \(...) {
    ret <- list(...)
    class(ret) <- class(x)
    return(ret)
  }, 
  x = split.default(x$x, f = f),
  y = split.default(x$y, f = f),
  marks = split.data.frame(x$marks, f = f),
  n = split.default(seq_along(f), f = f) |> lengths(use.names = FALSE),
  MoreArgs = list(
    window = x$window,
    markformat = markformat.ppp(x)
  ), SIMPLIFY = FALSE)
}





