


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
#' Default value is `~x+y`.  
#' 
#' @param window an observation window \link[spatstat.geom]{owin}, 
#' default is the \eqn{x}- and \eqn{y}-span of `coords` in `data`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' The function [grouped_ppp()] returns a [groupedHyperframe]
#' with ***one-and-only-one*** 
#' \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/grouped_ppp_appx.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom owin ppp as.hyperframe.data.frame split.ppp
#' @export
grouped_ppp <- function(
    formula, 
    data, 
    coords = ~ x + y, 
    window = owin(xrange = range(.x), yrange = range(.y)),
    ...
) {	
  
  # Step 1: grouped hyperframe (may consider writing into a function)
  
  fg <- (formula[[3L]][[3L]]) |> 
    get_nested_factor(data = data) |>
    interaction(drop = TRUE, sep = '.', lex.order = TRUE) # one or more hierarchy

  fom3var <- all.vars(formula[[3L]])
  if ('.' %in% fom3var) {
    dotvar <- names(data) |>
      setdiff(y = all.vars(formula[[2L]]))
    fom3var <- fom3var |>
      setdiff(y = '.') |>
      c(dotvar) |>
      unique.default()
  } # ugly but works :)
  
  hf <- data[fom3var] |>
    mc_identical_by(f = fg, ...) |>
    as.hyperframe.data.frame()
  
  # Step 2: grouped ppp
  
  xy_ <- as.list.default(coords[[2L]])
  if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
  if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
  if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
  if (!length(.x <- data[[x]]) || anyNA(.x)) stop('Do not allow missingness in x-coordinates')
  if (!length(.y <- data[[y]]) || anyNA(.y)) stop('Do not allow missingness in y-coordinates')
  
  force(window)
  hf$ppp. <- ppp(x = .x, y = .y, window = window, marks = data[all.vars(formula[[2L]])], checkdup = FALSE, drop = FALSE) |> # `drop = FALSE` important!!!
    split.ppp(f = fg, drop = FALSE)
  
  g <- call(name = '~', formula[[3L]][[3L]]) |>
    drop_lowest_nested()
  # let this be ?base::call instead of ?stats::formula
  # formula's environment is very annoying!!
  # end of additional attributes
  
  if (length(g)) {
    attr(hf, which = 'group') <- g
    class(hf) <- c('groupedHyperframe', class(hf)) |> unique.default()
  }
  
  return(hf)
  
}










