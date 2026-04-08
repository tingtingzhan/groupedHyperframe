


#' @title Hyper Data Frame with One-and-Only-One \link[spatstat.geom]{ppp}-Hyper Column
#' 
#' @description
#' To create a (grouped) hyper data frame with one-and-only-one \link[spatstat.geom]{ppp}-hyper column.
#' 
#' @param marks a one-sided \link[stats]{formula} in the format of 
#' `~ m1+m2`,
#' where \eqn{m_i}'s are one or more \link[spatstat.geom]{marks}
#' 
#' @param coords \link[stats]{formula}, variable names
#' of the \eqn{x}- and \eqn{y}-coordinates in `data`.
#' Default value is `~x+y`.  
#' 
#' @param by a two-sided \link[stats]{formula}, see function [aggregate2()]
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param window an observation window \link[spatstat.geom]{owin}, 
#' default value is the \eqn{x}- and \eqn{y}-span of `coords` in `data`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' The function [grouped_ppp()] returns a (grouped) hyper data frame
#' with *one-and-only-one*
#' \link[spatstat.geom]{ppp}-hyper column.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/grouped_ppp_appx.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom owin ppp as.hyperframe.data.frame split.ppp solapply
#' @importFrom stats model.frame aggregate.data.frame
#' @export
grouped_ppp <- function(
    marks,
    coords = ~ x + y, 
    by,
    data, 
    window = owin(xrange = range(.x), yrange = range(.y)),
    ...
) {	
  
  # drop unused factor levels in all columns of `x`
  data[] <- data |>
    lapply(FUN = \(i) {
      if (!is.factor(i)) return(i)
      factor(i) # drop empty levels!!
    })
  
  if (is.symbol(by[[2L]]) && (by[[2L]] == '.')) {
    # old name `by2var`
    vars <- names(data) |>
      setdiff(y = c(all.vars(marks), all.vars(coords)))
  } else if (is.call(by[[2L]]) && (by[[2L]][[1L]] == '-')) {
    # e.g. `by = . - x1 - x2 ~ subj_id/image_id`
    vars <- names(data) |>
      setdiff(y = c(all.vars(marks), all.vars(coords))) |>
      lapply(FUN = as.symbol) |>
      Reduce(f = \(e1, e2) call(name = '+', e1, e2)) |>
      call(name = '~', . = _) |>
      eval() |>
      update.formula(new = call(name = '~', by[[2L]])) |>
      all.vars()
  } else {
    vars <- all.vars(by)
  }
  
  unique_or_identity <- \(x) {
    u = unique(x)
    if (length(u) == 1L) return(u)
    return(x)
  }
  
  f <- by[[3L]] |> 
    call(name = '~', . = _) |>
    model.frame(formula = _, data = data) |>
    as.list.data.frame() |>
    interaction(drop = TRUE, lex.order = TRUE) # one or more hierarchy
  
  hf <- data[unique(c(all.vars(by[[3L]]), vars))] |> # grouping structure as the first column(s)
    #aggregate2(by = by, FUN = unique_or_identity, simplify = TRUE, drop = TRUE) |>
    aggregate.data.frame(by = list(.f = f), FUN = unique_or_identity, simplify = TRUE, drop = TRUE) |>
    as.hyperframe.data.frame()
  hf$.f <- NULL
  
  xy_ <- as.list.default(coords[[2L]])
  if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
  if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
  if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
  if (!length(.x <- data[[x]]) || anyNA(.x)) stop('Do not allow missingness in x-coordinates')
  if (!length(.y <- data[[y]]) || anyNA(.y)) stop('Do not allow missingness in y-coordinates')
  
  force(window)
  hf$ppp. <- ppp(x = .x, y = .y, window = window, marks = data[all.vars(marks)], checkdup = FALSE, drop = FALSE) |> # `drop = FALSE` important!!!
    split.ppp(f = f, drop = FALSE) |>
    solapply(FUN = \(i) {
      rownames(i$marks) <- NULL # from spatstat.geom::split.ppp
      return(i)
    })

  return(hf)
  
}



