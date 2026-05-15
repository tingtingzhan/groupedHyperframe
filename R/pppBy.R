
#' @title Hyper Data Frame with One-and-Only-One \link[spatstat.geom]{ppp}-Hyper Column
#' 
#' @description
#' To create a hyper data frame with one-and-only-one \link[spatstat.geom]{ppp}-hyper column.
#' 
#' @param marks one-sided \link[stats]{formula}, e.g., 
#' `~ m1+m2`,
#' where \eqn{m_i}'s are one or more \link[spatstat.geom]{marks}
#' 
#' @param coords one-sided \link[stats]{formula}, variable names
#' of the \eqn{x}- and \eqn{y}-coordinates in `data`.
#' Default value is `~x+y`.  
#' 
#' @param by two-sided \link[stats]{formula}
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param window observation window \link[spatstat.geom]{owin}, 
#' default value is the \eqn{x}- and \eqn{y}-span of `coords` in `data`.
#' 
#' @param ... additional parameters of the function \link[spatstat.geom]{ppp}
#' 
#' @returns
#' The function [pppBy()] returns a hyper data frame
#' with one-and-only-one
#' \link[spatstat.geom]{ppp}-hyper column.
#' 
#' @keywords internal
#' @importFrom spatstat.geom owin ppp as.hyperframe.data.frame split.ppp
#' @importFrom stats model.frame.default aggregate.data.frame
#' @export
pppBy <- function(
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
    if (!all(vars %in% names(data))) stop()
  }
  
  f <- by[[3L]] |> 
    call(name = '~', . = _) |>
    model.frame.default(formula = _, data = data) |>
    as.list.data.frame() |>
    interaction(drop = TRUE, lex.order = TRUE) # one or more hierarchy
  if (all(table(f) == 1L)) stop('shouldnt happen')
  
  d_ag <- data[unique.default(c(all.vars(by[[3L]]), vars))] |>
    aggregate.data.frame(
      x = _,
      by = list(.f = f), 
      FUN = unique_or_identity, 
      simplify = TRUE # must!! for `Surv`-column!!
    )
  d_ag[] <- d_ag |> 
    lapply(FUN = unsimplify)
  hf <- d_ag[-1L] |> # grouping structure on the 1st column removed
    as.hyperframe.data.frame()
  
  xy_ <- as.list.default(coords[[2L]])
  if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
  if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
  if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
  if (!length(.x <- data[[x]]) || anyNA(.x)) stop('Do not allow missingness in x-coordinates')
  if (!length(.y <- data[[y]]) || anyNA(.y)) stop('Do not allow missingness in y-coordinates')
  
  force(window)
  hf$ppp. <- data[all.vars(marks)] |> # future: use stats::model.frame
    ppp(x = .x, y = .y, window = window, marks = _, ...) |>
    split.ppp(f = f)

  return(hf)
  
}



