
#' @title Aggregate Hyper Data Frame
#' 
#' @description
#' To \link[stats]{aggregate} a \link[spatstat.geom]{hyperframe}.
#' 
#' @param x \link[spatstat.geom]{hyperframe}
#' 
#' @param by two-sided \link[stats]{formula}, 
#' whose right-hand-side contains only the regular-column names of the input `x`
#' 
#' @param ... additional parameters of the function \link[stats]{aggregate.data.frame},
#' *except for* `simplify`
#' 
#' @returns 
#' The `S3` method [aggregate.hyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#'  
#' @importFrom spatstat.geom as.hyperframe.data.frame cbind.hyperframe names.hyperframe subset.hyperframe
#' @importFrom stats aggregate aggregate.data.frame
#' @export
aggregate.hyperframe <- function(
    x, 
    by,
    ...
) {
  
  if (!is.call(by) || by[[1L]] != '~' || length(by) != 3L) stop('`by` must be two-sided formula')
  if (!all(all.vars(by[[3L]]) %in% names(unclass(x)$df))) stop('Variables in right-hand-side of `by` must be columns, not hypercolumns')
  
  f <- by[[3L]] |>
    call(name = '~', . = _) |>
    model.frame.default(formula = _, data = unclass(x)$df) |>
    as.list.data.frame() |>
    interaction(drop = TRUE, lex.order = TRUE)
  if (all(table(f) == 1L)) return(x) # exception handling (no need to aggregate)
  
  if (is.symbol(by[[2L]]) && (by[[2L]] == '.')) {
    vars <- names.hyperframe(x)
  } else if (is.call(by[[2L]]) && (by[[2L]][[1L]] == '-')) {
    # e.g. `by = . - x1 - x2 ~ subj_id/image_id`
    vars <- names.hyperframe(x) |>
      lapply(FUN = as.symbol) |>
      Reduce(f = \(e1, e2) call(name = '+', e1, e2)) |>
      call(name = '~', . = _) |>
      eval() |>
      update.formula(new = call(name = '~', by[[2L]])) |>
      all.vars()
  } else {
    vars <- all.vars(by)
    if (!all(vars %in% names.hyperframe(x))) stop()
  }
  
  x. <- x |> 
    subset.hyperframe(
      select = c(all.vars(by[[3L]]), vars) |> 
        unique.default()
    )
  # ?spatstat.geom::`[.hyperframe` # may have bugs..
  # ?spatstat.geom::subset.hyperframe # seems more reliable

  x0 <- unclass(x.)
  
  odf <- x0$df |>
    aggregate.data.frame(x = _, by = list(.f = f), ..., simplify = TRUE)
  # must use `simplify = TRUE` for `Surv`-column!!
  odf[] <- odf |> 
    lapply(FUN = unsimplify)
  odf <- odf[-1L] # grouping structure on the 1st column removed
  
  ohc <- x0$hypercolumns |>
    lapply(FUN = split.default, f = f)
  
  ret <- odf |>
    as.hyperframe.data.frame() |>
    list() |>
    c(ohc) |>
    do.call(
      what = cbind.hyperframe, 
      args = _
    )
  
  return(ret)
  
}


