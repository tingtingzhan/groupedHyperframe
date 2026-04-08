
#' @title Aggregate, an Alternative \link[stats]{formula}-Interface
#' 
#' @description
#' An alternative aggregation function with a \link[stats]{formula}-interface, 
#' to avoid the \link[base]{cbind}-operation in the function \link[stats]{aggregate.formula}.
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param by a two-sided \link[stats]{formula}
#' 
#' @param ... additional parameters of the function \link[stats]{aggregate.data.frame}
#' 
#' @details
#' The \link[base]{cbind}-operation in the function \link[stats]{aggregate.formula} 
#' messes up with column(s) that are 
#' \describe{
#' \item{\link[base]{factor}}{and treat them as \link[base]{integer}}
#' \item{\link[survival]{Surv}}{and treat them as \link[base]{matrix}}
#' }
#' 
#' The function \link[stats]{aggregate.data.frame} only accepts 
#' a \link[base]{list} of \link[base]{factor}s for the parameter `by`.
#' 
#' Therefore, the function [aggregate2()] is created to take care of 
#' the \link[base]{factor} and \link[survival]{Surv} columns of the input,
#' with a \link[stats]{formula}-interface.
#' 
#' @returns 
#' The function [aggregate2()] returns a \link[base]{data.frame}.
#' 
#' @note
#' The function \link[stats]{aggregate.data.frame} is the workhorse of 
#' the function \link[stats]{aggregate.formula}.
#' 
#' The function \link[spatstat.geom]{as.hyperframe.data.frame}
#' is **designed** to handle the \link[base]{list}-columns 
#' returned by the function \link[stats]{aggregate}.
#' 
#' @keywords internal
#' @importFrom stats aggregate.data.frame model.frame update.formula
#' @export
aggregate2 <- function(data, by, ...) {
  
  # drop unused factor levels in all columns of `data`
  data[] <- data |>
    lapply(FUN = \(i) {
      if (!is.factor(i)) return(i)
      factor(i) # drop empty levels!!
    })
  
  if (!is.call(by) || (by[[1L]] != '~') || (length(by) != 3L)) stop('`by` must be two-sided formula')
  
  if (is.symbol(by[[2L]]) && (by[[2L]] == '.')) {
    vars <- names(data)
  } else if (is.call(by[[2L]]) && (by[[2L]][[1L]] == '-')) {
    # e.g. `by = . - x1 - x2 ~ subj_id/image_id`
    vars <- names(data) |>
      lapply(FUN = as.symbol) |>
      Reduce(f = \(e1, e2) call(name = '+', e1, e2)) |>
      call(name = '~', . = _) |>
      eval() |>
      update.formula(new = call(name = '~', by[[2L]])) |>
      all.vars()
  } else {
    vars <- all.vars(by)
  }
  
  z <- by[[3L]] |>
    call(name = '~', . = _) |>
    model.frame(formula = _, data = data) |>
    as.list.data.frame() |>
    interaction(drop = TRUE, lex.order = TRUE) |>
    list(.f = _) |>
    aggregate.data.frame(x = data[unique(c(all.vars(by[[3L]]), vars))], by = _, ...) # grouping structure as the first column(s)
  z$.f <- NULL
  return(z)
  
}





