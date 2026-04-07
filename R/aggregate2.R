
#' @title Aggregate, An Alternative Way
#' 
#' @description
#' An alternative aggregation function, 
#' to avoid the \link[base]{cbind}-operation in the function \link[stats]{aggregate.formula}.
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @param by a two-sided \link[stats]{formula}
#' 
#' @param ... additional parameters of the function \link[stats]{aggregate.data.frame}
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
#' @importFrom stats aggregate.data.frame model.frame
#' @export
aggregate2 <- function(x, by, ...) {
  
  # drop unused factor levels in all columns of `x`
  x[] <- x |>
    lapply(FUN = \(i) {
      if (!is.factor(i)) return(i)
      factor(i) # drop empty levels!!
    })
  
  if (!is.call(by) || (by[[1L]] != '~') || (length(by) != 3L)) stop('`by` must be two-sided formula')
  
  if (by[[2L]] == '.') {
    vars <- setdiff(x = names(x), y = all.vars(by[[3L]]))
  } else {
    vars <- all.vars(by[[2L]])
  }
  
  call(name = '~', by[[3L]]) |>
    model.frame(formula = _, data = x) |>
    as.list.data.frame() |>
    lapply(FUN = \(i) {
      if (is.factor(i)) return(i)
      return(factor(i, levels = unique(i)))
    }) |>
    aggregate.data.frame(x = x[vars], by = _, ...)
  
}









if (FALSE) {
  
  d = data(package = 'nlme') |> 
    packageIQR::dataFrom()
  
  d |> 
    sapply(FUN = nrow) |>
    sort()
  
  nlme::Rail
  nlme::Rail$Rail |>
    levels()
  
  nlme::Rail |>
    aggregate.data.frame(
      by = list(Rail = nlme::Rail$Rail), # bad!!! base::levels messed up!!
      FUN = unique,
      simplify = FALSE
    )
  
  aggregateBy(. ~ Rail, data = nlme::Rail)
  
  x = nlme::Rail |>
    aggregate(x = . ~ Rail, data = _, FUN = unique, simplify = FALSE)
  x
  x$travel |> class() # 'list'
  
  suppressPackageStartupMessages(library(spatstat))
  x |>
    spatstat.geom::as.hyperframe.data.frame() # wow!!!!!
  
  
  
}


if (FALSE) {
  
  library(survival)
  unique_or_identity = \(x) {
    u = unique(x)
    if (length(u) == 1L) return(u)
    return(x)
  }
  
  wrobel_lung |>
    within.data.frame(expr = {
      x = y = NULL
      dapi = NULL
    }) |>
    aggregate(
      # `cbind` in ?stats:::aggregate.formula does not respect Surv-column!!
      # looks like factor-column is also messed up.
      x = . ~ patient_id/image_id, 
      data = _,
      FUN = unique_or_identity, 
      simplify = TRUE, drop = TRUE
    ) |> 
    spatstat.geom::as.hyperframe.data.frame()
  
  
}
