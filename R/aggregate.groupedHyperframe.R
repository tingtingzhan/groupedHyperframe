
#' @title Aggregate [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param by a one-sided \link[stats]{formula}
#' 
#' @param fun \link[base]{function}, aggregation method, 
#' currently supports
#' [pmean()], [pmedian()], \link[base]{pmax}, and \link[base]{pmin}.
#' 
#' @param ... additional parameters
#' 
#' @details
#' Function [aggregate.groupedHyperframe()] checks `by` against `attr(x,'group')`.
#' 
#' @returns 
#' Function [aggregate.groupedHyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#'  
#' @keywords internal
#' @importFrom stats aggregate
#' @importFrom spatstat.geom cbind.hyperframe
#' @export aggregate.groupedHyperframe
#' @export
aggregate.groupedHyperframe <- function(
    # dots, # to remove!!!
    x, 
    by,
    fun = pmean,
    ...
) {
  
  x0 <- unclass(x)
  xdf <- x0$df
  xhc <- x0$hypercolumns
  
  group <- x |> 
    attr(which = 'group', exact = TRUE)
  
  if (!is.call(by) || by[[1L]] != '~' || length(by) != 2L) stop('`by` must be one-sided formula')
  if (!is.symbol(by. <- by[[2L]])) {
    new_by <- by. |>
      all.vars() |>
      vapply(FUN = \(i) deparse1(call(name = '~', as.symbol(i))), FUN.VALUE = '')
    message('grouped structure ', paste('by =', deparse1(by)) |> col_cyan(), ' is not allowed')
    new_by_txt <- paste('by =', new_by) |> col_magenta()
    message('please use either one of ', paste(new_by_txt, collapse = ', '), '.')
    stop('`by` must be a formula and right-hand-side must be a symbol')
  }
  # `group` 'up-to' `by.`
  # how to do it beautifully?
  # below is an ugly bandage fix
  g <- all.vars(group)
  id <- match(as.character(by.), table = g)
  if (is.na(id)) stop('`by` must match one of the hierarchy in groupedHyperframe')
  # end of ugly bandage fix
  
  # grouping structure must be specified by `$df` part!!
  f <- xdf[g[seq_len(id)]] |>
    interaction(drop = TRUE, sep = '.', lex.order = TRUE)
  fid <- split.default(seq_along(f), f = f)
  
  if (all(lengths(fid) == 1L)) {
    message('no need to aggregate')
    return(x)
  } 

  # aggregation drops `ppp`- and `fv`-hypercolumn !!!
  xdf_ag <- xdf |> 
    mc_identical_by(f = f, ...)# |>
    #as.hyperframe.data.frame()
  
  fun_supported <- list(pmean, pmedian, pmax, pmin) |>
    vapply(FUN = identical, y = fun, FUN.VALUE = NA) |>
    any()
  if (!fun_supported) {
    '`fun`' |> 
      col_blue() |>
      sprintf(fmt = '%s must be one of {.fun groupedHyperframe::pmean}, {.fun groupedHyperframe::pmedian}, {.fun base::pmax} or {.fun base::pmin}') |> 
      cli_text() |> 
      message(appendLF = FALSE)
    stop()
  }
  
  id <- xhc |>
    vapply(FUN = is.vectorlist, mode = 'numeric', FUN.VALUE = NA)
  if (!any(id)) {
    stop('no `vectorlist` to aggregate!')
  }
  
  xhc_ag <- xhc[id] |> 
    lapply(FUN = \(m) { # (m = xhc[id][[1L]])
      fid |>
        lapply(FUN = \(i) { # (i = fid[[1L]])
          m[i] |> 
            do.call(what = fun, args = _)
        })
    })# |>
    #do.call(what = hyperframe)
  
  #ret <- cbind.hyperframe(xdf_ag, xhc_ag)
  ret <- do.call(
    what = cbind.hyperframe, 
    args = c(list(xdf_ag), xhc_ag)
  ) # returns 'hyperframe' !!!
  
  return(ret)
  
}


