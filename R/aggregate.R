
if (FALSE) {
  library(spatstat.mc)
  s = wrobel_lung |>
    grouped_ppp(
      formula = hladr + phenotype ~ OS + gender + age,
      by = ~ patient_id/image_id
    )
  r = seq.int(from = 0, to = 250, by = 10)
  out = s |>
    within(expr = {
      hladr.E = ppp. |> 
        Emark_(r = r, correction = 'none') |>
        .disrecommend2theo()
      phenotype.G = ppp. |> 
        Gcross_(i = 'CK+.CD8-', j = 'CK-.CD8+', r = r, correction = 'none') |>
        .disrecommend2theo()
      phenotype.nnc = ppp. |>
        nncross_(i = 'CK+.CD8-', j = 'CK-.CD8+', correction = 'none')
    })
  out_fv = out |>
    within(expr = {
      hladr.Ey = keyval(hladr.E)
      phenotype.Gy = keyval(phenotype.G)
      hladr.E.cumv = cumvtrapz(hladr.E, drop = TRUE)
      phenotype.G.cumv = cumvtrapz(phenotype.G, drop = TRUE)
    })
  out_fv |>
    aggregate(fun = pmean)
  
  
}


#' @title Aggregate Hyper Data Frame
#' 
#' @description
#' To aggregate the aggregatable hyper columns of a 
#' \link[spatstat.geom]{hyperframe}.
#' 
#' 
#' @param x a \link[spatstat.geom]{hyperframe}
#' 
#' @param by a one-sided \link[stats]{formula}, 
#' containing regular-column names of the input `x`
#' 
#' @param ... additional parameters of the function [aggregate.vectorlist()], 
#' most importantly the parameter `fun`
#' 
#' @returns 
#' The `S3` method [aggregate.hyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#'  
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom is.ppplist is.imlist
#' @importFrom nlme getGroupsFormula
#' @export
aggregate.hyperframe <- function(
    x, 
    by = getGroupsFormula(x),
    ...
) {
  
  x0 <- unclass(x)
  xdf <- x0$df
  xhc <- x0$hypercolumns
  
  if (!length(by)) stop('must provide valid `by`')
  
  if (!is.call(by) || by[[1L]] != '~' || length(by) != 2L) stop('`by` must be one-sided formula')
  if (!is.symbol(by. <- by[[2L]])) stop('right-hand-side of `by` must be a symbol')
  if (!(as.character(by.) %in% names(xdf))) stop('`.by` must be a column, not a hypercolumn')
  
  f <- xdf[[by.]] |> 
    as.factor()
  if (all(table(f) == 1L)) return(x) # exception handling
  
  xdf_ag <- xdf |>
    aggregate.data.frame(by = list(f), FUN = unique, simplify = TRUE, drop = TRUE)
  xdf_ag <- xdf_ag[-1L]
  orig_class <- xdf |> lapply(FUN = class)
  ag_class <- xdf_ag |> lapply(FUN = class)
  id <- which(!mapply(FUN = identical, orig_class, ag_class)) # columns to be turned into hypercolumns
  if (length(id)) {
    names(id) |>
      col_magenta() |> style_bold() |>
      paste(collapse = ', ') |>
      sprintf(fmt = 'Variable(s) %s removed from aggregation') |>
      message()
    xdf_ag[id] <- NULL 
  }
  
  id_vector <- xhc |>
    vapply(FUN = is.vectorlist, mode = 'numeric', FUN.VALUE = NA)
  xhc_vector <- if (any(id_vector)) {
    xhc[id_vector] |> 
      lapply(FUN = aggregate.vectorlist, by = f, ...)
  } #else NULL
  
  # object-list supported by spatstat family
  id_ppp <- xhc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  id_im <- xhc |>
    vapply(FUN = is.imlist, FUN.VALUE = NA)
  id_lol <- id_ppp | id_im # list-of-list
  xhc_lol <- if (any(id_lol)) {
    xhc[id_lol] |> 
      lapply(FUN = split.default, f = f)
  } #else NULL
  
  # object-list *not* supported by spatstat family
  id_fv <- xhc |>
    vapply(FUN = is.fvlist, FUN.VALUE = NA) |>
    suppressMessages()
  xhc_fv <- if (any(id_fv)) {
    xhc[id_fv] |> 
      lapply(FUN = as.fvlist) |>
      lapply(FUN = split.default, f = f)
  } #else NULL
  
  ret <- do.call(
    what = cbind.hyperframe, 
    args = c(list(xdf_ag), xhc_vector, xhc_lol, xhc_fv)
  ) # returns 'hyperframe', *not* 'groupedHyperframe' !!
  
  return(ret)
  
}






#' @title Aggregate `vectorlist`
#' 
#' @param x a `vectorlist`
#' 
#' @param by \link[base]{factor}, of same \link[base]{length} as `x`
#' 
#' @param fun \link[base]{function}, aggregation method, 
#' currently supports functions
#' [pmean()], [pmedian()], \link[base]{pmax}, and \link[base]{pmin}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns
#' The `S3` method [aggregate.vectorlist()] returns a `vectorlist`.
#' 
#' @keywords internal
#' @importFrom spatstat.geom anylapply
#' @export
aggregate.vectorlist <- function(x, by, fun = pmean, ...) {
  
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
  
  if (!is.factor(by)) stop('`by` must be factor')
  if (length(by) != length(x)) stop('`by` and `x` must be of same length')
  
  fid <- split.default(seq_along(by), f = by)
  if (all(lengths(fid) == 1L)) {
    message('no need to aggregate')
    return(invisible(x))
  } 
  
  fid |>
    anylapply(FUN = \(i) { # (i = fid[[1L]])
      x[i] |> 
        do.call(what = fun, args = _)
    }) |>
    as.vectorlist(mode = 'numeric')
  
  # 'vectorlist' not respected by spatstat.geom::hyperframe(), yet
  
  
}


