
#' @title Aggregate \link[spatstat.explore]{fv.object}s by Cluster
#' 
#' @description
#' Aggregate information in \link[spatstat.explore]{fv.object}s
#' by sample clustering.
#' 
#' @param X a [groupedHyperframe], 
#' containing one or more \link[spatstat.explore]{fv.object} column(s)
#' 
#' @param by one-sided \link[stats]{formula}, sample clustering.
#' Use only one-level hierarchy (e.g., `~patient` or `~image`).
#' Do not use multi-level hierarchy (e.g., `~patient/image`)
#' 
#' @param f_aggr_ see function [aggregate_by_()]
#' 
#' @param rmax \link[base]{numeric} scalar, user-specified truncation point \eqn{r_\text{max}}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
#' 
#' @param ... additional parameters, currently not in use
#' 
# @note 
# tzh hesitates to create a function of `aggregate.hyperframe`,
# which could be claimed by \CRANpkg{spatstat} authors in future.
# -- Inna is correct: we do not aggregate-hyperframe, we aggregate-fv-columns-inside-hyperframe.
#' 
#' @returns
#' Function [aggregate_fv()] returns a \link[base]{data.frame}, with
#' aggregated information stored in \link[base]{matrix}-columns.
#' 
#' Note that \link[spatstat.geom]{hyperframe} does not support
#' \link[base]{matrix}-column (for good reasons!).
#' Therefore, function [aggregate_fv()] must return a \link[base]{data.frame}, 
#' instead of a \link[spatstat.geom]{hyperframe}.
#' 
#' @examples
#' \dontshow{options(mc.cores = 1L)}
#' library(spatstat.geom)
#' r = seq.int(from = 0, to = 100, by = 5)
#' x0 = spatstat.data::flu |>
#'  subset(stain == 'M2-M1') 
#' x0[-19L,] |> # this element gives error..
#'  as.groupedHyperframe(group = ~ virustype/frameid) |>
#'  Gcross_(i = 'M1', j = 'M2', r = r) |>
#'  aggregate_fv(by = ~ virustype)
#' @keywords internal
#' @importFrom cli col_blue col_cyan col_magenta style_bold
#' @importFrom cli bg_br_yellow
#' @importFrom spatstat.geom names.hyperframe
#' @importFrom stats setNames
#' @export
aggregate_fv <- function(
    X, 
    by = stop('must specify `by`'),
    f_aggr_ = pmean,
    rmax,
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  if (!inherits(X, what = 'hyperframe')) stop('input must be hyperframe')
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `X` must contain at least one `fv` column')
  
  fv <- as.list.hyperframe(X)[names.hyperframe(X)[id]] # one or more 'fv' column(s)
  
  if (!missing(rmax)) {
    if (!is.numeric(rmax) || length(rmax) != 1L || is.na(rmax) || (rmax <= 0)) stop('illegal user-specified `rmax`')
  } else rmax <- numeric() # cannot use ?base::missing inside ?base::lapply
  
  setNames(nm = names(fv)) |> 
    lapply(FUN = \(nm) { # (nm = names(fv)[1L])
      
      x <- fv[[nm]]
      suppressMessages(fvcheck <- check_fvlist(x, data.name = nm))
      
      r <- fvcheck[['r']]
      rmax_fv <- fvcheck[['rmax']]
      if (rmax_fv == 0) stop('check your ppp-hypercolumn')
      
      if (!length(rmax)) { # missing user `rmax`
        if (rmax_fv < max(r)) {
          sprintf(fmt = 'Aggregation truncated at rmax(%s) = %.1f', nm, rmax_fv) |>
            style_bold() |> bg_br_yellow() |> message()
          id <- (r <= rmax_fv)
        } else id <- rep(TRUE, times = length(r)) # cannot just be `TRUE` (for later use..)
      } else if (rmax > rmax_fv) { # user `rmax > rmax_fv`
        if (rmax_fv < max(r)) {
          sprintf(fmt = 'Aggregation truncated at rmax(%s) = %.1f (user rmax = %.1f ignored)', nm, rmax_fv, rmax) |>
            style_bold() |> bg_br_yellow() |> message()
        } else {
          sprintf(fmt = 'Aggregation at maximum r(%s) = %.1f (user rmax = %.1f ignored)', nm, rmax_fv, rmax) |>
            style_bold() |> bg_br_yellow() |> message()
        }
        id <- (r <= rmax_fv)
      } else { # use user `rmax`
        sprintf(fmt = 'Aggregation truncated at rmax = %.1f for %s', rmax, nm) |>
          style_bold() |> bg_br_yellow() |> message()
        id <- (r <= rmax)
      }
      
      return(list(
        value = x |> 
          lapply(FUN = \(i) keyval.fv(i, key = fvcheck[['key1']])[id]),
        cumtrapz = x |> 
          mclapply(mc.cores = mc.cores, FUN = \(i) cumtrapz.fv(i, key = fvcheck[['key1']])[id[-1L]]), # `-1L` super important!!!
        cumvtrapz = x |> 
          mclapply(mc.cores = mc.cores, FUN = \(i) cumvtrapz.fv(i, key = fvcheck[['key1']])[id[-1L]]) # `-1L` super important!!!
      ))
      
    }) |>
    unlist(recursive = FALSE, use.names = TRUE) |> # smart!!
    aggregate_by_(X = X, by = by, f_aggr_ = f_aggr_, ...)

}

