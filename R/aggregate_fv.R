
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
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
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
  nm <- names.hyperframe(X)[id]
  
  if (!missing(rmax)) {
    if (!is.numeric(rmax) || length(rmax) != 1L || is.na(rmax) || (rmax <= 0)) stop('illegal user-specified `rmax`')
  } else rmax <- numeric() # cannot use ?base::missing inside ?base::mapply
  
  (as.list.hyperframe(X)[nm]) |>
    mapply(
      FUN = summary.fvlist, 
      object = _, data.name = nm, 
      MoreArgs = list(rmax = rmax, mc.cores = mc.cores), SIMPLIFY = FALSE
    ) |> 
    unlist(recursive = FALSE, use.names = TRUE) |> # smart!!
    aggregate_by_(X = X, by = by, f_aggr_ = f_aggr_, ...)

}

