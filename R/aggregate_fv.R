
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
#' library(spatstat.data)
#' library(spatstat.geom)
#' flu$pattern[] = flu$pattern |> 
#'  lapply(FUN = `mark_name<-`, value = 'stain') # read ?flu carefully
#' r = seq.int(from = 0, to = 100, by = 5)
#' x0 = flu |>
#'  subset(stain == 'M2-M1') 
#' x0[-19L,] |> # this element gives error..
#'  as.groupedHyperframe(group = ~ virustype/frameid) |>
#'  Gcross_(i = 'M1', j = 'M2', r = r, correction = 'best') |>
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
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  if (!inherits(X, what = 'hyperframe')) stop('input must be hyperframe')
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `X` must contain at least one `fv` column')
  
  fv <- as.list.hyperframe(X)[names.hyperframe(X)[id]] # one or more 'fv' column(s)
  
  setNames(nm = names(fv)) |> 
    lapply(FUN = \(nm) { # (nm = names(fv)[1L])
      
      x <- fv[[nm]]
      suppressMessages(fvcheck <- check_fvlist(x, data.name = nm))
      
      r <- fvcheck[['r']]
      if (length(rmax <- fvcheck[['rmax']])) {
        min_rmax <- min(rmax)
        if (min_rmax == 0) stop('check your ppp-hypercolumn')
        sprintf(fmt = 'Aggregation truncated at rmax(%s) = %.1f', nm, min_rmax) |>
          style_bold() |>
          bg_br_yellow() |>
          message()
        id <- (r <= min_rmax)
      } else id <- TRUE
      
      return(list(
        value = x |> 
          lapply(FUN = \(i) key1val.fv(i)[id]),
        cumtrapz = x |> 
          mclapply(mc.cores = mc.cores, FUN = \(i) cumtrapz.fv(i)[id])
      ))
      
    }) |>
    unlist(recursive = FALSE, use.names = TRUE) |> # smart!!
    aggregate_by_(X = X, by = by, f_aggr_ = f_aggr_, ...)

}

