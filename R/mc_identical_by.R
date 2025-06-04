
#' @title Check if Data Column is Identical by Grouping
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param f \link[base]{factor}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
#' CRAN requires `mc.cores <= 2L` in examples.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' Function \link[stats]{aggregate.data.frame} does not do parallel computing.
#' 
#' Function `collapse::collap` does not support \link[survival]{Surv} column.
#' 
#' @keywords internal
#' @importFrom cli col_blue
#' @importFrom parallel mclapply detectCores
#' @export
mc_identical_by <- function(
    data, 
    f,
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  nr <- .row_names_info(data, type = 2L)
  if (nr != length(f)) stop('`data` and `f` different length')
  
  ids <- nr |> seq_len() |> split.default(f = f)
  
  .ident <- data |>
    vapply(FUN = \(d) { # (d = data[[1L]])
      ids |> 
        mclapply(mc.cores = mc.cores, FUN = \(i) {
          all(duplicated(unclass(d[i]))[-1L]) # column `d` identical within split `i`
        }) |> 
        unlist(use.names = FALSE) |> # column `d` identical within all splits
        all()
    }, FUN.VALUE = NA)
  
  if (any(!.ident)) {
    nm <- names(data)[!.ident]
    #nm |> 
    #  col_blue() |> 
    #  paste(collapse = ';') |>
    #  sprintf(fmt = 'Column(s) %s removed; as they are not identical per aggregation-group') |>
    #  message() # choose not to print this message
    data[nm] <- NULL
  } else nm <- NULL
  
  ret <- data[vapply(ids, FUN = `[`, 1L, FUN.VALUE = NA_integer_), , drop = FALSE]
  # do.call(rbind.data.frame, args = .) # ?base::rbind.data.frame does not respect 'Surv', etc.
  .rowNamesDF(ret) <- NULL
  attr(ret, which = 'non_identical') <- nm
  return(ret)
  
}

