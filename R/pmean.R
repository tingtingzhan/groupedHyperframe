

#' @title Parallel \link[base]{mean} and \link[stats]{median}
#' 
#' @param ... \link[base]{numeric} \link[base]{vector}s
#' 
#' @param na.rm \link[base]{logical} scalar, default `TRUE`
#' 
#' 
#' @note
#' Functions [pmean()] and [pmedian()] mimic functions \link[base]{pmax.int} and \link[base]{pmin.int}.
#' They are written in a very naive way.
#' The pipeline `cbind() |> rowMeans()` is extremely slow.
#' 
#' @returns
#' Functions [pmean()] and [pmedian()] return a \link[base]{numeric} \link[base]{vector}.
#' 
#' @examples
#' pmean(1:3, 11:13, 21:23)
#' @keywords internal
#' @name pmean
#' @export
pmean <- function(..., na.rm = TRUE) {
  list(...) |>
    do.call(what = cbind) |>
    rowMeans(na.rm = na.rm)
}

#' @rdname pmean
#' @importFrom matrixStats rowMedians
#' @export
pmedian <- function(..., na.rm = TRUE) {
  list(...) |>
    do.call(what = cbind) |>
    rowMedians(na.rm = na.rm)
}

