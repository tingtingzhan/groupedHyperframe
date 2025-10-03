

#' @title `S3` methods table in vignette
#' 
#' @param class \link[base]{character} scalar
#' 
#' @param package \link[base]{character} scalar
#' 
#' @param ... additional parameters of function \link[utils]{methods}
#' 
#' @returns 
#' Function [vignette_methods()] returns a \link[base]{data.frame}.
#' 
#' @keywords internal
#' @importFrom utils methods
#' @export
vignette_methods <- function(class, package, ...) {
  x <- methods(class = class, ...) |> 
    attr(which = 'info', exact = TRUE) |>
    subset.data.frame(subset = eval(quote(from == package))) |>
    within.data.frame(expr = {
      generic = generic |> sprintf(fmt = '`%s`')
    })
  rownames(x) <- x |>
    rownames() |> 
    sprintf(fmt = '`%s`')
  return(x)
}