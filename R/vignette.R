

#' @title `S3` methods table in vignette
#' 
#' @param class \link[base]{character} scalar
#' 
#' @param package \link[base]{character} scalar
#' 
#' @param ... additional parameters of function \link[utils]{methods}
#' 
#' @returns 
#' Function [methods2kable()] returns a \link[base]{data.frame}.
#' 
#' @keywords internal
#' @importFrom utils methods
#' @importFrom knitr kable
#' @export
methods2kable <- function(class, package, ...) {
  x <- methods(class = class, ...) |> 
    attr(which = 'info', exact = TRUE) |>
    subset.data.frame(subset = eval(quote(from == package))) |>
    within.data.frame(expr = {
      generic = generic |> 
        vapply(FUN = .full_generic, backtick = TRUE, FUN.VALUE = '')
    })
  rownames(x) <- x |>
    rownames() |> 
    sprintf(fmt = '`%s`')
  x |> 
    kable(caption = sprintf(fmt = '`S3` method dispatches `%s::*.%s`', package, class))
}



.full_generic <- function(x, backtick = TRUE) {
  # `x` is 'character' scalar; # x = 'quantile'
  
  .sugar <- endsWith(x, '<-')
  
  fn <- x |> 
    #parse(text = _) |> eval() |> # base::parse() cannot deal with 'names<-'
    get()
  
  if (is.primitive(fn)) nm <- 'base' else {
    
    ev <- fn |>
      environment()
    if (!isNamespace(ev)) stop(x, 'dont support yet..')
    nm <- ev |> 
      getNamespaceName()
    
  }
  
  z <- nm |>
    sprintf(fmt = if (.sugar) '%s::`%s`' else '%s::%s', x)
  
  if (!backtick) return(z)
  
  z |>
    sprintf(fmt = if (.sugar) '`` %s ``' else '`%s`')
  
}