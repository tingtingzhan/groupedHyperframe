

#' @title `S3` methods table in vignette
#' 
#' @param class \link[base]{character} scalar
#' 
#' @param package \link[base]{character} scalar
#' 
#' @param package_pattern \link[base]{character} scalar of \link[base]{regex}
#' 
#' @param backtick \link[base]{logical} scalar, whether to put backticks around function names.
#' Default `TRUE` for Markdown/Quarto rendering.
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
methods2kable <- function(class, package, package_pattern, backtick = TRUE, ...) {
  
  if (!missing(package)) {
    cl <- quote(from == package)
    kcaption <- sprintf(fmt = '`S3` method dispatches `%s::*.%s`', package, class)
  } else if (!missing(package_pattern)) {
    cl <- quote(grepl(pattern = package_pattern, x = from))
    kcaption <- NULL # lazy way out :))
  } else stop('unspecified `package`')
  
  x <- methods(class = class, ...) |> 
    attr(which = 'info', exact = TRUE) |>
    subset.data.frame(subset = eval(cl)) |>
    within.data.frame(expr = {
      generic = generic |> 
        vapply(FUN = .full_generic, backtick = backtick, FUN.VALUE = '')
    })
  
  if (backtick) {
    rownames(x) <- x |>
      rownames() |> 
      sprintf(fmt = '`%s`')
  }
  
  x |> 
    kable(caption = kcaption)
  
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