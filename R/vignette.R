

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
#' @importFrom utils methods packageVersion
#' @importFrom knitr kable
#' @export
methods2kable <- function(class, package, package_pattern, backtick = TRUE, ...) {
  
  if (!missing(package)) {
    cl <- quote(from == package)
    kcaption <- sprintf(fmt = '`S3` method dispatches `%s::*.%s` (v%s)', package, class, packageVersion(package))
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


#' @title [Namespace2methodsInfo()]
#' 
#' @param ns \link[base]{character} scalar, package name, e.g., `'stats'`
#' 
#' @param ... parameters of function \link[utils]{.S3methods}, other than `envir` and `useEnv`
#' 
#' @examples
#' Namespace2methodsInfo(ns = 'stats', class = 'data.frame')
#' 
#' @keywords internal
#' @importFrom utils .S3methods
#' @export
Namespace2methodsInfo <- function(ns, ...) {
  ns |> 
    getNamespace() |>
    .S3methods(..., envir = _, useEnv = TRUE) |>
    attr(which = 'info', exact = TRUE) |>
    subset.data.frame(subset = eval(quote(from == 'envir'))) |>
    within.data.frame(expr = {
      from <- ns
    })
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