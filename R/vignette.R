

#' @title `S3` methods table in vignette
#' 
#' @param generic.function,class see function \link[utils]{methods}
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
methods2kable <- function(generic.function, class, package, package_pattern, backtick = TRUE, ...) {
  
  if (!missing(package)) {
    cl <- quote(from %in% package)
    kcaption <- if (!missing(generic.function)) {
      if (length(generic.function) > 1L) {
        sprintf(fmt = '`%s::%s.*`', package, generic.function) |>
          paste(collapse = ', ') |>
          sprintf(fmt = '`S3` methods %s (v%s)', . = _, packageVersion(package))
      } else {
        sprintf(fmt = '`S3` methods `%s::%s.*` (v%s)', package, generic.function, packageVersion(package))
      }
    } else if (!missing(class)) {
      sprintf(fmt = '`S3` methods `%s::*.%s` (v%s)', package, class, packageVersion(package))
    } else stop()
  } else if (!missing(package_pattern)) {
    cl <- quote(grepl(pattern = package_pattern, x = from))
    kcaption <- NULL # lazy way out :))
  } else stop('unspecified `package`')
  
  MFinfo <- function(...) {
    methods(...) |> 
      attr(which = 'info', exact = TRUE)
  }
  
  mf_info <- if (!missing(generic.function)) {
    if (length(generic.function) > 1L) {
      generic.function |>
        lapply(FUN = MFinfo, ...) |>
        do.call(what = rbind.data.frame, args = _)
    } else {
      MFinfo(generic.function = generic.function, ...)
    }
  } else if (!missing(class)) {
    MFinfo(class = class, ...)
  } else stop()
  
  x <- mf_info |>
    subset.data.frame(subset = eval(cl)) |>
    within.data.frame(expr = {
      
      if (length(from) > 1L & all(duplicated.default(from)[-1L])) {
        from <- NULL
      }
      
      if (length(generic) > 1L & all(duplicated.default(generic)[-1L])) {
        generic <- NULL
      } else {
        generic <- generic |> 
          vapply(FUN = .full_generic, backtick = backtick, FUN.VALUE = '')
      }
      
    })
  
  if (backtick) {
    rownames(x) <- x |>
      rownames() |> 
      sprintf(fmt = '`%s`')
  }
  
  x |> 
    kable(caption = kcaption)
  
}



#' @importFrom methods isGroup
.full_generic <- function(x, backtick = TRUE) {
  # `x` is 'character' scalar; # x = 'quantile'
  
  .sugar <- endsWith(x, '<-')
  
  fn <- x |> 
    #parse(text = _) |> eval() |> # base::parse() cannot deal with 'names<-'
    get()
  
  if (is.primitive(fn)) {
    nm <- 'base' 
  } else if (isGroup(x)) { # groupGeneric
    nm <- 'methods'
  } else {
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