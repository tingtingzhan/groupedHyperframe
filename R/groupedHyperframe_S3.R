

# @seealso `?nlme:::print.groupedData`
#' @importFrom spatstat.geom as.data.frame.hyperframe
#' @export
print.groupedHyperframe <- function(x, ...) {
  
  cat('Grouped Hyper Data Frame:\n\n')
  
  x |>
    getGroups.hyperframe() |>
    print.getGroups()
  cat('\n\n')
  
  # see inside ?spatstat.geom::print.hyperframe
  x |>
    as.data.frame.hyperframe(discard = FALSE) |> 
    print(...)
  
}




#' @importFrom spatstat.geom summary.hyperframe
#' @export
summary.groupedHyperframe <- function(object, ...) {
  
  z <- object |>
    summary.hyperframe()
  attr(z, which = 'group') <- object |>
    attr(which = 'group', exact = TRUE)
  attr(z, which = 'getGroups') <- object |>
    getGroups.hyperframe()
  class(z) <- c('summary.groupedHyperframe', class(z)) |>
    unique.default()
  return(z)
  
}



# @importFrom spatstat.geom print.summary.hyperframe
#' @method print summary.groupedHyperframe
#' @export
print.summary.groupedHyperframe <- function(x, ...) {

  # see inside ?spatstat.geom::print.summary.hyperframe
  
  cat('Grouped Hyper Data Frame:\n\n')
  
  x |>
    attr(which = 'getGroups', exact = TRUE) |>
    print.getGroups()
  cat('\n\n')
  
  if (any(x$storage == "dfcolumn")) {
    x$allcols |>
      print()
  } else {
    x$classes |> 
      noquote() |>
      print()
  }
  
  return(invisible())  
  
}







#' @importFrom spatstat.geom [.hyperframe
#' @export
`[.groupedHyperframe` <- function(x, ...) {
  
  ret <- `[.hyperframe`(x, ...)
  
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- c('groupedHyperframe', class(ret)) |> unique.default()
  return(ret)
  
}




#' @importFrom spatstat.geom $<-.hyperframe
#' @export
`$<-.groupedHyperframe` <- function(x, name, value) {
  
  group <- attr(x, which = 'group', exact = TRUE)
  if (name %in% all.vars(group)) stop('do not allow changing variables in grouping structure')
  
  ret <- `$<-.hyperframe`(x, name, value)
  attr(ret, which = 'group') <- group
  class(ret) <- c('groupedHyperframe', class(ret)) |> 
    unique.default()
  return(ret)
  
}

