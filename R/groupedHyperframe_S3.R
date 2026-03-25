

# @seealso `?nlme:::print.groupedData`
#' @importFrom spatstat.geom as.data.frame.hyperframe
#' @export
print.groupedHyperframe <- function(x, ...) {
  
  x |>
    attr(which = 'group', exact = TRUE) |>
    deparse1() |>
    sprintf(fmt = 'Grouped Hyper Data Frame: %s') |>
    cat()
  
  cat('\n\n')
  x |>
    get_nested_factor.groupedHyperframe() |>
    print.nested_factor()
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
  attr(z, which = 'group_size') <- object |>
    get_nested_factor.groupedHyperframe()
  class(z) <- c('summary.groupedHyperframe', class(z)) |>
    unique.default()
  return(z)
  
}



# @importFrom spatstat.geom print.summary.hyperframe
#' @method print summary.groupedHyperframe
#' @export
print.summary.groupedHyperframe <- function(x, ...) {

  # see inside ?spatstat.geom::print.summary.hyperframe
  
  x |>
    attr(which = 'group', exact = TRUE) |>
    deparse1() |>
    sprintf(fmt = 'Grouped Hyper Data Frame: %s') |>
    cat()
  
  cat('\n\n')
  x |>
    attr(which = 'group_size', exact = TRUE) |>
    print.nested_factor()
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
  
  # a super genius fix! 
  # working on the lowest function `[` :))
  # no longer needed to write
  # .. [subset.groupedHyperframe()]
  # .. probably [split.groupedHyperframe()]
  
  ret <- `[.hyperframe`(x, ...)
  
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- c('groupedHyperframe', class(ret)) |> unique.default()
  return(ret)
  
}




#' @importFrom nlme getGroupsFormula
#' @export
getGroupsFormula.groupedHyperframe <- function(object, asList, sep) {
  attr(object, which = 'group', exact = TRUE)
}


# nlme::getGroupsFormula(datasets::Formaldehyde) # returns NULL
#' @export
getGroupsFormula.hyperframe <- function(object, asList, sep) invisible()
