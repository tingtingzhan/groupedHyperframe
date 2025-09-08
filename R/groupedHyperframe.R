

#' @title Print [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [print.groupedHyperframe()] does not have a returned value.
#' 
#' @keywords internal
#' @importFrom spatstat.geom as.data.frame.hyperframe
#' @importFrom utils head
#' @export print.groupedHyperframe
#' @export
print.groupedHyperframe <- function(x, ...) {
  
  # @seealso `?nlme:::print.groupedData`
  
  'Grouped Hyperframe: ' |> cat()
  group <- attr(x, which = 'group', exact = TRUE)
  print(group, ...)
  
  f <- group |> 
    get_nested_factors(data = x)
  ns <- f |> 
    seq_along() |> 
    vapply(FUN = \(i) { # (i = 1L)
      f[seq_len(i)] |>
        interaction(drop = TRUE, lex.order = TRUE) |>
        levels() |>
        length()
    }, FUN.VALUE = NA_integer_) # names dropped by ?base::vapply
    
  cat('\n')
  mapply(FUN = \(n, g) {
    paste(n, g |> col_blue() |> style_bold())
  }, n = ns, g = names(f), SIMPLIFY = TRUE) |> 
    rev.default() |> 
    cat(sep = ' nested in\n')
  
  '\nPreview of first 10 (or less) rows:\n\n' |> col_magenta() |> style_bold() |> cat()
  # see inside ?spatstat.geom::print.hyperframe
  x |>
    as.data.frame.hyperframe(discard = FALSE) |> 
    head(n = 10L) |>
    print(...)
  
}








#' @title Extract Subset of [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters of \link[spatstat.geom]{[.hyperframe}
#' 
#' @returns
#' Function \link{[.groupedHyperframe} returns a [groupedHyperframe] or a \link[spatstat.geom]{hyperframe}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom [.hyperframe
#' @export [.groupedHyperframe
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




# @title Extract Grouping Formula from [groupedHyperframe]
# @description ..
# @param object a [groupedHyperframe]
# @param asList,sep place holders for S3 generic \link[nlme]{getGroupsFormula}
# @returns 
# Function [getGroupsFormula.groupedHyperframe()] returns a one-sided \link[stats]{formula}
# @note
# tzh mask this for now, does not want to import(nlme) only for this
# @keywords internal
# @importFrom nlme getGroupsFormula
# @export getGroupsFormula.groupedHyperframe
# @export
#getGroupsFormula.groupedHyperframe <- function(object, asList, sep) {
#  attr(object, which = 'group', exact = TRUE)
#}

