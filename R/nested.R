

nested_ <- \(g) {
  if (is.symbol(g)) return(g)
  if (g[[1L]] == ':') return(g)
  if (g[[1L]] == '~') {
    if (length(g) == 2L) return(nested_(g[[2L]]))
    stop('only accept one-sided formula')
  }
  if (g[[1L]] == '/') { # recursive!!
    z <- as.list(g)[-1L] |> 
      lapply(FUN = nested_)
    return(z)
  }
  stop('should not come here')
}


#' @title Drop Lowest Nested Level
#' 
#' @param g an R \link[base]{language} object
#' 
#' @examples
#' drop_lowest_nested(~ g1/g2a:g2b/g3a:g3b:g3c)
#' drop_lowest_nested(~ g1)
#' drop_lowest_nested(~ g1:g2)
#' 
#' @keywords internal
#' @export
drop_lowest_nested <- \(g) {
  if (is.symbol(g)) return(invisible()) # dropped!!
  if (g[[1L]] == ':') return(invisible()) # dropped!!
  if (g[[1L]] == '/') return(as.list(g)[[2L]]) # beautiful!!!
  if (g[[1L]] == '~') { # recursive!!
    if (length(g) != 2L) stop('only accept one-sided formula')
    g0 <- g[[2L]] |>
      drop_lowest_nested()
    if (!length(g0)) return(invisible())
    g[[2L]] <- g0
    return(g)
  }
  stop('should not come here')
}



#' @title Get Nested Groups
#' 
#' @param by an R \link[base]{language} object, (nested) grouping structure
#' 
#' @returns 
#' The function [get_nested()] returns a \link[base]{list} of \link[base]{language} object(s).
#' 
#' @examples
#' get_nested(~ g1)
#' get_nested(~ g1:g2)
#' get_nested(~ g1/g2a:g2b/g3a:g3b:g3c)
#' 
#' library(nlme)
#' nlme::Wafer |>
#'  getGroupsFormula() |>
#'  get_nested()
#' 
#' @keywords internal
#' @export
get_nested <- function(by) {
  
  z <- by |>
    nested_() |> 
    unlist()
  if (!is.list(z)) z <- list(z)
  
  names(z) <- z |>
    vapply(FUN = deparse1, FUN.VALUE = NA_character_)
  
  # class(z) <- c('nested') # do I want to do this?
  return(z)
  
}


#' @title Get Nested Factors
#' 
#' @param data see *Usage*
#' 
#' @param by an R \link[base]{language} object, see function [get_nested]
#' 
#' @returns
#' The `S3` generic function [get_nested_factor()] returns 
#' a \link[base]{list} of \link[base]{factor}s, 
#' ready to be passed into the parameter `by` of the function
#' \link[stats]{aggregate.data.frame}.
#' 
#' @examples
#' nlme::Wafer |>
#'  get_nested_factor()
#' 
#' spatstat.data::osteo |>
#'  get_nested_factor(by = ~ id/brick)
#' 
#' @keywords internal
#' @export
get_nested_factor <- function(data, by) UseMethod(generic = 'get_nested_factor')

#' @export
get_nested_factor.data.frame <- function(data, by) {
  ret <- by |>
    get_nested() |>
    lapply(FUN = \(g) {
      if (is.symbol(g)) {
        z <- data[[g]]
        if (is.factor(z)) return(factor(z)) # drop empty levels!!
        return(factor(z, levels = unique(z)))
      }
      z <- data[all.vars(g)] |>
        interaction(drop = TRUE, sep = '.', lex.order = TRUE) # must be 'factor'
      return(z)
    })
  class(ret) <- c('nested_factor', class(ret)) |>
    unique.default()
  return(ret)
}

#' @importFrom nlme getGroupsFormula
#' @export
get_nested_factor.groupedData <- function(data, by = getGroupsFormula(data)) {
  get_nested_factor.data.frame(data = data, by = by)
}


#' @export
get_nested_factor.hyperframe <- function(data, by) {
  get_nested_factor.data.frame(data = unclass(data)$df, by = by)
}


#' @export
get_nested_factor.groupedHyperframe <- function(data, by = getGroupsFormula.groupedHyperframe(data)) {
  get_nested_factor.hyperframe(data = data, by = by)
}


#' @export
print.nested_factor <- \(x, ...) {
  
  ns <- x |>
    unclass() |>
    Reduce(f = list, accumulate = TRUE) |>
    vapply(FUN = \(i) { # (i = 1L)
      i |>
        interaction(drop = TRUE, lex.order = TRUE) |>
        levels() |>
        length()
    }, FUN.VALUE = NA_integer_) # names dropped by ?base::vapply
  
  mapply(
    FUN = \(n, g) {
      paste(n, g |> col_blue() |> style_bold())
    }, n = ns, g = names(x), SIMPLIFY = TRUE
  ) |> 
    rev.default() |> 
    paste(collapse = ' nested in\n') |>
    cat()
  
}
