
getGroups_df_ <- \(object, form, ...) {
  # tzh does not know how to force
  # ?nlme:::getGroups.data.frame
  # to return a data.frame
  
  z <- getGroups( # ?nlme:::getGroups.data.frame
    object = object, form = form, ...
  )
  
  if (!is.data.frame(z)) {
    z <- data.frame(z)
    if (!is.symbol(form[[2L]])) stop('do not allow')
    names(z) <- as.character(form[[2L]])
  }
  
  z |>
    structure(class = c('getGroups', 'data.frame'))
  
}




# nlme::getGroupsFormula(datasets::Formaldehyde) # returns NULL
#' @importFrom nlme getGroupsFormula
#' @export
getGroupsFormula.hyperframe <- function(object, asList, sep) {
  attr(object, which = 'group', exact = TRUE) # NULL is okay
}


#' @importFrom nlme getGroups getGroupsFormula
#' @export
getGroups.hyperframe <- function(
    object, 
    form = getGroupsFormula(object), # tzh's un-exported [getGroupsFormula.hyperframe()]
    level, 
    data, 
    sep = '/'
) {
  unclass(object)$df |>
    getGroups_df_(
      object = _, form = form, level = level, sep = sep
    )
}


#' @export
print.getGroups <- \(x, ...) {
  
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

