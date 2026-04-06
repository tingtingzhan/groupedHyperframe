
# stop('Is aggregate() |> hyperframe() already supported?  If not, suggest to Adrian')

# stop('write aggregate.groupedData() to pkg{ranef.tzh}')


if (FALSE) {
  # as of # packageDate('nlme') # 2026-03-27
  # ?aggregate.groupedData not in \pkg{nlme}
  d = data(package = 'nlme') |> 
    packageIQR::dataFrom()
  
  d |> 
    sapply(FUN = nrow) |>
    sort()
  
  nlme::Rail |>
    aggregate.data.frame(
      by = list(Rail = nlme::Rail$Rail), 
      FUN = unique,
      simplify = FALSE
    )
  
  x = nlme::Rail |>
    aggregate(x = travel ~ Rail, data = _, FUN = unique, simplify = FALSE)
  x
  x$travel |> class() # 'list'
  
  suppressPackageStartupMessages(library(spatstat))
  x |>
    spatstat.geom::as.hyperframe.data.frame() # wow!!!!!
  
}




#' @title Aggregate to (Grouped) Hyper Data Frame
#' 
#' @param x see **Usage**
#' 
#' @param by \link[stats]{formula}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export
aggregate2hyper <- function(x, by, ...) UseMethod(generic = 'aggregate2hyper')


#' @importFrom spatstat.geom as.hyperframe.data.frame hyperframe cbind.hyperframe
#' @export
aggregate2hyper.data.frame <- function(x, by, ...) {
  
  by. <- getGroups_df_(object = x, form = by) |>
    as.list.data.frame()
    
  by_seq <- seq_along(by.)
  
  # drop unused factor levels in all columns of `x`
  x[] <- x |>
    lapply(FUN = \(i) {
      if (!is.factor(i)) return(i)
      factor(i) # drop empty levels!!
    })
  
  tmp <- aggregate.data.frame(x = x, by = by., FUN = unique, simplify = TRUE, drop = TRUE)
  # the first couple columns are created based on `by.`
  tmp <- tmp[- by_seq]
  
  orig_class <- x |> lapply(FUN = class)
  tmp_class <- tmp |> lapply(FUN = class)
  id <- which(!mapply(FUN = identical, orig_class, tmp_class)) # columns to be turned into hypercolumns
  
  if (!length(id)) {
    
    hf <- as.hyperframe.data.frame(tmp)
      
  } else {
    
    id |>
      names() |>
      col_magenta() |> style_bold() |>
      paste(collapse = ', ') |>
      sprintf(fmt = 'Hypercolumn(s) %s created!') |>
      message()
    
    tmp2 <- x[id] |>
      aggregate.data.frame(by = by., FUN = c, simplify = FALSE, drop = TRUE)
    tmp2 <- tmp2[- by_seq]
    
    hf <- cbind.hyperframe(
      as.hyperframe.data.frame(tmp[-id]), # data.frame part
      as.hyperframe.data.frame(tmp2)
    )
    
  }

  #by0 <- by |>
  #  drop_lowest_nested()
  #if (length(by0)) {
  #  attr(hf, which = 'group') <- by0
  #  class(hf) <- c('groupedHyperframe', class(hf)) |> 
  #    unique.default()
  #}
  
  return(hf)
  
}


#' @importFrom nlme getGroupsFormula
#' @export
aggregate2hyper.groupedData <- function(x, by = getGroupsFormula(x), ...) {
  .Defunct(msg = 'remove this example')
  aggregate2hyper.data.frame(x = x, by = by, ...)
}
