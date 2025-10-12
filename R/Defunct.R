

# ?cli::cli_text()
# The text to show .. will be concatenated into a single string. 
# Newlines are `not` preserved.

#' @title Defunct Messages using Package \CRANpkg{cli}
#' 
#' @param x \link[base]{character} scalar
#' 
#' @keywords internal
#' @name cli_defunct_
#' @export
cli_RPubs_ <- function(x) {
  x |>
    sprintf(fmt = '{.url https://rpubs.com/%s}') |>
    cli_text()
}

#' @rdname cli_defunct_
#' @export
cli_doi_ <- function(x) {
  # `x`: 'character' scalar of doi
  sprintf(fmt = '{.href [doi:%s](https://doi.org/%s)}', x, x) |>
    cli_text()
}



#' @title Defunct Functions
#' 
#' @description
#' Functions mentioned in hard-copy journals, but later \link[base]{.Defunct}.
#' 
#' @param new \link[base]{character} scalar, see function \link[base]{.Defunct}.
#' 
#' @keywords internal
#' @name defunct
#' @export
aggregate_quantile <- function(new = '<groupedHyperframe> |> quantile() |> aggregate()') {
  
  'Function aggregate_quantile() described in' |>
    message()
  
  cli_doi_('10.1093/bioinformatics/btaf430')
  
  'has been replaced by pipeline' |> 
    message()
  
  new |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette for details' |> 
    message()
  
  cli_RPubs_('tingtingzhan/groupedHyperframe')

  .Defunct(new = new)
  
}


#' @rdname defunct
#' @export
aggregate_fv <- function(new = '<groupedHyperframe> |> summary_fv() |> aggregate()') {
  
  'Function aggregate_fv() has been replaced by pipeline' |>
    message()
  
  new |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette for details' |> 
    message()
  
  cli_RPubs_('tingtingzhan/groupedHyperframe')
  
  .Defunct(new = new)

}



