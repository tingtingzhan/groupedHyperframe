

Rpubs_tzh <- function(x) {
  x |>
    sprintf(fmt = '{.url https://rpubs.com/tingtingzhan/%s}')
}

doi_link_ <- function(doi) {
  # doi is character scalar
  sprintf(fmt = '{.href [doi:%s](http://doi.org/%s)}', doi, doi)
}



#' @title Defunct Functions
#' 
#' @description
#' Functions mentioned in hard-copy journals, but later \link[base]{.Defunct}.
#' 
#' @keywords internal
#' @name defunct
#' @export
aggregate_quantile <- function(...) {
  
  # ?cli::cli_text()
  # The text to show .. will be concatenated into a single string. 
  # Newlines are `not` preserved.
  
  'Function aggregate_quantile() described in' |>
    message()
  
  doi_link_('10.1093/bioinformatics/btaf430') |>
    cli_text()
  
  'has been replaced by pipeline' |> 
    message()
  
  '<groupedHyperframe> |> quantile() |> aggregate()' |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette for details' |> 
    message()
  
  Rpubs_tzh('groupedHyperframe') |>
    cli_text()

  .Defunct(new = '<groupedHyperframe> |> quantile() |> aggregate()')
  
}


#' @rdname defunct
#' @export
aggregate_fv <- function(...) {
  
  'Function aggregate_fv() has been replaced by pipeline' |>
    message()
  
  '<groupedHyperframe> |> summary_fv() |> aggregate()' |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette for details' |> 
    message()
  
  Rpubs_tzh('groupedHyperframe') |>
    cli_text()
  
  .Defunct(new = '<groupedHyperframe> |> summary_fv() |> aggregate()')

}



