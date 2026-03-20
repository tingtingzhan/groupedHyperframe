

# ?cli::cli_text()
# The text to show .. will be concatenated into a single string. 
# Newlines are `not` preserved.

if (FALSE) {
  cli_RPubs_(pub = 'groupedHyperframe')
  cli_QuartoPub_(pub = 'groupedhyperframe')
  cli_QuartoPub_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
  cli_Netlify_(pub = 'groupedhyperframe')
  cli_Netlify_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
  cli_doi_('10.1002/bimj.4710230408')
}

cli_doi_ <- \(doi) {
  # `x`: 'character' scalar of doi
  sprintf(fmt = '{.href [doi:%s](https://doi.org/%s)}', doi, doi) |>
    cli_text()
}


cli_book_ <- \(
    fmt,
    author = 'tingtingzhan', 
    pub, 
    chapter, print_chapter = TRUE
) {    
  if (missing(chapter) || !print_chapter) {
    paste0('{.url ', fmt, '}') |>
      sprintf(fmt = _, author, pub) |>
      cli_text()
  } else {
    paste0('{.url ', fmt, '/%s.html}') |>
      sprintf(fmt = _, author, pub, chapter) |>
      cli_text()
  }
}



cli_QuartoPub_ <- \(...) cli_book_(fmt = 'https://%s.quarto.pub/%s', ...)

cli_Netlify_ <- \(...) cli_book_(fmt = 'https://%s-%s.netlify.app', ...)

cli_RPubs_ <- \(...) cli_book_(fmt = '{.url https://rpubs.com/%s/%s}', ..., print_chapter = FALSE) 



#' @title Defunct Functions
#' 
#' @description
#' The functions mentioned in hard-copy journals, but later \link[base]{.Defunct}.
#' 
#' @param ... Defunct parameters
#' 
#' @keywords internal
#' @name defunct
#' @export
aggregate_quantile <- function(...) {
  
  new. <- '<groupedHyperframe> |> quantile() |> aggregate()'
  
  match.call()[[1L]] |> deparse1() |> 
    sprintf(fmt = '%s()') |>
    col_cyan() |> style_bold() |>
    sprintf(fmt = 'Function %s described in') |> message()
  cli_doi_('10.1093/bioinformatics/btaf430')
  'has been replaced by pipeline' |> message()
  
  new. |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette (mirrors) for details' |> message()
  cli_QuartoPub_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
  cli_Netlify_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')

  .Defunct(new = new.)
  
}






