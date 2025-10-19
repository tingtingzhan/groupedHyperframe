

# ?cli::cli_text()
# The text to show .. will be concatenated into a single string. 
# Newlines are `not` preserved.

#' @title \link[base]{.Defunct} Messages using Package \CRANpkg{cli}
#' 
#' @description
#' Internal helper functions, 
#' to display beautiful \link[base]{.Defunct} messages using package \CRANpkg{cli}.
#' 
#' @param author \link[base]{character} scalar
#' 
#' @param pub \link[base]{character} scalar
#' 
#' @param chapter \link[base]{character} scalar
#' 
#' @param doi \link[base]{character} scalar
#' 
#' @examples
#' cli_RPubs_(pub = 'groupedHyperframe')
#' cli_QuartoPub_(pub = 'groupedhyperframe')
#' cli_QuartoPub_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
#' cli_doi_('10.1002/bimj.4710230408')
#' 
#' @keywords internal
#' @name cli_
#' @export
cli_RPubs_ <- function(author = 'tingtingzhan', pub) {
  sprintf(fmt = '{.url https://rpubs.com/%s/%s}', author, pub) |>
    cli_text()
}

#' @rdname cli_
#' @export
cli_doi_ <- function(doi) {
  # `x`: 'character' scalar of doi
  sprintf(fmt = '{.href [doi:%s](https://doi.org/%s)}', doi, doi) |>
    cli_text()
}


#' @rdname cli_
#' @export
cli_QuartoPub_ <- function(author = 'tingtingzhan', pub, chapter) {
  if (missing(chapter)) {
    sprintf(fmt = '{.url https://%s.quarto.pub/%s}', author, pub) |>
      cli_text()
  } else {
    sprintf(fmt = '{.url https://%s.quarto.pub/%s/%s.html}', author, pub, chapter) |>
      cli_text()
  }
  
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
  
  match.call()[[1L]] |> deparse1() |> col_cyan() |> style_bold() |>
    sprintf(fmt = 'Function %s described in') |> message()
  cli_doi_('10.1093/bioinformatics/btaf430')
  'has been replaced by pipeline' |> message()
  
  new |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette for details' |> message()
  cli_RPubs_(pub = 'groupedHyperframe')

  .Defunct(new = new)
  
}


#' @rdname defunct
#' @export
aggregate_fv <- function(new = '<groupedHyperframe> |> summary_fv() |> aggregate()') {
  
  match.call()[[1L]] |> deparse1() |> col_cyan() |> style_bold() |>
    sprintf(fmt = 'Function %s has been replaced by pipeline') |> message()
  
  new |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette for details' |> message()
  cli_RPubs_(pub = 'groupedHyperframe')
  
  .Defunct(new = new)

}



