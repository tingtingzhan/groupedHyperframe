


#' @title Citations of \pkg{groupedHyperframe} and its Applications
#' 
#' @keywords internal
#' @name bib
#' @importFrom utils bibentry
#' @export
bib_btaf430 <- function() {
  bibentry(
    bibtype = 'article',
    title = 'Quantile Index predictors using R package hyper.gam',
    author = c('Tingting Zhan', 'Misung Yi', 'Inna Chervoneva' ),
    journal = 'Bioinformatics',
    pages = 'btaf430',
    year = '2025', 
    month = '07',
    issn = '1367-4811',
    doi = '10.1093/bioinformatics/btaf430'
  )
}


#' @rdname bib
#' @importFrom utils available.packages
#' @export
bib_groupedHyperframe <- function() {
  ap <- available.packages(repos = 'https://cran.rstudio.com/') |> # 'matrix'
    as.data.frame.matrix()
  # using base::subset or base::subset.data.frame may both trigger ?devtools::check NOTE
  v <- ap$Version[ap$Package == 'groupedHyperframe']
  
  bibentry(
    bibtype = 'Manual',
    title = 'groupedHyperframe: Grouped Hyper Data Frame: An Extension of Hyper Data Frame',
    author = c('Tingting Zhan', 'Inna Chervoneva'),
    year = '2025', # Sys.Date() |> as.POSIXlt() |> then??? (without imports lubridate::year)
    note = v |> sprintf(fmt = 'R package version %s'),
    url = 'https://CRAN.R-project.org/package=groupedHyperframe',
    doi = '10.32614/CRAN.package.groupedHyperframe'
  )
}


#' @rdname bib
#' @importFrom utils available.packages
#' @export
bib_hypergam <- function() {
  ap <- available.packages(repos = 'https://cran.rstudio.com/') |> # 'matrix'
    as.data.frame.matrix()
  v <- ap$Version[ap$Package == 'hyper.gam']
  
  bibentry(
    bibtype = 'Manual',
    title = 'hyper.gam: Generalized Additive Models with Hyper Column',
    author = c('Tingting Zhan', 'Inna Chervoneva'),
    year = '2025',
    note = v |> sprintf(fmt = 'R package version %s'),
    url = 'https://CRAN.R-project.org/package=hyper.gam',
    doi = '10.32614/CRAN.package.hyper.gam'
  )
}