

#' @import cli
#' @import patchwork
#' 
#' @import stats
#' @import survival
'_PACKAGE'

if (FALSE) {
  # collision between
  ?patchwork::area
  ?spatstat.geom::area
  # and we even have
  ?MASS::area
  # one bandage fix is to import 
  spatstat.geom::area.owin
}


# citation(auto = packageDescription('groupedHyperframe'))
# requires installed package from CRAN..
# ?utils::citation does *not* need to Imports the package!!

#' @importFrom utils citation packageVersion packageDate packageDescription
.onAttach <- function(libname, pkgname) {
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  'Welcome to R Package {.href [groupedHyperframe](https://cran.r-project.org/package=groupedHyperframe)}' |>
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'version %s (%s) -- %s',
    'groupedHyperframe' |> packageVersion(),
    'groupedHyperframe' |> packageDate(),
    'groupedHyperframe' |> 
      packageDescription() |> 
      getElement(name = 'Nickname') |>
      dQuote() |>
      #style_bold() |>
      bg_br_yellow()
  ) |>
    cli_inform(class = 'packageStartupMessage')

  if (FALSE) {
    'groupedHyperframe' |> 
      packageDescription() |> 
      getElement(name = 'Authors@R') |>
      str2lang() |>
      eval() |>
      format() |> # ?utils:::format.person; ORCID logo does *not* show
      cli_inform(class = 'packageStartupMessage')
  }
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')

  sprintf(
    fmt = 'Created by {.href [%s](https://github.com/tingtingzhan/groupedHyperframe)}',
    'Tingting Zhan' |> 
      col_red() |> 
      style_bold()
  ) |>
    cli_inform(class = 'packageStartupMessage')
  
  # '\n' |>
  #  cli_inform(class = 'packageStartupMessage')

  sprintf(
    fmt = 'For technical details, see the {.href [%s](https://tingtingzhan.quarto.pub/groupedhyperframe/)}',
    'vignettes' |> 
      col_blue() |> 
      style_bold()
  ) |>
    # style_italic() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  'To cite this package in publications please use:' |>
    cli_inform(class = 'packageStartupMessage')
    
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  'groupedHyperframe' |>
    citation() |>
    format(style = 'text') |> # utils:::format.citation
    col_green() |>
    style_bold() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  'Active development of this R package is temporarily suspended. Until further notice, it should not be used as a basis for research grant applications or cited as an actively maintained tool.' |>
    col_magenta() |>
    style_bold() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
}



