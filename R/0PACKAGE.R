

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
    # cli_rule(center = _) |> # um, has issue
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'version %s (%s) -- %s',
    'groupedHyperframe' |> packageVersion(),
    'groupedHyperframe' |> packageDate(),
    # 'groupedHyperframe' |> 
    #  packageDescription() |> 
    #  getElement(name = 'Nickname') |> # "Nickname" field not allowed on CRAN
    'Entertaining-but-Useless' |> 
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
  
  'This package is a one-person project undergoing rapid evolution. {.href [Backward compatibility](https://en.wikipedia.org/wiki/Backward_compatibility)} is provided as a courtesy rather than a guarantee.' |>
    col_grey() |>
    cli_inform(class = 'packageStartupMessage')  
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'Until further notice, it should %s be used as the basis for research grant applications, referenced in final research progress reports, or cited as an actively maintained tool in a peer-reviewed manuscript, %s should it be used to support or fulfill requirements for pursuing an academic degree.',
    'not' |> style_bold() |> style_underline() |> bg_br_yellow(),
    'nor' |> style_bold() |> style_underline() |> bg_br_yellow()
  ) |>
    col_grey() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'In addition, work %s based on this package should %s be presented at academic conferences or similar scholarly venues.',
    'primarily' |> style_bold() |> style_underline(),
    'not' |> style_bold() |> style_underline() |> bg_br_yellow()
  ) |>
    col_grey() |>
    cli_inform(class = 'packageStartupMessage')

  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'Furthermore, a person\'s %s these packages does not necessarily indicate an understanding of their underlying mechanisms. Accordingly, demonstration of their use %s should %s be considered sufficient evidence of expertise, %s should it be credited as a basis for academic promotion or advancement.',
    'ability to use' |> style_bold() |> style_underline(),
    'alone' |> style_bold() |> style_underline(),
    'not' |> style_bold() |> style_underline() |> bg_br_yellow(),
    'nor' |> style_bold() |> style_underline() |> bg_br_yellow()
  ) |>
    col_grey() |>
    cli_inform(class = 'packageStartupMessage')

  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  'do not apply' |>
    style_underline() |>
    sprintf(fmt = 'These statements %s to the contributors to these packages with respect to their specific contributions.') |>
    style_italic() |>
    col_grey() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  'do not' |>
    style_underline() |>
    sprintf(fmt = 'These statements are advisory in nature and %s modify or restrict the rights granted under the GNU General Public License {.href [www.r-project.org/Licenses/](https://www.r-project.org/Licenses/)}.') |>
    style_italic() |>
    col_grey() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
}



