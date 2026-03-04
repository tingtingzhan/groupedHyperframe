
# @importFrom parallel detectCores
.onLoad <- function(libname, pkgname) {
  
  options(
    
    bitmapType = 'cairo'#, # unicode support # MUST as of macOS, R 4.5.1
    
    # .Platform$OS.type # as of R 4.5, only two responses, 'windows' or 'unix'
    # cores = detectCores()
    # \CRANpkg{doParallel} convention, the '^mc\\.' prefix is dropped! i.e., not 'mc.cores', but 'cores'
    # NEED this option to suppress parallel computing when submitting to CRAN !!!
    
    # read this discussion very very carefully!  Especially Dirk's reply!!!!
    # https://github.com/Rdatatable/data.table/issues/5658
    
  )
  
}

