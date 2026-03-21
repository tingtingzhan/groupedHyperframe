
# spatstat.geom::with.hyperframe
# with multi-core parallel support
# packageVersion('spatstat.geom') # 3.7.1

# \CRANpkg{doParallel} and \CRAN{foreach}
# https://cran.r-project.org/package=doParallel
# https://cran.r-project.org/package=foreach
# are "official" parallel package with Microsoft (copyright) 

#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel detectCores mclapply makeCluster stopCluster
#' 
#' @importFrom spatstat.geom as.anylist
with_hyperframe_mc <- function(
    data, expr, ..., simplify = TRUE, ee = NULL, enclos = NULL, # original parameters
    mc.cores = detectCores() # new parameter!!
) {
  if (!inherits(data, "hyperframe")) 
    stop("data must be a hyperframe")
  if (is.null(ee)) 
    ee <- as.expression(substitute(expr))
  if (is.null(enclos)) 
    enclos <- parent.frame()
  n <- nrow(data)
  out <- vector(mode = "list", length = n)
  nama <- intersect(all.names(ee), colnames(data))
  if (length(nama)) {
    bad <- apply(is.na(data)[, nama, drop = FALSE], 1, any)
    goodrows <- which(!bad)
    out[bad] <- NA
  }
  else {
    goodrows <- seq_len(n)
  }
  datalist <- as.list(data)
  
  
  # original chunk
  #for (i in goodrows) {
  #  rowi <- lapply(datalist, "[[", i = i)
  #  outi <- eval(ee, rowi, enclos)
  #  if (!is.null(outi)) 
  #    out[[i]] <- outi
  #}
  # end of original chunk
  
  # new chunk: parallel!
  foo <- \(i) {
    rowi <- lapply(datalist, "[[", i = i)
    eval(ee, rowi, enclos) # just return, whether NULL or not
  }
  switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = { 
      out[goodrows] <- goodrows |> 
        mclapply(mc.cores = mc.cores, FUN = foo)
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
      out[goodrows] <- foreach(i = goodrows, .options.multicore = list(cores = mc.cores)) %dopar% foo(i = i)
      stopCluster(cl)
    })
  # end of new chunk
  
  
  
  names(out) <- row.names(data)
  if (simplify && all(unlist(lapply(out, is.vector)))) {
    lenfs <- lengths(out)
    if (all(unlist(lapply(out, is.atomic))) && length(unique(lenfs)) == 
        1) {
      out <- t(as.matrix(as.data.frame(out)))
      row.names(out) <- row.names(data)
      out <- out[, , drop = TRUE]
      return(out)
    }
  }
  out <- hyperframe(result = as.anylist(out), row.names = row.names(data))$result
  return(out)
}