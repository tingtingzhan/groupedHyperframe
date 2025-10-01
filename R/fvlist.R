

#' @title Inspect a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @description
#' A helper function to check the validity of a \link[stats]{listof} \link[spatstat.explore]{fv.object}s.
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @returns 
#' Function [is.fvlist()] returns a \link[base]{logical} scalar with \link[base]{attributes}
#' \describe{
#' \item{`attr(,'r')`}{\eqn{x}-axis, or the \eqn{r}-values}
#' \item{`attr(,'fname')`}{see explanation of this \link[base]{attributes} in function \link[spatstat.explore]{fv}}
#' \item{`attr(,'.x')`}{`spatstat.explore::fvnames(x, a = '.x')` returns}
#' \item{`attr(,'.y')`}{`spatstat.explore::fvnames(x, a = '.y')` returns}
#' }
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames
#' @export
is.fvlist <- function(X) {
  
  # tzh is aware that
  # ?spatstat.explore::roc.ppp returns an `'roc'` object, inherits from `'fv'`, first argument being `p` instead of `r`!!!
  # in [as.fvlist()] tzh still uses `r`
  # because we have function [.rmax()] ...
  
  id <- X |>
    vapply(FUN = inherits, what = 'fv', FUN.VALUE = NA)
  if (!all(id)) {
    message('not all elements are `fv.object`')
    return(FALSE)
  }
  .x <- X |>
    vapply(FUN = fvnames, a = '.x', FUN.VALUE = '')
  if (!all(duplicated.default(.x)[-1L])) {
    message('`.x` of all fv.objects are not the same')
    return(FALSE)
  }
  
  .y <- X |> 
    vapply(FUN = fvnames, a = '.y', FUN.VALUE = '')
  if (!all(duplicated.default(.y)[-1L])) {
    message('`.y` of all fv.objects are not the same')
    return(FALSE)
  }
  
  fname. <- X |>
    lapply(FUN = attr, which = 'fname', exact = TRUE)
  if (!all(duplicated.default(fname.)[-1L])) {
    message('`fname` of all fv.objects are not the same')
    return(FALSE)
  }
  
  r. <- X |>
    lapply(FUN = `[[`, .x[1L])
  if (!all(duplicated.default(r.)[-1L])) {
    message('x-axis of all fv.objects are not the same')
    return(FALSE)
  }
  
  ret <- TRUE
  attr(ret, which = 'r') <- r.[[1L]]
  attr(ret, which = '.x') <- .x[[1L]]
  attr(ret, which = '.y') <- .y[[1L]]
  attr(ret, which = 'fname') <- fname.[[1L]]
  return(ret)
  
}


#' @title Convert a \link[stats]{listof} \link[spatstat.explore]{fv.object}s into `'fvlist'`
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @param data.name \link[base]{character} scalar, name of `X`, for console message output
#' 
#' @returns 
#' Function [as.fvlist()] returns an \link[base]{invisible} \link[base]{list}.
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames
#' @export
as.fvlist <- function(X, data.name) {
  
  tmp <- is.fvlist(X) |>
    suppressMessages()
  if (!tmp) return(X) # exception handling
  
  attr(X, which = 'r') <- r <- attr(tmp, which = 'r', exact = TRUE)
  nr <- length(r)
  attr(X, which = '.x') <- attr(tmp, which = '.x', exact = TRUE)
  attr(X, which = '.y') <- .y <- attr(tmp, which = '.y', exact = TRUE)
  attr(X, which = 'fname') <- attr(tmp, which = 'fname', exact = TRUE)
  
  id <- X |> 
    vapply(FUN = \(x) {
      x[[.y]] |>
        lastLegal()
    }, FUN.VALUE = NA_integer_)
  
  if (any(id < nr)) {
    
    id0 <- id[id != nr]
    tb <- id0 |> table()
    uid <- id0 |> unique.default() |> sort.int()
    loc <- uid |>
      vapply(FUN = \(u) {
        which(id == u) |>
          paste0('L', collapse = ', ') |>
          col_magenta() |> style_bold()
      }, FUN.VALUE = '')
    
    if (!missing(data.name)) {
      paste0(
        'Legal ', 
        'rmax' |> col_magenta() |> style_bold(),
        '(', 
        data.name |> col_blue() |> style_bold(), 
        sprintf(fmt = '), smaller than user input of rmax = %.1f, are\n', max(r)), 
        sprintf(fmt = '%d\u2a2f ', tb) |> col_br_magenta() |> style_bold() |>
          paste0('rmax=', r[uid], ' at location ', loc, collapse = '\n')
      ) |>
        message()
    }
    
    rmax <- min(r[uid]) # minimum legal rmax of 'fvlist'
    
  } else rmax <- max(r)
  
  if (rmax == 0) stop("check the `'ppplist'` that created the input `x`")
  attr(X, which = 'rmax') <- rmax
  
  class(X) <- c('fvlist', 'anylist', 'listof', 'list', class(X)) |> 
    unique.default()
  return(X)
  
}




#' @title Print `'fvlist'`
#' 
#' @param x an `'fvlist'`
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export print.fvlist
#' @export  
print.fvlist <- function(x, ...) {
  
  x |>
    length() |>
    col_red () |> style_bold() |>
    sprintf(fmt = 'An \'fvlist\' of %s fv.objects') |>
    message()
  
  r_range <- x |>
    attr(which = 'r', exact = TRUE) |>
    range() |>
    paste(collapse = ' ~ ') |>
    col_magenta() |> style_bold()
  
  .x <- x |>
    attr(which = '.x', exact = TRUE)

  fname <- x |> 
    attr(which = 'fname', exact = TRUE)
    
  if (length(fname) == 1L) {
    sprintf(fmt = '%s(%s)', fname, .x) |>
      message()
  } else if (length(fname) == 2L) {
    fnm2 <- fname[2L] |> str2lang() |> as.list()
    .subscript <- if (length(fnm2) == 1L) {
      fnm2[[1L]] |> deparse1()
    } else {
      fnm2[-1L] |> 
        vapply(FUN = deparse1, FUN.VALUE = '') |>
        paste(collapse = ',')
    }
    sprintf(fmt = '%s[%s](%s)', fname[1L], .subscript, .x) |>
      message()
  } else stop('not supported!!')
      
  sprintf(fmt = '%s-range: %s', .x, r_range) |>
    message()
  
  x |>
    attr(which = 'rmax', exact = TRUE) |>
    sprintf(fmt = 'Minimum Legal rmax: %.4g') |>
    message()

}





#' @title Truncated Summary of `'fvlist'`
#' 
#' @param object an `'fvlist'`
#' 
#' @param data.name \link[base]{character} scalar
#' 
#' @param rmax (optional) \link[base]{numeric} scalar, user \eqn{r_\text{max}}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is 1L on Windows, or \link[parallel]{detectCores} on Mac.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export summary.fvlist
#' @export
summary.fvlist <- function(
    object, 
    data.name = deparse1(substitute(object)),
    rmax, 
    mc.cores = getOption('mc.cores'), 
    ...
) {
  
  x <- object |>
    as.fvlist() |> # because ?spatstat.geom::hyperframe drops tzh's 'fvlist'
    suppressMessages()
  
  r <- attr(x, which = 'r', exact = TRUE)
  x_rmax <- attr(x, which = 'rmax', exact = TRUE)
  .y <- attr(x, which = '.y', exact = TRUE)
  
  if (missing(rmax) || !length(rmax)) { # missing user `rmax`
    # `!length(rmax)` needed in ?base::mapply (at least tzh thinks so, 2025-09-09)
    if (x_rmax < max(r)) {
      sprintf(fmt = 'summary.fvlist truncated at rmax(%s) = %.1f', data.name, x_rmax) |>
        style_bold() |> bg_br_yellow() |> message()
      id <- (r <= x_rmax)
    } else id <- rep(TRUE, times = length(r)) # cannot just be `TRUE` (for later use..)
    
  } else if (rmax > x_rmax) { # user `rmax > x_rmax`
    if (x_rmax < max(r)) {
      sprintf(fmt = 'summary.fvlist truncated at rmax(%s) = %.1f (user rmax = %.1f ignored)', data.name, x_rmax, rmax) |>
        style_bold() |> bg_br_yellow() |> message()
    } else {
      sprintf(fmt = 'summary.fvlist at maximum r(%s) = %.1f (user rmax = %.1f ignored)', data.name, x_rmax, rmax) |>
        style_bold() |> bg_br_yellow() |> message()
    }
    id <- (r <= x_rmax)
    
  } else { # use user `rmax`
    sprintf(fmt = 'summary.fvlist truncated at rmax = %.1f for %s', rmax, data.name) |>
      style_bold() |> bg_br_yellow() |> message()
    id <- (r <= rmax)
  }
  
  return(list(
    y = x |> 
      lapply(FUN = \(i) keyval.fv(i, key = .y)[id]),
    cumtrapz = x |> 
      mclapply(mc.cores = mc.cores, FUN = \(i) cumtrapz.fv(i, key = .y)[id[-1L]]), # `-1L` super important!!!
    cumvtrapz = x |> 
      mclapply(mc.cores = mc.cores, FUN = \(i) cumvtrapz.fv(i, key = .y)[id[-1L]]) # `-1L` super important!!!
  ))
  
}















#' @title Last Legal Index
#' 
#' @param v \link[base]{double} \link[base]{vector}
#' 
#' @details
#' Legal, meaning not `0`, not `NaN` and not `Inf`.
#' 
#' @keywords internal
#' @export
lastLegal <- function(v) {
  
  vok <- is.finite(v) & (abs(v) > .Machine$double.eps) # not 0, not NaN, not Inf
  
  if (all(vok)) {
    
    id <- length(vok) # faster than [.diff()]
    
  } else {
    
    .diff <- \(x) {
      x[-1L] - x[-length(x)]
    } # faster than ?base::diff.default
    
    z <- vok |> 
      which() |>
      .diff()
    
    id <- if (all(z == 1L)) {
      length(z) + 1L
    } else min(which(z != 1L)) # smart!
    
  }

  attr(id, which = 'value') <- v[id]
  return(id)
  
}


#' @title Tentative Fix for Illegal Function Value in `fv.object`
#' 
#' @param X an \link[spatstat.explore]{fv.object}
#' 
#' @keywords internal
#' @name illegal2
#' @importFrom spatstat.explore bind.fv fvnames
#' @export
illegal2theo <- function(X) {
  
  key <- X |> 
    fvnames(a = '.y')
  
  theo <- X$theo
  if (is.null(theo)) stop('this fv.object does not have theo ?')
  
  yval <- X[[key]]
  
  y_theo <- ifelse(
    test = is.finite(yval) & (abs(yval) > .Machine$double.eps),
    yes = yval,
    no = theo
  )
  
  ret <- bind.fv(x = X, y = data.frame(y_theo = y_theo), labl = 'y_theo')
  return(ret)

}



#' @rdname illegal2
#' @importFrom spatstat.explore bind.fv fvnames
#' @export
illegal2theo_1st <- function(X) {
  
  key <- X |> 
    fvnames(a = '.y')
  
  theo <- X$theo
  if (is.null(theo)) stop('this fv.object does not have theo ?')
  
  yval <- X[[key]]
  
  id <- yval |>
    lastLegal()
  sq <- id:length(yval)
  y_theo <- yval
  y_theo[sq] <- theo[sq]
  
  ret <- bind.fv(x = X, y = data.frame(y_theo = y_theo), labl = 'y_theo')
  return(ret)
  
}


