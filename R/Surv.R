


#' @title Summary Information of \link[survival]{Surv} Object
#' 
#' @param object a \link[survival]{Surv} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' Otherwise dispatch to \link[base]{summary.default}.
#' 
#' @examples
#' aml2 = survival::aml |>
#'  within.data.frame(expr = {
#'   os = survival::Surv(time = time, event = status)
#'   time = status = NULL
#'  })
#' summary(aml2$os)
#' summary(aml2)
#' 
#' heart2 = survival::heart |>
#'  within.data.frame(expr = {
#'   os = survival::Surv(time = start, time2 = stop, event = event)
#'   start = stop = event = NULL
#'  })
#' summary(heart2$os)
#' summary(heart2)
#' 
#' @keywords internal
#' @export summary.Surv
#' @export
summary.Surv <- function(object, ...) {
  
  # read ?base::summary.data.frame very carefully!!
  
  z <- if (ncol(object) == 2L) {
    (object[,2L] + 1L) |>
      structure(levels = c('[right-censored]', '[observed]'), class = 'factor') |>
      summary.factor()
  } else if (ncol(object) == 3L) {
    return(NextMethod(generic = 'summary')) # tzh doesnt know left-censoring and interval-censoring too well..
  } else stop('shouldnt happen')
  
  return(c(
    '<time-to-event>' = '(Surv)',
    z
  ))
  
}

