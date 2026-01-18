


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
#' summary(aml2)
#' 
#' @keywords internal
#' @export summary.Surv
#' @export
summary.Surv <- function(object, ...) {
  z <- c('<time-to-event>' = '(Surv)') # read ?base::summary.data.frame very carefully!!
  class(z) <- 'summary.Surv'
  return(z)
}

#' @export
print.summary.Surv <- function(x, ...) {
  x[] |> # drops extra class
    print.default()
}