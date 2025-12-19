
# confidence interval based on permutation

# permute `mark` across the points for `nsim` times
# for each permutation, calculate ?spatstat.explore::Kmark 
# with all Kmark from all permutation, create `envelope` (think it as a confidence band)
# calculate how likely original Kmark `belongs` to the envelope


#' @title Random Re-Labelling Envelope Test
#' 
#' @param x see **Usage**
#' 
#' @param ... arguments of function \link[spatstat.explore]{envelope.ppp}, 
#' other than parameters `simulate`, 
#' `savefuns` and `verbose`
#' 
#' @examples
#' set.seed(52); re1_anemones = spatstat.data::anemones |>
#'   rlabelEnv.test(fun = spatstat.explore::Kmark)
#' re1_anemones |> class()
#' re1_anemones |> attr(which = 'p', exact = TRUE)
#' 
#' set.seed(52); re1_anemones_times = spatstat.data::anemones |>
#'   rlabelEnv.test(fun = spatstat.explore::Kmark, f = `*`)
#' re1_anemones_times |> attr(which = 'p', exact = TRUE)
#' 
#' stopifnot(!identical(re1_anemones, re1_anemones_times))
#'   
#' \donttest{
#' set.seed(52); spatstat.data::anemones |>
#'   rlabelEnv.test(fun = spatstat.explore::Kmark, f = \(m1, m2) { m1*m2 }) |>
#'   identical(y = re1_anemones_times) |>
#'   stopifnot()
#' }
#' 
#' set.seed(12); re_ants = spatstat.data::ants |>
#'   rlabelEnv.test(fun = spatstat.explore::Gcross)
#' re_ants |> attr(which = 'p', exact = TRUE)
#' 
#' @keywords internal
#' @name rlabelEnv_test
#' @export
rlabelEnv.test <- function(x, ...) UseMethod(generic = 'rlabelEnv.test')


#' @rdname rlabelEnv_test
#' @importFrom spatstat.explore envelope.ppp
#' @importFrom spatstat.random rlabel
#' @importFrom GET global_envelope_test residual
#' @method rlabelEnv.test ppp
#' @export rlabelEnv.test.ppp
#' @export
rlabelEnv.test.ppp <- function(x, ...) {
  x |>
    envelope.ppp(
      ...,
      simulate = expression(rlabel(X = x, permute = TRUE)),
      savefuns = TRUE, 
      verbose = FALSE
    ) |> # c('envelope', 'fv', 'data.frame')
    residual() |> # c('curve_set', 'list')
    global_envelope_test() # c('global_envelope', 'data.frame')
}


#' @rdname rlabelEnv_test
#' @importFrom spatstat.geom anylapply
#' @method rlabelEnv.test ppplist
#' @export rlabelEnv.test.ppplist
#' @export
rlabelEnv.test.ppplist <- function(x, ...) {
  x |>
    anylapply(FUN = rlabelEnv.test.ppp, ...)
}






#' @rdname rlabelEnv_test
#' @method rlabelEnv.test hyperframe
#' @export rlabelEnv.test.hyperframe
#' @export
rlabelEnv.test.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  n_ppp <- sum(hc_ppp)
  if (!n_ppp) return(invisible()) # exception handling
  if (n_ppp > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  
  z <- tryCatch(expr = {
    hc[[which(hc_ppp)]] |>
      rlabelEnv.test.ppplist(...)
  }, error = identity)
  if (inherits(z, what = 'error')) return(x) # exception handling
  
  return(cbind( # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    x, 
    .rlabelEnv = z
  ))
  
}


