
if (FALSE) {
  data(package = 'spatstat.data') |>
    adv.tzh::dataFrom(what = 'ppp') |>
    names()
  
  set.seed(52); aa = spatstat.data::anemones |> # 'ppp'
    spatstat.explore::envelope.ppp(
      fun = spatstat.explore::Kmark, 
      f = function(m1, m2) { m1*m2 }, # ???
      simulate = expression(spatstat.random::rlabel(spatstat.data::anemones, permute = TRUE)), # term 'random labeling' comes from here; should be hard-coded
      savefuns = TRUE, 
      verbose = FALSE
    ) |> # c('envelope', 'fv', 'data.frame')
    GET::residual() |> # c('curve_set', 'list')
    GET::global_envelope_test() # c('global_envelope', 'data.frame')
  attr(aa, which = 'p', exact = TRUE)
}

# confidence interval based on permutation

# permute `mark` across the points for `nsim` times
# for each permutation, calculate ?spatstat.explore::Kmark 
# with all Kmark from all permutation, create `envelope` (think it as a confidence band)
# calculate how likely original Kmark `belongs` to the envelope


#' @title Random Re-Labelling Test
#' 
#' @param Y a \link[spatstat.geom]{ppp.object}, see function \link[spatstat.explore]{envelope.ppp}
#' 
#' @param fun,f,... arguments of function \link[spatstat.explore]{envelope.ppp}
#' 
#' @examples
#' set.seed(52); spatstat.data::anemones |>
#'   foobar() |>
#'   attr(which = 'p', exact = TRUE)
#' 
#' @keywords internal
#' @importFrom spatstat.explore envelope.ppp Kmark
#' @importFrom spatstat.random rlabel
#' @importFrom GET global_envelope_test residual
#' @export
foobar <- function(
  Y, # 'ppp'
  fun = Kmark, 
  f = function(m1, m2) { m1*m2 },
  ...
) {
  
  Y |>
    envelope.ppp(
      fun = fun,
      f = f,
      simulate = expression(rlabel(Y, permute = TRUE)), # term 'random labeling' comes from here; should be hard-coded
      savefuns = TRUE, 
      verbose = FALSE,
      ...
    ) |> # c('envelope', 'fv', 'data.frame')
    residual() |> # c('curve_set', 'list')
    global_envelope_test() # c('global_envelope', 'data.frame')
  
}






