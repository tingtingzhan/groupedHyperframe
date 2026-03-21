
# confidence interval based on permutation

# permute `mark` across the points for `nsim` times
# for each permutation, calculate ?spatstat.explore::Kmark 
# with all Kmark from all permutation, create `envelope` (think it as a confidence band)
# calculate how likely original Kmark `belongs` to the envelope


#' @title Random Re-Labelling Envelope Residual
#' 
#' @param x see **Usage**
#' 
#' @param ... arguments of the function \link[spatstat.explore]{envelope.ppp}, 
#' other than parameters `simulate`, 
#' `savefuns` and `verbose`
#' 
#' @examples
#' set.seed(52); res1_anemones = spatstat.data::anemones |>
#'   rlabelRes(fun = spatstat.explore::Kmark)
#' set.seed(52); res1_anemones_times = spatstat.data::anemones |>
#'   rlabelRes(fun = spatstat.explore::Kmark, f = `*`)
#' stopifnot(!identical(res1_anemones, res1_anemones_times))
#'   
#' \donttest{
#' set.seed(52); spatstat.data::anemones |>
#'   rlabelRes(fun = spatstat.explore::Kmark, f = \(m1, m2) { m1*m2 }) |>
#'   identical(y = res1_anemones_times) |>
#'   stopifnot()
#' }
#' 
#' set.seed(12); res_ants = spatstat.data::ants |>
#'   rlabelRes(fun = spatstat.explore::Gcross)
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name rlabelRes
#' @export
rlabelRes <- function(x, ...) UseMethod(generic = 'rlabelRes')


#' @rdname rlabelRes
#' @importFrom spatstat.explore envelope.ppp
#' @importFrom spatstat.random rlabel
#' @importFrom GET residual
#' @export
rlabelRes.ppp <- function(x, ...) {
  x |>
    envelope.ppp(
      ...,
      simulate = expression(rlabel(X = x, permute = TRUE)),
      savefuns = TRUE, 
      verbose = FALSE
    ) |> # c('envelope', 'fv', 'data.frame')
    residual() # c('curve_set', 'list')
}



