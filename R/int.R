

unsimplify <- \(x) {
  # survival::Surv is.matrix
  # survival::Surv does **not** inherits from matrix :)
  if (!inherits(x, what = 'matrix')) return(x)
  x |> 
    asplit(MARGIN = 1L, drop = TRUE)
  # i.e., `simplify = TRUE` for Surv column,
  # but un-simplify for 'matrix'
}