
if (FALSE) {
  old1 = wrobel_lung |>
    grouped_ppp_OLD(formula = hladr + phenotype ~ OS + gender + age | patient_id/image_id, data = _, coords = ~ x + y)
  old1
  
  old2 = wrobel_lung |>
    grouped_ppp_OLD(formula = hladr + phenotype ~ . | patient_id/image_id)
  old2 
  
  new1 = wrobel_lung |>
    grouped_ppp(formula = hladr + phenotype ~ OS + gender + age,
                    by = ~ patient_id/image_id)
  new1
  
  new2 = wrobel_lung |>
    grouped_ppp(formula = hladr + phenotype ~ .,
                    by = ~ patient_id/image_id)
  new2
  
}

#' @title Hyper Data Frame with One-and-Only-One \link[spatstat.geom]{ppp}-Hyper Column
#' 
#' @description
#' To create a (grouped) hyper data frame with one-and-only-one \link[spatstat.geom]{ppp}-hyper column.
#' 
#' @param marks a one-sided \link[stats]{formula} in the format of 
#' `~ m1+m2`,
#' where \eqn{m_i}'s are one or more \link[spatstat.geom]{marks}
#' 
#' @param coords \link[stats]{formula}, variable names
#' of the \eqn{x}- and \eqn{y}-coordinates in `data`.
#' Default value is `~x+y`.  
#' 
#' @param by a two-sided \link[stats]{formula}, see function [aggregate2()]
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param window an observation window \link[spatstat.geom]{owin}, 
#' default value is the \eqn{x}- and \eqn{y}-span of `coords` in `data`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' The function [grouped_ppp()] returns a (grouped) hyper data frame
#' with *one-and-only-one*
#' \link[spatstat.geom]{ppp}-hyper column.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/grouped_ppp_appx.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom owin ppp as.hyperframe.data.frame split.ppp
#' @importFrom stats model.frame
#' @export
grouped_ppp <- function(
    marks,
    coords = ~ x + y, 
    by,
    data, 
    window = owin(xrange = range(.x), yrange = range(.y)),
    ...
) {	
  
  if (by[[2L]] == '.') {
    by2var <- names(data) |>
      setdiff(y = c(
        all.vars(marks),
        all.vars(coords),
        all.vars(by[[3L]])
      ))
  } else {
    if ('.' %in% all.vars(by[[2L]])) stop('do not allow')
    by2var <- by[[2L]] |>
      all.vars() |>
      unique.default() # just to be double sure
  }
  
  unique_or_identity <- \(x) {
    u = unique(x)
    if (length(u) == 1L) return(u)
    return(x)
  }
  
  hf <- data[c(by2var, all.vars(by[[3L]]))] |>
    aggregate2(by = by, FUN = unique_or_identity, simplify = TRUE, drop = TRUE) |>
    as.hyperframe.data.frame()
  
  xy_ <- as.list.default(coords[[2L]])
  if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
  if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
  if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
  if (!length(.x <- data[[x]]) || anyNA(.x)) stop('Do not allow missingness in x-coordinates')
  if (!length(.y <- data[[y]]) || anyNA(.y)) stop('Do not allow missingness in y-coordinates')
  
  force(window)
  hf$ppp. <- ppp(x = .x, y = .y, window = window, marks = data[all.vars(marks)], checkdup = FALSE, drop = FALSE) |> # `drop = FALSE` important!!!
    split.ppp(
      f = by[[3L]] |> 
        call(name = '~') |>
        model.frame(formula = _, data = data) |>
        as.list.data.frame() |>
        interaction(drop = TRUE, sep = '.', lex.order = TRUE), # one or more hierarchy
      drop = FALSE
    )
  
  return(hf)
  
}









grouped_ppp_OLD <- function(
    formula, 
    by,
    data, 
    coords = ~ x + y, 
    window = owin(xrange = range(.x), yrange = range(.y)),
    ...
) {	
  
  if (formula[[3L]] == '.') {
    fom3var <- names(data) |>
      setdiff(y = c(
        all.vars(formula[[2L]]),
        all.vars(by),
        all.vars(coords)
      ))
  } else {
    if ('.' %in% all.vars(formula[[3L]])) stop('do not allow')
    fom3var <- formula[[3L]] |>
      all.vars() |>
      unique.default() # just to be double sure
  }
  
  hf <- data[c(fom3var, all.vars(by))] |>
    aggregate2(by = by) |>
    as.hyperframe.data.frame()

  xy_ <- as.list.default(coords[[2L]])
  if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
  if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
  if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
  if (!length(.x <- data[[x]]) || anyNA(.x)) stop('Do not allow missingness in x-coordinates')
  if (!length(.y <- data[[y]]) || anyNA(.y)) stop('Do not allow missingness in y-coordinates')
  
  force(window)
  hf$ppp. <- ppp(x = .x, y = .y, window = window, marks = data[all.vars(formula[[2L]])], checkdup = FALSE, drop = FALSE) |> # `drop = FALSE` important!!!
    split.ppp(
      f = by |> 
        getGroups_df_(object = data, form = _) |>
        as.list.data.frame() |>
        interaction(drop = TRUE, sep = '.', lex.order = TRUE), # one or more hierarchy
      drop = FALSE
    )
  
  return(hf)
  
}



