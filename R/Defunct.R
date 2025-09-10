



#' @title Defunct Functions
#' 
#' @description
#' Functions mentioned in hard-copy journals, but later \link[base]{.Defunct}.
#' 
#' @keywords internal
#' @name defunct
#' @export
aggregate_quantile <- function(...) {
  .Defunct(msg = '\nThis function described in\ndoi:10.1093/bioinformatics/btaf430\nhas been replaced by pipeline\n<groupedHyperframe> |> quantile() |> aggregate()\nRead vignette for details\nhttps://rpubs.com/tingtingzhan/groupedHyperframe')
}


#' @rdname defunct
#' @export
aggregate_fv <- function(...) {
  .Defunct(msg = '\nThis function used by developers\nhas been replaced by pipeline\n<groupedHyperframe> |> summary_fv() |> aggregate()\nRead vignette for details\nhttps://rpubs.com/tingtingzhan/groupedHyperframe')
}



