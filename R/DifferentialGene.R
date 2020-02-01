#' @include generics.R
#' @include Classes.R
NULL

#' @importFrom DEsingle DEsingle
#'
.findTwoGroupsMarker <- function(object, group.1 = NULL, group.2 =NULL){
  # two group number as factor.
  tmp.group <-
  results <- DEsingle(counts = object@LTMG@LTMG_discrete, group = tmp.group)

}
