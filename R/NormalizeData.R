#' @include generics.R
NULL
#' Normarlize data
#'
#' @param Input
#' @param IsImputation
#'
#' @return
#' @export
#' @examples
NULL
#' @importFrom scater normalize
#' @importFrom Seurat as.sparse
#' @importFrom DrImpute DrImpute

.NormalizeData <- function(object = NULL, IsScaterNormal=FALSE, IsImputation = FALSE){
  Input <- object@raw_count
  if(all(as.numeric(unlist(Input[nrow(Input),]))%%1==0)){
    ## normalization##############################
    if (IsScaterNormal == FALSE) {
      my.normalized.data <- Input
    } else{
      sce <- tryCatch(computeSumFactors(sce),error = function(e) normalizeSCE(sce))
      sce <- scater::normalize(sce,return_log=F)
      my.normalized.data <- normcounts(sce)
    }
  } else {
    my.normalized.data <- Input

  }

  ## imputation#################################
  if (IsImputation == TRUE) {
    my.imputated.data <- DrImpute(as.matrix(my.normalized.data),dists = "spearman")
  } else {
    my.imputated.data <- my.normalized.data
  }

  colnames(my.imputated.data) <- colnames(Input)
  rownames(my.imputated.data) <- rownames(Input)


  my.imputated.data <- as.sparse(my.imputated.data)
  object@processed_count <- my.imputated.data
  return(object)
}


#' @export
#' @rdname NormalizeData
setMethod("NormalizeData", "BRIC", .NormalizeData)





