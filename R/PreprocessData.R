#' @include generics.R
NULL

#' @param object
#'
#' @param IsScaterNormal whether open normalization
#' @param IsImputation
#'
#' @importFrom scater normalize logNormCounts
#' @importFrom SingleCellExperiment SingleCellExperiment normcounts
#' @importFrom scran quickCluster computeSumFactors
#' @importFrom Seurat as.sparse
#' @importFrom DrImpute DrImpute

.processData <- function(object = NULL, IsScaterNormal=FALSE, IsImputation = FALSE){
  Input <- object@Raw_count
  if(all(as.numeric(unlist(Input[nrow(Input),]))%%1==0)){
    ## normalization##############################
    if (IsScaterNormal == FALSE) {
      my.normalized.data <- Input
    } else{
      sce <- SingleCellExperiment(assays = list(counts = Input))
      clusters <- quickCluster(sce,min.size = floor(ncol(Input)/3))
      sce <- computeSumFactors(sce, clusters = clusters)
      sce <- scater::logNormCounts(sce,log = FALSE)
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
  object@Processed_count <- my.imputated.data
  return(object)
}


#' @export
#' @rdname NormalizeData
setMethod("ProcessData", "BRIC", .processData)





