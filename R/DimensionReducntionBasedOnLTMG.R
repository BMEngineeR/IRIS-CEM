#' @include generics.R
#' @include Classes.R
NULL

#' @import Seurat CreateSeuratObject RunPCA RunTSNE RunUMAP FindNeighbors FindClusters
#'

.runPCA <- function(object, reduction = "pca", dims = 1:15, )
  tmp.seurat<- ScaleData(tmp.seurat)
  tmp.seurat <- Seurat::RunPCA(tmp.seurat, features = rownames(tmp.seurat@assays$RNA))
  DimPlot(tmp.seurat, reduction = "pca")
  tmp.seurat <- Seurat::FindNeighbors(tmp.seurat,dims=1:15)
  tmp.seurat <- Seurat::FindClusters(tmp.seurat, resolution = 0.5)
  tmp.seurat <- Seurat::RunTSNE(tmp.seurat,dims=1:15)
  tmp.seurat <- Seurat::RunUMAP(tmp.seurat,dims = 1:15)
  DimPlot(tmp.seurat)
