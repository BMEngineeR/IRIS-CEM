#' @include generics.R
#' @include Classes.R
NULL

#' @param object
#'
#' @param reduction
#' @param dims
#' @name RundimensionReduction
#' @importFrom Seurat CreateSeuratObject ScaleData RunPCA RunTSNE RunUMAP
.runDimensionReduction <- function(object, reduction = "tsne", dims = 1:15){
  tmp.seurat <- CreateSeuratObject(object@LTMG@LTMG_discrete)
  tmp.seurat<- ScaleData(tmp.seurat)
  tmp.seurat <- suppressMessages(RunPCA(tmp.seurat, features = rownames(tmp.seurat@assays$RNA)))
  object@LTMG@DimReduce@PCA <- tmp.seurat@reductions$pca@cell.embeddings
  if(grepl("tsne", reduction, ignore.case = T) || grepl("umap", reduction, ignore.case = T)){
    if(grepl("tsne", reduction, ignore.case = T)){
      tmp.seurat <- RunTSNE(tmp.seurat,dims=dims)
      object@LTMG@DimReduce@TSNE <- tmp.seurat@reductions$tsne@cell.embeddings
    }
    if(grepl("umap", reduction, ignore.case = T)){
      tmp.seurat <- suppressMessages(RunUMAP(tmp.seurat,dims = dims))
      object@LTMG@DimReduce@UMAP <- tmp.seurat@reductions$umap@cell.embeddings
    }
  } else {stop("choose a dimension reduction method between umap or tsne")}
object@LTMG@tmp.seurat<-tmp.seurat
return(object)
}


#' @rdname RunDimensionRecution
#' @export
setMethod("RunDimensionReduction", "BRIC", .runDimensionReduction)



#' @param object
#' @param k.param
#' @param resolution
#' @param algorithm
#' @param dims
#' @name RunClassification
#' @importFrom Seurat FindNeighbors FindClusters
#' @import ggplot2
.runClassification <- function(object,dims = 1:15, k.param = 20, resolution = 0.6, algorithm = 1 ){
  if ( is.null(object@LTMG@tmp.seurat)) {stop("There is no temporary seurat obejct getting detected. \n Try to run RundimensionRuduction first.")}
  tmp.seurat <- object@LTMG@tmp.seurat
  tmp.seurat <- FindNeighbors(tmp.seurat,dims=dims)
  tmp.seurat <- FindClusters(tmp.seurat, resolution = resolution, algorithm = algorithm)
  tmp.meta <- object@MetaInfo
  tmp.colname <- colnames(tmp.meta)
  res.index <- grep(paste0("res.",resolution),colnames(tmp.seurat@meta.data))
  tmp.colname <- c(tmp.colname,paste0("Seurat",resolution) )
  tmp.meta <- cbind(tmp.meta, tmp.seurat@meta.data[,res.index])
  colnames(tmp.meta) <- tmp.colname
  object@MetaInfo <- tmp.meta
  object@LTMG@tmp.seurat <- tmp.seurat
  return(object)
}

#' @rdname RunDimensionRecution
#' @export
setMethod("RunClassification", "BRIC", .runClassification)

# .visualzeDim <- function(object, reduction = "tsne", )


.plotDimension <- function(object, reduction = "tsne", pt_size = 0.5){

  if(grepl("tsne", reduction, ignore.case = T) || grepl("umap", reduction, ignore.case = T)||grepl("pca", reduction, ignore.case = T)){

    if(grepl("tsne", reduction, ignore.case = T)){
      tmp.plot.table <- object@LTMG@DimReduce@TSNE[,c(1,2)]
    }
      else if(grepl("umap", reduction, ignore.case = T)){
      tmp.plot.table <- object@LTMG@DimReduce@UMAP[,c(1,2)]
     }
      else if(grepl("tsne", reduction, ignore.case = T)){
      tmp.plot.table <- object@LTMG@DimReduce@PCA[,c(1,2)]
    }
  } else {stop("choose a dimension reduction method from pca, umap, or tsne")}
  message("select condition to present")
  message(paste0(c(1:ncol(object@MetaInfo))," : ",c(colnames(object@MetaInfo)),"\n"))
  ident.index <-  readline(prompt="select index of cell condition: ")
  ident.index <- as.numeric(ident.index)
  tmp.ident <- object@MetaInfo[,ident.index]
  names(tmp.ident) <- rownames(object@MetaInfo)
  # check name later add
  tmp.plot.table <- cbind.data.frame(tmp.plot.table,Cell_type=as.character(tmp.ident))
  p.cluster <- ggplot(tmp.plot.table, aes(x= tmp.plot.table[,1],y = tmp.plot.table[,2]))

  p.cluster <- p.cluster+geom_point(stroke=pt_size,size=pt_size,aes(col=tmp.plot.table[,"Cell_type"]))

  p.cluster <- p.cluster + guides(colour = guide_legend(override.aes = list(size=5))) + labs( color ="cell type")+ xlab("Dimension 1") + ylab("Dimentsion 2")
  p.cluster
}

#' @rdname PlotDimension
#' @export
setMethod("PlotDimension", "BRIC", .plotDimension)





