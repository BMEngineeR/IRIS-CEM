#' @include generics.R
#' @include Classes.R
NULL

#' plot heatmap based on bicluster
#'
#' @param object BRIC object
#' @param N.block number of blocks
#' @name PlotHeatmap
#' @import pheatmap
.plotHeatmap <- function(object = object, N.block = 2){
  condition.index <- 1:N.block
  gene.sub <- c()
  cell.sub <- c()
  for (i in condition.index){
    gene.sub <- rbind(gene.sub,object@BiCluster@CoReg_gene[object@BiCluster@CoReg_gene$Condition == i,])
    cell.sub <- rbind(cell.sub, object@BiCluster@CoCond_cell[object@BiCluster@CoCond_cell$Condition==i,])
  }
  gene.sub.rmdup <- gene.sub[!duplicated(gene.sub$cell_name),]
  cell.sub.rmdup <- cell.sub[!duplicated(cell.sub$cell_name),]
  cell.duplicate.name <- cell.sub$cell_name[duplicated(cell.sub$cell_name)]
  unique.gene.name <- gene.sub.rmdup$cell_name
  unique.cell.name <- cell.sub.rmdup$cell_name
  heatmap.matrix <- object@Processed_count[unique.gene.name,unique.cell.name]
  annotation <- data.frame(row.names = cell.sub.rmdup$cell_name, Condition = cell.sub.rmdup$Condition)
  annotation[cell.duplicate.name,] <- "overlap"
  annotation$temp <- rep(1,nrow(annotation))
  order.index<-order(annotation$Condition)
  annotation <- annotation[order.index,]
  heatmap.matrix <- heatmap.matrix[,order.index]
  annotation$temp <- NULL
  pheatmap(heatmap.matrix,
           color = colorRampPalette(c("blue","white","red"))(100),
           scale = "row",
           cluster_rows = F,
           cluster_cols = F,
           show_rownames = F,
           show_colnames = F,
           annotation = annotation
           )
}

#' @export
#' @rdname PlotHeatmap
setMethod("PlotHeatmap", "BRIC", .plotHeatmap)


