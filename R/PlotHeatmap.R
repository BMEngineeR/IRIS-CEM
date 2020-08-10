#' @include generics.R
#' @include Classes.R
NULL

#' @title plot heatmap based on bicluster
#'
#'
#' @param object BRIC object
#' @param N.block number of blocks
#' @name PlotHeatmap
#' @import pheatmap
.plotHeatmap <- function(object = object, N.block = c(1,5)){
  vec.boolean <- vector(mode = "logical")
  for (i in seq_along(N.block)){
    vec.boolean[i]<-is.double(N.block[i])
  }
  if (!all(vec.boolean)){stop("please type two block numbers")}
  if(length(N.block)!=2){stop("Only plot two block; please type in two numbers")}
  condition.index <- N.block
  gene.sub <- c()
  cell.sub <- c()
  for (i in condition.index){
    gene.sub <- rbind(gene.sub,object@BiCluster@CoReg_gene[object@BiCluster@CoReg_gene$Condition == i,])
    cell.sub <- rbind(cell.sub, object@BiCluster@CoCond_cell[object@BiCluster@CoCond_cell$Condition==i,])
  }
  cell.block.1 <- cell.sub$cell_name[cell.sub$Condition == N.block[1]]
  cell.block.2 <- cell.sub$cell_name[cell.sub$Condition == N.block[2]]
  cell.block1.diff <- setdiff(cell.block.1,cell.block.2)
  cell.block2.diff <- setdiff(cell.block.2,cell.block.1)
  cell.overlap <- intersect(cell.block.1,cell.block.2 )
  cell.block.vec <- c(rep(N.block[1],length(cell.block1.diff)),
                      rep(N.block[2],length(cell.block2.diff)),
                      rep("overlap",length(cell.overlap))
                      )
  annotation_col <- data.frame(row.names = c(cell.block1.diff,cell.block2.diff,cell.overlap) , block = cell.block.vec)

  gene.block.1 <- gene.sub$cell_name[gene.sub$Condition == N.block[1]]
  gene.block.2 <- gene.sub$cell_name[gene.sub$Condition == N.block[2]]
  gene.block1.diff <- setdiff(gene.block.1,gene.block.2)
  gene.block2.diff <- setdiff(gene.block.2,gene.block.1)
  gene.overlap <- intersect(gene.block.1,gene.block.2 )
  gene.block.vec <- c(rep(N.block[1],length(gene.block1.diff)),
                      rep(N.block[2],length(gene.block2.diff)),
                      rep("overlap",length(gene.overlap))
  )
  annotation_row <- data.frame(row.names = c(gene.block1.diff,gene.block2.diff,gene.overlap) , block = gene.block.vec)
  heatmap.matrix <- object@Processed_count[rownames(annotation_row),rownames(annotation_col)]

  pheatmap(heatmap.matrix,
           color = colorRampPalette(c("#A402DC","#0D0E00","#B1BD16"))(100),
           scale = "row",
           border_color=NA,
           cluster_rows = F,
           cluster_cols = F,
           show_rownames = F,
           show_colnames = F,
           main = paste0("block ",N.block[1]," and block ",N.block[2]),
           annotation_col = annotation_col
           #annotation_row = annotation_row
           )
}

#' @export
#' @rdname PlotHeatmap
setMethod("PlotHeatmap", "BRIC", .plotHeatmap)


