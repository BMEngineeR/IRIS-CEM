#' @include generics.R
#' @include Classes.R
NULL

#' @param object
#'
.separateBic <- function(object = NULL){
  tmp.expression <- object@Processed_count
  bic.number<-length(unique(object@BiCluster@CoCond_cell$Condition))
  Bic <- c()
  Bic.name <-c()
  for (i in 1:bic.number){
    gene.name <- object@BiCluster@CoReg_gene[,1][object@BiCluster@CoReg_gene$Condition==i]
    cell.name <- object@BiCluster@CoCond_cell$cell_name[object@BiCluster@CoCond_cell$Condition==i]
    tmp.Bic <- tmp.expression[gene.name,cell.name]
    Bic.name <- c(Bic.name, paste0("Bicluster",i))
    Bic <- c(Bic, list(tmp.Bic))
  }
  names(Bic) <- Bic.name
  return(Bic)
}


#' Title PlotNetwork
#'
#' @description This function is for building the module- module network. Nodes mean individual module. Edges mean overlapped gene (or cell).
#' The weight of line show the number of gene (or cell).
#' @param object Input object
#' @param edge.by This parameter decide the edge by "cell" or by "gene". The default value is by gene.
#' @param lay.out The type of layout to create, include linear (default), circle, kk.
#' @param ...
#'
#' @name PlotNetwork
#' @return
#' @importFrom igraph graph_from_adjacency_matrix
#' @import ggraph
#' @examples
.plotnetwork <- function(object, edge.by = "gene",lay.out = "linear",N.block =c(1:20), ...){
  Bic.list <- .separateBic(object)
  Bic.list.select<- Bic.list[N.block]
  ntwork.adjacency.mtx <- matrix(1:(length(N.block)*length(N.block)),nrow = length(N.block))
  rownames(ntwork.adjacency.mtx) <- colnames(ntwork.adjacency.mtx)<- names(Bic.list.select)
  for (i in 1:nrow(ntwork.adjacency.mtx)){
    for (j in 1:ncol(ntwork.adjacency.mtx)){
      tmp.block.1 <- Bic.list.select[[rownames(ntwork.adjacency.mtx)[i]]]
      tmp.block.2 <- Bic.list.select[[colnames(ntwork.adjacency.mtx)[j]]]
      if (edge.by == "gene"){
        n.intersect <- length(intersect(rownames(tmp.block.1),rownames(tmp.block.2)))
      } else if(edge.by == "cell"){
        n.intersect <- length(intersect(colnames(tmp.block.1),colnames(tmp.block.2)))
      } else {stop(paste("Please select 'gene' or 'cell' to edge.by parameter"))}
      ntwork.adjacency.mtx[i,j] <- n.intersect
    }
  }
  edge.list <- graph_from_adjacency_matrix(adjmatrix =ntwork.adjacency.mtx ,mode = "undirected",weighted = T,diag = F)
  label = rownames(ntwork.adjacency.mtx)
  p<- ggraph(edge.list, layout = lay.out) +
    geom_node_point() +
    geom_edge_arc(aes(width = weight), alpha = 0.8) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = edge.by) +
    theme_graph()
  print(p)
}

#' @param BRIC
#'
#' @rdname PlotNetwork
#' @export
#'
setMethod("PlotNetwork","BRIC", .plotnetwork)


