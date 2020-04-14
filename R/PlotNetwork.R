#' @include generics.R
#' @include Classes.R
NULL

#' @param object
#' @importFrom qgraph qgraph
.separateBic <- function(object = NULL){
  tmp.expression <- object@Processed_count
  bic.number<-length(unique(object@BiCluster@CoCond_cell$Condition))
  Bic <- c()
  Bic.name <-c()
  for (i in 1:bic.number){
    gene.name <- object@BiCluster@CoReg_gene$cell_name[object@BiCluster@CoReg_gene$Condition==i]
    cell.name <- object@BiCluster@CoCond_cell$cell_name[object@BiCluster@CoCond_cell$Condition==i]
    tmp.Bic <- tmp.expression[gene.name,cell.name]
    Bic.name <- c(Bic.name, paste0("Bicluster",i))
    Bic <- c(Bic, list(tmp.Bic))
  }
  names(Bic) <- Bic.name
  return(Bic)
}

#' plot network based on bicluster
#'
#' @param object
#' @param Bic.index
#' @param method
#'
#' @return
#' @name PlotNetwork
#' @examples
.qunetwork <- function(object = NULL, Bic.index = 1, method = c("pearson", "kendall", "spearman"), is.plot = TRUE) {

  x <- object@Processed_count
  number = 1: length(unique(object@BiCluster@CoCond_cell$Condition))
  groups = c(number[[Bic.index = Bic.index]])
  if (length(number) < 1)
    stop("at least 1 bicluster needed.")
  bics <- .separateBic(object = object)
  index <- which(number %in% groups)

  rownamelist <- list()
  colnamelist <- list()
  for (i in 1:length(bics)) {
    rownamelist[[names(bics)[i]]] <- rownames(bics[[i]])
    colnamelist[[names(bics)[i]]] <- colnames(bics[[i]])
  }

  allrownames <- Reduce(union, rownamelist)
  allcolnames <- Reduce(union, colnamelist)

  un <- x[allrownames, allcolnames]
  rowidlist <- list()

  if (length(groups) > 2)
    stop("length(group) > 2")
  if (length(groups) == 1) {
    rowidlist[[names(bics)[index[[1]]]]] <-
      match(rownamelist[[index[[1]]]],
            rownames(un))
  } else if (length(groups) == 2) {
    rowidlist[[paste0(names(bics)[index[[1]]], " & ", names(bics)[index[[2]]])]] <-
      match(intersect(rownamelist[[index[[1]]]], rownamelist[[index[[2]]]]),
            rownames(un))
    rowidlist[[names(bics)[index[[1]]]]] <-
      match(setdiff(rownamelist[[index[[1]]]],
                    rownamelist[[index[[2]]]]), rownames(un))
    rowidlist[[names(bics)[index[[2]]]]] <-
      match(setdiff(rownamelist[[index[[2]]]],
                    rownamelist[[index[[1]]]]), rownames(un))
    rowidlist[["Others"]] <-
      match(setdiff(allrownames, union(rownamelist[[index[[1]]]],
                                       rownamelist[[index[[2]]]])), rownames(un))
  }

  cort <- stats::cor(t(un), method = method)

  if (is.plot == TRUE){
    plot.data <- list(cort, rowidlist)
    qgraph(plot.data[[1]], groups = plot.data[[2]], layout = "spring", minimum = 0.6, legend.cex = 0.5, color = c("red", "blue", "gold", "gray"), edge.label = FALSE)
  } else {
    return(list(cort, rowidlist))
  }

}
#' @rdname PlotNetwork
#' @export
setMethod("PlotNetwork","BRIC", .qunetwork)


