#' @include generics.R
#' @include Classes.R
NULL


#' @importFrom clusterProfiler enrichKEGG enrichGO
#' @import org.Mm.eg.db org.Hs.eg.db
#' @importFrom AnnotationDbi select
.runPathway <- function(object = NULL, database = "KEGG", genes.source = "LTMG"){
  if (genes.source == "LTMG"){
    genes.use <- object@LTMG@MarkerGene
  } else if (genes.source == "Bicluster" ){
    genes.use <- object @BiCluster@CoReg_gene
  }


  if(database = "KEGG"){
    markerko.vs.WT.KEGG<-enrichKEGG(gene=genes.use,
                                    organism = "mmu",
                                    keyType = "kegg",pAdjustMethod = "BH",
                                    pvalueCutoff  = 0.01,
                                    qvalueCutoff  = 0.05)
  }
  if(database = "GO"){
    markerko.vs.WT.KEGG<-enrichGO(gene=genes.use,OrgDb = org.Mm.eg.db,
                                  ont="ALL",keyType = "SYMBOL",pAdjustMethod = "BH",
                                  pvalueCutoff  = 0.01,
                                  qvalueCutoff  = 0.05)
  }
}

#' @rdname RunPathway
#' @export
setMethod("RunPathway","BRIC",.runPathway)

