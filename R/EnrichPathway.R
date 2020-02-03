#' @include generics.R
#' @include Classes.R
NULL


#' @importFrom clusterProfiler enrichKEGG enrichGO
#' @import org.Mm.eg.db org.Hs.eg.db
#' @importFrom AnnotationDbi select
.runPathway <- function(object = NULL,customize.genelist = NULL,
                        module.number = NULL,
                        source = "Human", database = "GO", genes.source = "LTMG"){
  if (genes.source == "LTMG"){
    tmp.table<- object@LTMG@MarkerGene
    genes.use <- rownames(tmp.table)[tmp.table$pvalue.adj.FDR < 0.05]
  } else if (genes.source == "Bicluster" ){
    message("there is total ",length(unique(object@BiCluster@CoReg_gene$Condition))," blocks detected, \n please choose number bellow: \n",
            paste(unique(object@BiCluster@CoReg_gene$Condition)," "))
    block.number <- readline("type the block numebr to enrich on Pathway : ")
    genes.use <- object@BiCluster@CoReg_gene$cell_name[object@BiCluster@CoReg_gene$Condition == block.number]
  }
  if (is.null(customize.genelist)){
    genes.use <- genes.use
  } else {genes.use <- customize.genelist}
  if(source == "Mouse"){
    if(database == "KEGG"){
      pathway<-enrichKEGG(gene=genes.use,
                                      organism = "mmu",
                                      keyType = "kegg",pAdjustMethod = "BH",
                                      pvalueCutoff  = 0.01,
                                      qvalueCutoff  = 0.05)
    }
    if(database == "GO"){
      pathway<-enrichGO(gene=genes.use,OrgDb = org.Mm.eg.db,
                                    ont="ALL",keyType = "SYMBOL",pAdjustMethod = "BH",
                                    pvalueCutoff  = 0.01,
                                    qvalueCutoff  = 0.05)
    }
  }
  if(source == "Human"){
    if(database == "KEGG"){
      pathway<-enrichKEGG(gene=genes.use,
                          organism = "hsa",
                          keyType = "kegg",pAdjustMethod = "BH",
                          pvalueCutoff  = 0.01,
                          qvalueCutoff  = 0.05)
    }
    if(database == "GO"){
      pathway<-enrichGO(gene=genes.use,OrgDb = org.Hs.eg.db,
                        ont="ALL",keyType = "SYMBOL",pAdjustMethod = "BH",
                        pvalueCutoff  = 0.01,
                        qvalueCutoff  = 0.05)
    }
  }
  if (genes.source == "LTMG"){
    object@LTMG@Pathway <- pathway@result
  }
  if (genes.source == "Bicluster"){
    object@BiCluster@Pathway <- pathway@result
  }
return(object)

}

#' @rdname RunPathway
#' @export
setMethod("RunPathway","BRIC",.runPathway)

