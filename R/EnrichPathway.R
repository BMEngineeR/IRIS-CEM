#' @include generics.R
#' @include Classes.R
NULL

.RunGO <- function(genes.use = NULL, species = "mouse"){
  if(grepl("mouse", species, ignore.case = T)){
    pathway<- invisible(suppressMessages(enrichGO(gene=genes.use,OrgDb = org.Mm.eg.db,
                      ont="ALL",keyType = "SYMBOL",pAdjustMethod = "BH",
                      pvalueCutoff  = 0.01,
                      qvalueCutoff  = 0.05)))
  } else if(grepl("human", species, ignore.case = T)){
    pathway<- invisible(suppressMessages(enrichGO(gene=genes.use,OrgDb = org.Hs.eg.db,
                      ont="ALL",keyType = "SYMBOL",pAdjustMethod = "BH",
                      pvalueCutoff  = 0.01,
                      qvalueCutoff  = 0.05)))
  }
}

.RunKEGG <- function(genes.use = NULL,species = "mouse"){
  if(grepl("mouse", species, ignore.case = T)){
    pathway<-invisible(suppressMessages(enrichKEGG(gene=genes.use,
                        organism = "mmu",
                        keyType = "kegg",pAdjustMethod = "BH",
                        pvalueCutoff  = 0.01,
                        qvalueCutoff  = 0.05)))
  }
  else if(grepl("human", species, ignore.case = T)){
    pathway<-invisible(suppressMessages(enrichKEGG(gene=genes.use,
                        organism = "hsa",
                        keyType = "kegg",pAdjustMethod = "BH",
                        pvalueCutoff  = 0.01,
                        qvalueCutoff  = 0.05)))
  }
}
#' @param object
#'
#' @param module.number
#' @param selected.gene.cutoff
#' @param species "Human" "Mouse"
#' @param database "GO" "KEGG"
#' @param genes.source
#'
#' @importFrom clusterProfiler enrichKEGG enrichGO
#' @import org.Mm.eg.db org.Hs.eg.db
#' @importFrom AnnotationDbi select
.runPathway <- function(object = NULL,module.number = NULL, selected.gene.cutoff = 0.05,
                        species = "Human", database = "GO", genes.source = "CTS"){
  if (genes.source == "CTS"){
    tmp.table<- object@LTMG@MarkerGene
    genes.use.LTMG <- rownames(tmp.table)[tmp.table$pvalue.adj.FDR < selected.gene.cutoff]
    if(database == "GO"){
      pathway <- .RunGO(genes.use = genes.use.LTMG , species = species)
    }else if(database == "KEGG"){
      pathway <- .RunKEGG(genes.use = genes.use.LTMG , species = species)
    }
    object@LTMG@Pathway <- pathway@result
  } else if (genes.source == "Bicluster" ){
    message("there is total ",length(unique(object@BiCluster@CoReg_gene$Condition))," blocks detected, \n please choose number bellow: \n",
            paste(unique(object@BiCluster@CoReg_gene$Condition)," "))
    block.number <- readline("type the block numebr to enrich on Pathway : ")
    genes.use.module <- object@BiCluster@CoReg_gene$cell_name[object@BiCluster@CoReg_gene$Condition == block.number]
    # run on Bicluster marker gene
      if(is.null(object@BiCluster@MarkerGene)){message("There is no gene in MarkerGene slot.
                                                     \n ignore pathway analysis based on marker gene derived from MC defined cell type. ")
        genes.use.MC <- NULL
      }
      else{
        tmp.table<- object@BiCluster@MarkerGene
        genes.use.MC <- rownames(tmp.table)[tmp.table$pvalue.adj.FDR < selected.gene.cutoff]
      }
    gene.list <-c(genes.use.module = list(genes.use.module), genes.use.MC = list(genes.use.MC))
    if(database == "GO"){
      pathway <- lapply(gene.list, function(x) .RunGO(genes.use = x , species = species))
    }else if(database == "KEGG"){
      pathway <- lapply(gene.list, function(x) .RunKEGG(genes.use = x , species = species))
    }
    object@BiCluster@PathwayFromModule <- pathway$genes.use.module@result
    object@BiCluster@PathwayFromMC <- pathway$genes.use.MC@result
  }


return(object)

}

#' @rdname RunPathway
#' @export
setMethod("RunPathway","BRIC",.runPathway)

