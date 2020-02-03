#' @include generics.R
#' @include Classes.R
NULL

#' getblock function
#'
#' @param keyword "Conds" for co-regulatory or "Genes" for co-expression gene
#'
.getBlock <-function( keyword = "Conds"){
  tmp.block <- readLines(paste0(getwd(),"/tmp_expression.txt.chars.blocks"))
  tmp.bc <-grep(keyword, tmp.block, value = T)
  tmp.cel.module <- sapply(strsplit(tmp.bc,':',2),'[',2)
  CONDS <-as.character()   # store the conditions
  label_C <-as.numeric()   # store the occurence of one condistions

  for (j in 1:length(tmp.cel.module)){
    BCcond <-unlist(strsplit(tmp.cel.module[j], split = " "))
    BCcond <-BCcond[BCcond!=""]  # exclude the blank string
    CONDS <-c(CONDS,BCcond)
    label_C <-c(label_C,rep(j,length(BCcond)))
  }
  df_C <-data.frame(cell_name=CONDS,Condition=label_C)
  if(keyword == "Conds"){
    df_C$cell_name <-as.character(df_C$cell_name)
    object@BiCluster@CoCond_cell <- df_C
    return(object)
  } else if(keyword == "Genes"){
    tmp.df_C <- df_C
    tmp.df_C$cell_name <-unlist(sapply(strsplit(as.character(tmp.df_C$cell_name),"_"),"[",1))
    object@BiCluster@CoReg_gene <- tmp.df_C
    return(object)
  }

}

#' run discretization
#'
#' @param object
#' @param q
#'
#' @examples
.runDiscretization <- function(object = NULL, q = 0.05, LogTransformation = FALSE){
  message("writing temporary expression file ...")
  tmp.dir <- paste0(getwd(),"/tmp_expression.txt")
  tmp.count<- object@processed_count
  tmp.count <- cbind(ID=rownames(tmp.count),tmp.count)
  write.table(tmp.count, file = tmp.dir, row.names = F, quote = F, sep = "\t")
  message("create temporary discretize file")
  qubic(i = tmp.dir, Fa = TRUE, q = q, R = LogTransformation)
  tmp.chars <- paste0(getwd(),"/tmp_expression.txt.chars")
  tmp.readin <- read.table(tmp.chars, row.names = 1, header = T)
  object@Discretization <- as.matrix(tmp.readin)
  return(object)
}


#' @export
#' @rdname RunDiscretization
setMethod("RunDiscretization", "BRIC", .runDiscretization)

#' RunBicusterBaseOnLTMG
#'
#' @param object
#' @param OpenDual
#' @param Extention
#' @param NumBlockOutput
#' @param BlockOverlap
#' @param BlockCellMin
#'
#' @examples
.runBiclusterBaseOnLTMG <- function(object = NULL, OpenDual = TRUE, Extension = 0.90,
                                    NumBlockOutput = 100, BlockOverlap = 0.7, BlockCellMin = 15) {
  print("writing LTMG Discretization file ...")
  tmp.dir <- paste0(getwd(),"/tmp_expression.txt.chars")
  tmp.multi <- object@LTMG@LTMG_BinaryMultisignal
  tmp.multi <- cbind(ID = rownames(tmp.multi),tmp.multi)
  write.table(tmp.multi, file = tmp.dir, row.names = F, quote = F, sep = "\t")
  print("finsished!")
  print("running Bicluster . . .")
  qubic(i= tmp.dir, d = TRUE, C = OpenDual, c = Extension, o = NumBlockOutput, f= BlockOverlap, k = BlockCellMin)
  object <- .getBlock(keyword = "Conds")
  object <- .getBlock(keyword = "Genes")
  return(object)
}

#' Title
#'
#' @param object
#' @param OpenDual
#' @param Extention
#' @param NumBlockOutput
#' @param BlockOverlap
#' @param BlockCellMin
#' @examples
.runBiclusterBaseOnDiscretization <- function(object = NULL, OpenDual = TRUE, Extension = 0.90,
                                    NumBlockOutput = 100, BlockOverlap = 0.7, BlockCellMin = 15) {
  tmp.dir <- paste0(getwd(),"/tmp_expression.txt.chars")
  if(file.exists(tmp.dir)){
    qubic(i= tmp.dir, d = TRUE, C = OpenDual, c = Extension, o = NumBlockOutput, f= BlockOverlap, k = BlockCellMin)
  } else{print("please use `RunDiscretization` first and then execute this command")}

  object<- .getBlock(keyword = "Conds")
  object <- .getBlock(keyword = "Genes")
  return(object)
}

#' Run cluster
#'
#' @param object
#' @param DiscretizationModel
#' @param OpenDual
#' @param Extention
#' @param NumBlockOutput
#' @param BlockOverlap
#' @param BlockCellMin
#' @name RunBicluster
#' @return
#' @examples
.runBicluster <- function(object = NULL, DiscretizationModel = "LTMG",OpenDual = TRUE, Extension = 0.90,
                          NumBlockOutput = 100, BlockOverlap = 0.7, BlockCellMin = 15) {
  if(DiscretizationModel != "LTMG" && DiscretizationModel != "Quantile"){stop("please select either LTMG or Quantile")}
  if(DiscretizationModel == "LTMG") {
    .runBiclusterBaseOnLTMG(object = object, OpenDual = OpenDual, Extension = Extension,
                                      NumBlockOutput = NumBlockOutput, BlockOverlap = BlockOverlap, BlockCellMin = BlockCellMin)
  }
  if(DiscretizationModel == "Quantile") {
    .runBiclusterBaseOnDiscretization(object = object, OpenDual = OpenDual, Extension = Extension,
                                      NumBlockOutput = NumBlockOutput, BlockOverlap = BlockOverlap, BlockCellMin = BlockCellMin)
  }
  return(object)

}
#' @export
#' @rdname RunBicluster
setMethod("RunBicluster", "BRIC", .runBicluster)












