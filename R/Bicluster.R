#' @include generics.R
#' @include Classes.R
NULL
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
  tmp.dir <- paste0(getwd(),"/LTMG.chars")
  tmp.multi <- object@LTMG@LTMG_BinaryMultisignal
  tmp.multi <- cbind(ID = rownames(tmp.multi),tmp.multi)
  write.table(object@LTMG@LTMG_BinaryMultisignal, file = tmp.dir, row.names = T, quote = F, sep = "\t")
  print("finsished!")
  print("running Bicluster . . .")
  qubic(i= tmp.dir, d = TRUE, C = OpenDual, c = Extension, o = NumBlockOutput, f= BlockOverlap, k = BlockCellMin)
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

}
#' @export
#' @rdname RunBicluster
setMethod("RunBicluster", "BRIC", .runBicluster)












