
#' Create DimRduce object
#'
#' @slot PCA ANY.
#' @slot UMAP ANY.
#' @slot TSNE ANY.
#'
#'
setClass("DimReduce",slots = c(
  PCA = "ANY",
  UMAP = "ANY",
  TSNE = "ANY"
)
)
# create Bicluster object
#'
#' @slot block ANY.
#' @slot CoReg_gene ANY.
#' @slot CoCond_cell ANY.
#'

setClass("Bicluster", slots = c(
  CoReg_gene = "ANY",
  CoCond_cell = "ANY",
  Pathway = "ANY"
)
)
# create LTMG object
#'
#' @slot LTMG_discrete matrix.
#' @slot LTMG_BinarySingleSignal matrix.
#' @slot LTMG_BinaryMultisignal matrix.
#' @slot  .
#'
setClass("LTMGr", slots = c(
  LTMG_discrete = "matrix",
  LTMG_BinarySingleSignal = "matrix",
  LTMG_BinaryMultisignal = "matrix",
  DimReduce = "DimReduce",
  Cluster = "ANY",
  MarkerGene = "ANY",
  Pathway = "ANY"
)
)
# set BRIC class
#'
#' @slot raw_count ANY.
#' @slot LTMG_multisignal
#' @slot LTMG_BiSingleSignal
#' @slot LTMG_BiMultisignal
#' @slot processed_count ANY.
#'
#' @return
#' @export
#' @name BRIC
#' @rdname BRIC
#' @exportClass BRIC
setClass("BRIC",
         slots=c(raw_count = "matrix",
                 processed_count = "ANY",
                 MetaInfo = "ANY",
                 Discretization = "matrix",
                 LTMG = "LTMGr",
                 BiCluster = "Bicluster",
         )

)
