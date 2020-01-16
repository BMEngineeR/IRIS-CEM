# create LTMG object
#' Title
#'
#' @slot LTMG_discrete matrix.
#' @slot LTMG_BinarySingleSignal matrix.
#' @slot LTMG_BinaryMultisignal matrix.
#' @slot  .
#'
#' @return
#' @export
#'
#' @examples
setClass("LTMGr", slots = c(
  LTMG_discrete = "matrix",
  LTMG_BinarySingleSignal = "matrix",
  LTMG_BinaryMultisignal = "matrix"
)
)
# create Bicluster object
#' Title
#'
#' @slot block ANY.
#' @slot CoReg_gene ANY.
#' @slot CoCond_cell ANY.
#'
#' @return
#' @export
#'
#' @examples
setClass("Bicluster", slots = c(
  block = "ANY",
  CoReg_gene = "ANY",
  CoCond_cell = "ANY"
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
                 processed_count = "matrix",
                 LTMG = "LTMGr",
                 BiCluster = "Bicluster"
         )

)
