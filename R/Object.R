
# set BRIC object
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
CreateBRICObject <- function(x = input_matrix, min.cell = 0, min.gene =0,
                             LTMGr = new(Class = "LTMGr"),
                             Bicluster = new(Class = "Bicluster")) {
  raw.matrix <- as.matrix(x)
  raw.matrix.filterbycell <- raw.matrix[(rowSums(raw.matrix > 0) > min.cell),]
  raw.matrix.filterbygene <- raw.matrix.filterbycell[(colSums(raw.matrix.filterbycell > 0) > min.gene),]
  BRIC_Object<- new(Class = 'BRIC', Raw_count =  raw.matrix.filterbygene)
  return(BRIC_Object)
}
