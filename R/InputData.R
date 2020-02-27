#' Read 10X HDF5 file based on Seurat package
#'
#' @param Input input 10X Chromium output data with the extented name as ".h5"
#' @param ... inherit function from Seurat::Read10X_h5
#' @return The output from \code{\link{Read10X_h5}}
#' @export
#' @importFrom Seurat Read10X_h5
#' @examples
#' \dontrun{
#' my.matrix<-ReadFrom10X_h5("path/to/data/files")
#' }
#'
ReadFrom10X_h5<-function(input=NULL,...){
  Input_10X<- Read10X_h5(input)
  Gene_expression<-Input_10X$`Gene Expression`
  return(Gene_expression)
}



#' Read 10X folder based on Seurat package
#'
#' @param Input.dir input 10X Chromium output data by using output folder
#' @param ... inherit function from Seurat::Read10X_h5
#'
#' @return The output from \code{\link{Read10X}}
#' @export
#' @importFrom Seurat Read10X
#'
#' @examples
ReadFrom10X_folder <- function(input.dir=NULL,...){
  Input_10X <- Read10X(input.dir)
  Gene_expression <- Input_10X
  return(Gene_expression)
}


