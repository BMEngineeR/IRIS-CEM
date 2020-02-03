#' @include generics.R
#' @include Classes.R
NULL

#' Create addmeta function for adding meta information
#'
#' @param object BRIC object
#' @param meta.info meta information table should be a data frame with rows representing cell and coloumn representing different group condition
#'
#' @return
#' @export
#'
#' @examples
.addMeta <- function(object = NULL, meta.info = NULL){
  if(is.null(meta.info)){
    message("Do not provide meta info table for the object.")
    message("using original cell identity")
    meta.info <- data.frame(row.names = as.character(colnames(object@raw_count)),
                            Original = as.character(colnames(object@raw_count)),
                            ncount_RNA = as.numeric(colSums(object@raw_count)))
    object@MetaInfo <- meta.info
  } else{
    if (colnames(object@raw_count) != rownames(meta.info)){
      stop("\n There is inconsisten cell names between meta info and original raw count.
           \n Please check rownames of meta info and colnames of original raw count.")
    } else {
        tmp.meta <- cbind(ncount_RNA = as.numeric(colSums(object@raw_count)), meta.info)
        object@MetaInfo <- tmp.meta
      }
  }
  return(object)
}

#' @rdname AddMeta
#' @export
setMethod("AddMeta", "BRIC", .addMeta)
