
# set BRIC object
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
CreateBRICObject <- function(x = input_matrix,
                             LTMGr = new(Class = "LTMGr"),
                             Bicluster = new(Class = "Bicluster")) {
  BRIC_Object<- new(Class = 'BRIC', raw_count = x)
  return(BRIC_Object)
}



