
#' @describeIn QUBIC Performs a QUalitative BIClustering.
#'
#' @usage qubic(i, R = FALSE, F = FALSE, d = FALSE, f = 0.85, k = 13, c = 0.90, o = 5000)
#'
#' @importFrom Rcpp evalCpp
qubic <- function(i, N = FALSE, R = FALSE, Fa = FALSE, d = FALSE, D = FALSE, n = FALSE, f = 0.85, k = 13, c = 0.90, o = 5000) {
  vec <- c("./qubic", "-i", i)
  if(N) vec <- c(vec, "-N")
  if(R) vec <- c(vec, "-R")
  if(Fa) vec <- c(vec, "-F")
  if(d) vec <- c(vec, "-d")
  if(D) vec <- c(vec, "-D")
  if(n) vec <- c(vec, "-n")
  vec <- c(vec, "-f", as.character(f))
  vec <- c(vec, "-k", as.character(k))
  vec <- c(vec, "-c", as.character(c))
  vec <- c(vec, "-o", as.character(o))

  unloadNamespace("BRIC")
  ret <- .main(vec)
  if(ret == 42) return(BRIC::qubic(paste0(i, ".chars"), d = TRUE))
  return (ret)
}


.onUnload <- function (libpath) {
  library.dynam.unload("BRIC", libpath)
}


#' RunDiscretization
#'
#' @param object
#' @param q
#'
#' @return
#' @export
#'

.runDiscretization <- function(object = obejct, q = 0.05){
  print("writing tmp expression file ...")
  tmp.dir <- paste(getwd(),"/tmp_expression.txt")
  write.table(object@raw_count, file = tmp.dir, row.names = T, quote = F, sep = "\t")
  qubic(i = tmp.dir, Fa = TRUE, q = 0.05)
  tmp
}

setMethod("RunDiscretization","BRIC",.runDiscretization)






