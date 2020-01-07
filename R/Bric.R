#' @describeIn QUBIC Performs a QUalitative BIClustering.
#'
#' @usage qubic(i, R = FALSE, F = FALSE, d = FALSE, f = 0.85, k = 13, c = 0.90, o = 5000)
#'
#' @importFrom Rcpp evalCpp
#' @export
qubic <- function(i, N = FALSE, R = FALSE, F = FALSE, d = FALSE, D = FALSE, n = FALSE, f = 0.85, k = 13, c = 0.90, o = 5000) {
  vec <- c("./qubic", "-i", i)
  if(N) vec <- c(vec, "-N")
  if(R) vec <- c(vec, "-R")
  if(F) vec <- c(vec, "-F")
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
