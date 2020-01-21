library(BRIC)
setwd("D:/my_analysis/BRIC_TEST/")
a<- read.csv("C:/Users/cyz/Documents/BRIC/BRIC/inst/extdata/Pollen/Pollen_expression.csv",header = T,row.names = 1)
object<-CreateBRICObject(as.matrix(a))
object <- BRIC::NormalizeData(object)
object <- RunDiscretization(object)
# # LTMG suite
# object <- BRIC::RunLTMG(object,Gene_use = 2000)
# object <- BRIC::CalBinaryMultiSignal(object)
# object <- BRIC::CalBinarySingleSignal(object)
start.time <- Sys.time()
BRIC::RunBicluster(object = object,DiscretizationModel = "Quantile")
end.time <- Sys.time()
start.time - end.time

# test large cell
setwd("D:/my_analysis/BRIC_TEST/")
a <- ReadFrom10X_folder("C:/Users/cyz/Documents/BRIC/BRIC/inst/extdata/10X_3K/folder_10X/")
object<-CreateBRICObject(as.matrix(a))
object@processed_count <- as.matrix(a)
object <- RunDiscretization(object)
# # LTMG suite
# object <- BRIC::RunLTMG(object,Gene_use = 2000)
# object <- BRIC::CalBinaryMultiSignal(object)
# object <- BRIC::CalBinarySingleSignal(object)
start.time <- Sys.time()
BRIC::RunBicluster(object = object,DiscretizationModel = "Quantile")
end.time <- Sys.time()
start.time - end.time
