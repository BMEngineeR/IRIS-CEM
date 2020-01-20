# a<- read.csv("C:/Users/cha224/Documents/BRIC/inst/extdata/Pollen/Pollen_expression.csv",header = T,row.names = 1)
# object<-CreateBRICObject(as.matrix(a))
# object <- BRIC::NormalizeData(object)
# object <- RunDiscretization(object)
# # LTMG suite
# object <- BRIC::RunLTMG(object,Gene_use = 2000)
# object <- BRIC::CalBinaryMultiSignal(object)
# object <- BRIC::CalBinarySingleSignal(object)
# start.time <- Sys.time()
# object <- BRIC::RunBicluster(object = object,DiscretizationModel = "LTMG")
# end.time <- Sys.time()
# totAL <- start.time - end.time
