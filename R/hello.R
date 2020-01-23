# setwd("c:/analysis_work/BRIC/")
# a<- read.csv("C:/Users/cha224/Documents/BRIC/inst/extdata/Pollen/Pollen_expression.csv",header = T,row.names = 1)
# object<-CreateBRICObject(as.matrix(a))
# object <- BRIC::NormalizeData(object)
# object <- RunDiscretization(object)
# # LTMG suite
# object <- BRIC::RunLTMG(object,Gene_use = 2000)
# # compare changlin ltmg and mine
# #####################
# b<- as.matrix(object@processed_count)
# set.seed(123)
# Zcut_G <- log(Global_Zcut(b))
# b<-b[rowSums(b)>0,colSums(b)>0]
#
#
# Gene_use<-rownames(b)[order(apply(b, 1, var),decreasing = T)[1:2000]]
# set.seed(123)
# File_LTMG<-LTMG_MAT(MAT = b,Zcut_G = Zcut_G,Gene_use = Gene_use)
# index.compare<- object@LTMG@LTMG_discrete == as.matrix(File_LTMG$State)
# which(index.compare==F)
#
# ####################################
#
# object <- BRIC::CalBinaryMultiSignal(object)
# object <- BRIC::CalBinarySingleSignal(object)
# start.time <- Sys.time()
# BRIC::RunBicluster(object = object,DiscretizationModel = "Quantile")
# end.time <- Sys.time()
# totAL <- start.time - end.time
#
# start.time <- Sys.time()
# BRIC::RunBicluster(object = object,DiscretizationModel = "LTMG")
# end.time <- Sys.time()
# totAL <- start.time - end.time
#
