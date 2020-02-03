# # # # # setwd("c:/analysis_work/BRIC/")
# # # # # a<- read.csv("C:/Users/cha224/Documents/BRIC/inst/extdata/Pollen/Pollen_expression.csv",header = T,row.names = 1)
# # # # # object<-CreateBRICObject(as.matrix(a))
# # # # # object <- BRIC::NormalizeData(object)
# # # object <- .addMeta(object)
# # # # # object <- RunDiscretization(object)
# # # # # # LTMG suite
# # # # # object <- BRIC::RunLTMG(object,Gene_use = 2000)
# # # # # # compare changlin ltmg and mine
# # # # # #####################
# # # # # ####################################
# # # # #
# # # # # object <- BRIC::CalBinaryMultiSignal(object)
# # # # # object <- BRIC::CalBinarySingleSignal(object)
# # # # # start.time <- Sys.time()
# # # # # BRIC::RunBicluster(object = object,DiscretizationModel = "Quantile")
# # # # # end.time <- Sys.time()
# # # # # totAL <- start.time - end.time
# # # # #
# # # # # start.time <- Sys.time()
# # # # # BRIC::RunBicluster(object = object,DiscretizationModel = "LTMG")
# # # # # end.time <- Sys.time()
# # # # # totAL <- start.time - end.time
# # # # #
# # # # # ######################
# # # # # # example to kolo data
# # # # # ####################
# # # # # library(BRIC)
# # # # # setwd("/analysis_work/xudong/FInished_LTMG/LTMG/6.Kolodziejczyk")
# # # # # a<- read.csv("genesymble_expression.csv",header =T,row.names = 1)
# # # # # pathway.dir<-c("T1000","T2000","T4000","T8000")
# # # # # object<-CreateBRICObject(as.matrix(a))
# # # # # object <- BRIC::NormalizeData(object)
# # # # # number <- c(1000,2000,4000,8000)
# # # # # for (i in 1:4){
# # # # #   object <- BRIC::RunLTMG(object,Gene_use = number[i])
# # # # #   object <- BRIC::CalBinaryMultiSignal(object)
# # # # #   object <- BRIC::CalBinarySingleSignal(object)
# # # # #   SignalMatirx <- cbind(ID=rownames(object@LTMG@LTMG_discrete),object@LTMG@LTMG_discrete)
# # # # #   write.table(SignalMatirx,
# # # # #               file = paste0("/analysis_work/xudong/FInished_LTMG/LTMG/",my.path[i],"/",my.path[i],"Discretization_LTMG.txt"),
# # # # #               quote = F, row.names = F,sep = "\t")
# # # # #   Onesign<- cbind(ID=rownames(object@LTMG@LTMG_BinarySingleSignal),object@LTMG@LTMG_BinarySingleSignal)
# # # # #   write.table(Onesign,
# # # # #               file = paste0("/analysis_work/xudong/FInished_LTMG/LTMG/",my.path[i],"/",my.path[i],"OneSign_LTMG.txt"),
# # # # #             quote = F, row.names = F,sep = "\t")
# # # # #   multiSig <- cbind(ID=rownames(object@LTMG@LTMG_BinaryMultisignal),object@LTMG@LTMG_BinaryMultisignal)
# # # # #   write.table(multiSig,
# # # # #               file = paste0("/analysis_work/xudong/FInished_LTMG/LTMG/",my.path[i],"/",my.path[i],"MultiSignal_LTMG.txt"),
# # # # #             quote = F, row.names = F,sep = "\t")
# # # # # }
# # # # #
# # # # # # # # run all samples
# # # setwd("/analysis_work/xudong/FInished_LTMG/LTMG/")
# # # my.path <- read.table("myfile",stringsAsFactors = F)
# # # my.path <- my.path$V1[-13]
# # #
# # # for (i in 2:length(my.path)){
# # #   setwd(paste0("/analysis_work/xudong/FInished_LTMG/LTMG/",my.path[i]))
# # #   a <- read.csv("genesymble_expression.csv",header = T, row.names = 1,check.names = F)
# # #   a <- t(a)
# # #   a[1:5,1:5]
# # #   object<-CreateBRICObject(as.matrix(a))
# # #   object <- BRIC::NormalizeData(object)
# # #   object <- BRIC::RunLTMG(object)
# # #   SignalMatirx <- cbind(ID=rownames(object@LTMG@LTMG_discrete),object@LTMG@LTMG_discrete)
# # #   write.table(SignalMatirx,
# # #               file = paste0("/analysis_work/xudong/FInished_LTMG/LTMG/3.Goolam/Goolam_Discretization_LTMG.txt"),
# # #               quote = F, row.names = F,sep = "\t")
# # # #   Onesign<- cbind(ID=rownames(object@LTMG@LTMG_BinarySingleSignal),object@LTMG@LTMG_BinarySingleSignal)
# # # #   write.table(Onesign,
# # # #               file = paste0("/analysis_work/xudong/FInished_LTMG/LTMG/",my.path[i],"/",my.path[i],"_OneSign_LTMG.txt"),
# # # #               quote = F, row.names = F,sep = "\t")
# # # #   multiSig <- cbind(ID=rownames(object@LTMG@LTMG_BinaryMultisignal),object@LTMG@LTMG_BinaryMultisignal)
# # # #   write.table(multiSig,
# # # #               file = paste0("/analysis_work/xudong/FInished_LTMG/LTMG/",my.path[i],"/",my.path[i],"_MultiSignal_LTMG.txt"),
# # # #               quote = F, row.names = F,sep = "\t")
# # # }
# # #
# # #
# # # #
# # #
# # setwd("/analysis_work/xudong/FInished_LTMG/LTMG_original_data/1.Biase/")
# # a<- read.delim("Biase_expression.csv",sep = ",",header = T,row.names = 1)
# # a[1:5,1:5]
# # my.object <-CreateBRICObject(as.matrix(a))
# # my.object <- NormalizeData(my.object)
# # my.object <-RunLTMG(my.object , Gene_use = 2000)
# # SignalMatirx <- cbind(ID=rownames(object@LTMG@LTMG_discrete),object@LTMG@LTMG_discrete)
#   write.table(SignalMatirx,
#               file = "Biase_Discretization_LTMG.txt",
#               quote = F, row.names = F,sep = "\t")
