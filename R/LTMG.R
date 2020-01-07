#' @include generics.R
#' @include object.R
NULL
#' BIC_LTMG
BIC_LTMG <- function(y, rrr, Zcut) {
  n <- length(y)

  nparams <- nrow(rrr) * 3-1
  w <- rrr[, 1]
  u <- rrr[, 2]
  sig <- rrr[, 3]
  y0 <- y[which(y >= Zcut)]

  cc <- c()
  for (i in 1:nrow(rrr)) {
    c <- dnorm(y0, u[i], sig[i]) * w[i]
    cc <- rbind(cc, c)
  }
  d <- apply(cc, 2, sum)
  e <- sum(log(d))
  f <- nparams * log(n)-e*2
  return (f)
}


#' Pure_CDF
Pure_CDF<-function(Vec){
  ### Vec should be sorted ###
  TEMP<-sort(Vec)
  TOTAL<-length(Vec)
  CDF<-rep(0,length = length(TEMP))
  m<-TEMP[1]
  KEEP<-c(1)
  if(length(TEMP)>1){
    for (i in 2:length(TEMP)) {
      if (TEMP[i]==m) {
        KEEP<-c(KEEP,i)
      }else{
        m<-TEMP[i]
        CDF[KEEP]<-(i-1)/TOTAL
        KEEP<-c(i)
      }
    }
  }
  CDF[KEEP]<-1
  return(CDF)
}


#' KS_LTMG

KS_LTMG<-function(y,rrr,Zcut){
  y<-sort(y)
  num_c<-nrow(rrr)
  y[which(y<Zcut)]<-Zcut-2
  y0<-y[which(y>=Zcut)]
  p_x<-rep(0,length(y0))

  for(j in 1:num_c){
    p_x<-p_x+pnorm(y0,mean=rrr[j,2],sd=rrr[j,3])*rrr[j,1]
  }

  p_uni_x<-Pure_CDF(y)
  p_uni_x<-p_uni_x[which(y>=Zcut)]
  return(max(abs(p_x-p_uni_x)))
}

#' MIN_return
MIN_return<-function(x){
  return(min(x[x>0]))
}

#' State_return
State_return<-function(x){
  return(order(x,decreasing = T)[1])
}

#' MINUS
#' @export
MINUS <- function(x,y){
  if(x<y){
    return(0)
  }else{
    return(x-y)
  }
}



# EvaluateMatrix<- function (object = NULL, ){}
#
.AddNormalNoise <- function (object = NULL,seed = 123){
  set.seed(seed)
  raw<-object@raw_count
  raw <- as.data.frame(raw)
  MAT<-sapply(raw,function(x) x+rnorm(1,0,0.0001))
  MAT <- as.matrix(MAT)
  MAT2<-MAT+matrix(rnorm(ncol(MAT)*nrow(MAT),0,0.0001),ncol=ncol(MAT),nrow=nrow(MAT))
  MAT2 <- as.matrix(MAT2)
  MAT3 <- ifelse(MAT2 < 0, 0, MAT2)
  rownames(MAT3) <- rownames(raw)
  object@LTMG@AddedNoiseMatirx <- MAT3
  return(object)
}


#' @export
#' @rdname AddNormalNoise
setMethod("AddNormalNoise", "BRIC", function (object) .AddNormalNoise(object))


.GetNormalNoiseMatirx <- function(object = NULL) {
  tmp <- object@LTMG@AddedNoiseMatirx
  return(tmp)
}



#' @export
#' @rdname GetNormalNoiseMatirx
setMethod("GetNormalNoiseMatirx","BRIC",.GetNormalNoiseMatirx)
# Run LTMG function --------------------------------------------------------------------
#' RunLTMG
#'
#' @param object
#' @param NFeatures
#' @name RunLTMG
#' @return
#' @export
#' @importFrom AdaptGauss Intersect2Mixtures
#' @importFrom LTMGSCA SeparateKRpkmNew
#' @importFrom mixtools normalmixEM
#' @importFrom stats sd
#' @examples
.RunLTMG <- function (object = NULL, NFeatures = NULL){
  if(nrow(object@LTMG@AddedNoiseMatirx) == 0) {
    object <- AddNormalNoise(object)
  }

  MAT <- object@LTMG@AddedNoiseMatirx
  if(is.null(NFeatures)){
    print(paste("use all ", nrow(MAT)," genes"))
  } else {
    print(paste("Calculating and using top ",NFeatures, " variant genes"))
    All_variance <- apply(MAT, 1, function(x) sqrt(sd(x)))
    All_variance <- sort(All_variance, decreasing = TRUE)
    Top_variance_name <- names(All_variance)[1:NFeatures]
    MAT <- MAT[Top_variance_name,]
  }

  MIN<-apply(MAT, 1, MIN_return)
  MIN<-log(MIN)
  MIN_fit<-normalmixEM(MIN)
  INTER<-Intersect2Mixtures(Mean1 = MIN_fit$mu[1],SD1 = MIN_fit$sigma[1],Weight1 = MIN_fit$lambda[1],
                            Mean2 = MIN_fit$mu[2],SD2 = MIN_fit$sigma[2],Weight2 = MIN_fit$lambda[2])
  Zcut_univ<-INTER$CutX


  MAT_state<-MAT*0
  MAT<-as.matrix(MAT)
  pb <- txtProgressBar(min = 0, max = nrow(MAT), style = 3)
  for (i in 1:nrow(MAT)) {
    VEC<-MAT[i,]
    y<-log(VEC)
    Zcut<-min(log(VEC[VEC>0]))
    if(Zcut<Zcut_univ){
      Zcut<-Zcut_univ
    }

    MARK<-Inf
    rrr_LTMG<-NULL
    for (K in 1:5) {
      rrr<-SeparateKRpkmNew(x = y,n = 100,q = Zcut,k = K)
      rrr<-matrix(as.numeric(rrr[!is.na(rrr[,2]),]),ncol=3,byrow=F)
      TEMP<-BIC_LTMG(y,rrr,Zcut)
      #print(TEMP)
      if(TEMP<MARK){
        rrr_LTMG<-rrr
        MARK<-TEMP
      }
    }

    rrr_LTMG<-rrr_LTMG[order(rrr_LTMG[,2]),]
    rrr_use<-matrix(as.numeric(rrr_LTMG),ncol=3,byrow=F)

    y_use<-y[y>Zcut]
    y_value<-NULL
    for (k in 1:nrow(rrr_use)) {
      TEMP<-dnorm(y_use,mean = rrr_use[k,2],sd = rrr_use[k,3])*rrr_use[k,1]
      y_value<-rbind(y_value,TEMP)
    }
    y_state<-rep(0,length(y))
    y_state[y>Zcut]<-apply(y_value,2,State_return)-1

    MAT_state[i,]<-y_state
    setTxtProgressBar(pb, i)
  }
  close(pb)
  object@LTMG@LTMG_discrete <- as.matrix(MAT_state)
  return(object)
}


#' @export
#' @rdname RunLTMG
setMethod("RunLTMG","BRIC", .RunLTMG)
#----------------------------------------------------------------------------

# get LTMG matrix function-------------------------
.GetLTMGmatrix <- function (object) {
  tmp <- object@LTMG@LTMG_discrete
  return(tmp)
}

#' @export
#' @rdname GetLTMGmatrix
setMethod("GetLTMGmatrix", "BRIC", .GetLTMGmatrix)
# --------------------------------------------------

# calculate single signal function and get function ---------------------
.CalBinarySingleSignal <- function(object = NULL){
  MAT <- object@LTMG@LTMG_discrete
  SingleSignal <- ifelse(MAT > 0, 1, MAT)
  object@LTMG@LTMG_BinarySingleSignal <- SingleSignal
  return(object)
}
#' @export
#' @rdname CalBinarySingleSignal
setMethod("CalBinarySingleSignal", "BRIC", .CalBinarySingleSignal)

.GetBinarySingleSignal <- function(object = NULL) {
  tmp <- object@LTMG@LTMG_BinarySingleSignal
  return(tmp)
}

#' @export
#' @rdname GetBinarySingleSignal
setMethod("GetBinarySingleSignal", "BRIC", .GetBinarySingleSignal)
# --------------------------------------------------------------------------
# calculate multisignal function and get function--------------------------
.CalBinaryMultiSignal <- function(object = NULL){
  x <- object@LTMG@LTMG_discrete
  MultiSig<-c()
  pb <- txtProgressBar(min = 0, max = nrow(x), style = 3)
  for (i in 1:nrow(x)){
      tmp.gene<-x[i,]
      tmp.gene.name<-rownames(x)[i]
      tmp.signal<-max(tmp.gene)
      sub.MultiSig<-c()
      for (j in 1:tmp.signal) {
        tmp.sub.ms<-ifelse(tmp.gene==j,1,0)
        sub.MultiSig<-rbind(sub.MultiSig,tmp.sub.ms)
      }
      rownames(sub.MultiSig)<-paste0(tmp.gene.name,"_",1:tmp.signal)
      MultiSig<-rbind(MultiSig,sub.MultiSig)
      setTxtProgressBar(pb, i)
  }
  close(pb)
  object@LTMG@LTMG_BinaryMultisignal <- MultiSig
  return(object)
}

#' @export
#' @rdname CalBinaryMultiSignal
setMethod("CalBinaryMultiSignal", "BRIC", .CalBinaryMultiSignal)

.GetBinaryMultiSignal <- function(object = NULL){
  tmp <- object@LTMG@LTMG_BinaryMultisignal
  return(tmp)
}

#' @export
#' @rdname GetBinaryMultiSignal
setMethod("GetBinaryMultiSignal", "BRIC", .GetBinaryMultiSignal)








