
# META-ANALYSIS
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    

#' @return metaResult object 
#' @examples
#' doMetaAnalysis<-function(nsims=50,metaAnalysis=makeMetaAnalysis(),
#'                          hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,
#'                          metaResult=NULL) {
#' @export
doMetaAnalysis<-function(nsims=100,metaResult=braw.res$metaMultiple,metaAnalysis=braw.def$metaAnalysis,
                         hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence
                         ) {
  if (is.null(metaAnalysis)) metaAnalysis<-makeMetaAnalysis()
  if (nsims==1) metaResult<-NULL
  evidence$sigOnly<-metaAnalysis$sigOnlySource
  
  for (i in 1:nsims) {
    studies<-multipleAnalysis(metaAnalysis$nstudies,hypothesis,design,evidence)
    metaResult<-runMetaAnalysis(metaAnalysis,studies,metaResult)
  }
  metaResult$count<-length(metaResult$bestDist)
  metaResult$hypothesis<-hypothesis
  metaResult$design<-design
  setBrawRes("metaResult",metaResult)
  if (nsims>1) setBrawRes("metaMultiple",metaResult)
  else         setBrawRes("metaSingle",metaResult)
  metaResult
}


getMaxLikelihood<-function(zs,ns,df1,dist,metaAnalysis) {
  nkpoints<-13
  nnullpoints<-13
  
  niterations<-1
  reInc<-(nkpoints-1)/2/3
  
  if (metaAnalysis$includeNulls) {
    nullvals<-seq(0,1,length.out=nnullpoints)
  } else {
    nullvals<-0
  }
  
  if (dist=="Single") {
    kvals<-seq(-1,1,length.out=nkpoints)*0.95
  } else {
    kvals<-seq(0.01,1,length.out=nkpoints)
  }
  
  remove_nonsig<-metaAnalysis$includeBias
  for (re in 1:niterations) {
    S<-getLogLikelihood(zs,ns,df1,dist,kvals,nullvals,remove_nonsig)
    Smax<-max(S,na.rm=TRUE)
    
    use<-which(S==Smax, arr.ind = TRUE)
    Nullmax<-nullvals[use[1,2]]
    lb2<-nullvals[max(1,use[1,2]-reInc)]
    ub2<-nullvals[min(length(nullvals),use[1,2]+reInc)]
    Kmax<-kvals[use[1,1]]
    lb1<-kvals[max(1,use[1,1]-reInc)]
    ub1<-kvals[min(length(kvals),use[1,1]+reInc)]
    
    kvals<-seq(lb1,ub1,length.out=nkpoints)
    if (metaAnalysis$includeNulls) {
      nullvals<-seq(lb2,ub2,length.out=nnullpoints)
    }
  }
  
  if (niterations==1) {      
    fun<-function(x) { -getLogLikelihood(zs,ns,df1,dist,x[1],x[2],remove_nonsig)}
    result<-fmincon(c(Kmax,Nullmax),fun,ub=c(ub1,ub2),lb=c(lb1,lb2))
    Kmax<-result$par[1]
    Nullmax<-result$par[2]
    Smax<- -result$value
  }
  return(list(Kmax=Kmax,Nullmax=Nullmax,Smax=Smax))
}


runMetaAnalysis<-function(metaAnalysis,studies,metaResult){
  rs<-studies$rIV
  zs<-atanh(rs)
  ns<-studies$nval
  df1<-studies$df1
  
  if (metaAnalysis$analysisType=="fixed") {
    metaAnalysis$includeNulls<-FALSE
    single<-getMaxLikelihood(zs,ns,df1,"Single",metaAnalysis)
    gauss<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    exp<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    
  } else {
    
    # doing random effects analysis
    
    # find best Single 
    if (metaAnalysis$modelPDF=="Single" || (metaAnalysis$modelPDF=="All")) {
      single<-getMaxLikelihood(zs,ns,df1,"Single",metaAnalysis)
    } else {
      single<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    }
    
    # find best Gauss
    if (metaAnalysis$modelPDF=="Gauss" || metaAnalysis$modelPDF=="All") {
      gauss<-getMaxLikelihood(zs,ns,df1,"Gauss",metaAnalysis)
    } else {
      gauss<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    }
    
    # find best Exp
    if (metaAnalysis$modelPDF=="Exp" || metaAnalysis$modelPDF=="All") {
      exp<-getMaxLikelihood(zs,ns,df1,"Exp",metaAnalysis)
    } else {
      exp<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    }
    
  }
  
  use<-which.max(c(single$Smax,gauss$Smax,exp$Smax))
  bestDist<-c("Single","Gauss","Exp")[use]
  bestK<-c(single$Kmax,gauss$Kmax,exp$Kmax)[use]
  bestNull<-c(single$Nullmax,gauss$Nullmax,exp$Nullmax)[use]
  bestS<-c(single$Smax,gauss$Smax,exp$Smax)[use]
  
  if (!is.null(metaResult)) {
    bestDist<-c(metaResult$bestDist,bestDist)
    bestK<-c(metaResult$bestK,bestK)
    bestNull<-c(metaResult$bestNull,bestNull)
    bestS<-c(metaResult$bestS,bestS)
    single$Kmax<-c(metaResult$single$Kmax,single$Kmax)
    single$Smax<-c(metaResult$single$Smax,single$Smax)
    single$Nullmax<-c(metaResult$single$Nullmax,single$Nullmax)
    gauss$Kmax<-c(metaResult$gauss$Kmax,gauss$Kmax)
    gauss$Smax<-c(metaResult$gauss$Smax,gauss$Smax)
    gauss$Nullmax<-c(metaResult$gauss$Nullmax,gauss$Nullmax)
    exp$Kmax<-c(metaResult$exp$Kmax,exp$Kmax)
    exp$Smax<-c(metaResult$exp$Smax,exp$Smax)
    exp$Nullmax<-c(metaResult$exp$Nullmax,exp$Nullmax)
  }
  
  metaResult<-list(single=single,
                   gauss=gauss,
                   exp=exp,
                   bestDist=bestDist,
                   bestK=bestK,
                   bestNull=bestNull,
                   bestS=bestS,
                   count=length(metaResult$bestDist),
                   metaAnalysis=metaAnalysis,
                   result=studies
  )
  return(metaResult)
}

