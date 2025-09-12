#' make multiple samples whilst varying a parameter
#' 
#' @param exploreType "rIV","Heteroscedasticity","rIV2","rIVIV2","rIVIV2DV" \cr
#'                    "p(R+)","Lambda" \cr
#'                    "n","Method","Usage","WithinCorr","ClusterRad","SampleSD" \cr
#'                     "Dependence","Outliers","NonResponse","IVRange","IVRangeC","IVRangeE","DVRange" \cr
#'                     "Cheating","CheatingAmount" \cr
#'                     "Alpha","Transform","InteractionOn" \cr
#'                     "Power","Keep","Repeats" \cr
#' @returns explore object
#' @seealso doExplore() 
#' @seealso showExplore() 
#' @seealso reportExplore()
#' @examples
#' explore<-makeExplore(exploreType="n",exploreNPoints=11,
#'                              minVal=10,maxVal=250,xlog=FALSE)
#' @export
makeExplore<-function(exploreType="n",exploreNPoints=11,
                      vals=NULL,minVal=NA,maxVal=NA,xlog=FALSE
) {
  if (exploreType=="alpha") exploreType<-"Alpha"
  if (exploreType=="pRPlus") exploreType<-"p(R+)"
  if (exploreType=="meanRPlus") exploreType<-"mean(R+)"
  
  explore<-list(exploreType=exploreType,
                exploreNPoints=exploreNPoints,
                minVal=minVal,maxVal=maxVal,xlog=xlog
  )
  if (!is.null(vals)) explore$minVal<-vals
  
  if (any(is.na(explore$minVal))) {
    range<-getExploreRange(explore)
    explore$minVal<-range$minVal
    explore$maxVal<-range$maxVal
    explore$exploreNPoints<-range$np
    explore$xlog<-range$logScale
  }
  return(explore)
}

getExploreRange<-function(explore) {
  
  exploreType<-explore$exploreType
  if (is.element(exploreType,c("rIV2","rIVIV2","rIVIV2DV"))) exploreType<-"rs"
  if (is.element(exploreType,c("IVskew","DVskew","Heteroscedasticity","Dependence","Outliers","NonResponse"))) exploreType<-"anom"
  if (is.element(exploreType,c("IVRange","IVRangeC","DVRange"))) exploreType<-"anom1"
  if (is.element(exploreType,c("IVRangeE"))) exploreType<-"anom2"
  if (is.element(exploreType,c("IVkurtosis","DVkurtosis"))) exploreType<-"kurt"
  
  switch(exploreType,
         "n"=range<-list(minVal=10,maxVal=250,logScale=TRUE,np=13),
         "NoSplits"=range<-list(minVal=1,maxVal=32,logScale=TRUE,np=6),
         "rIV"=range<-list(minVal=0,maxVal=0.75,logScale=FALSE,np=13),
         "rSD"=range<-list(minVal=0,maxVal=0.4,logScale=FALSE,np=13),
         "rs"=range<-list(minVal=-0.75,maxVal=0.75,logScale=FALSE,np=13),
         "anom"=range<-list(minVal=0,maxVal=1,logScale=FALSE,np=13),
         "anom1"=range<-list(minVal=0.1,maxVal=3,logScale=FALSE,np=13),
         "anom2"=range<-list(minVal=-3,maxVal=3,logScale=FALSE,np=13),
         "kurt"=range<-list(minVal=-1.5,maxVal=1.3,logScale=FALSE,np=13),
         "IVprops"=range<-list(minVal=0.2,maxVal=0.8,logScale=FALSE,np=13),
         "DVprops"=range<-list(minVal=0.2,maxVal=0.8,logScale=FALSE,np=13),
         "IVlevels"=range<-list(minVal=3,maxVal=10,logScale=FALSE,np=8),
         "DVlevels"=range<-list(minVal=3,maxVal=10,logScale=FALSE,np=8),
         "IVcats"=range<-list(minVal=2,maxVal=6,logScale=FALSE,np=5),
         "DVcats"=range<-list(minVal=2,maxVal=6,logScale=FALSE,np=5),
         "WithinCorr"=range<-list(minVal=0,maxVal=1,logScale=FALSE,np=13),
         "Alpha"=range<-list(minVal=0.001,maxVal=0.5,logScale=TRUE,np=13),
         "minRp"=range<-list(minVal=0.0,maxVal=0.5,logScale=FALSE,np=13),
         "Power"=range<-list(minVal=0.1,maxVal=0.9,logScale=FALSE,np=13),
         "Repeats"=range<-list(minVal=0,maxVal=8,logScale=FALSE,np=9),
         "p(R+)"=range<-list(minVal=0,maxVal=1,logScale=FALSE,np=13),
         "mean(R+)"=range<-list(minVal=0.1,maxVal=1,logScale=FALSE,np=13),
         "PoorSamplingAmount"=range<-list(minVal=0, maxVal=0.2,logScale=FALSE,np=13),
         "CheatingAmount"=range<-list(minVal=0, maxVal=0.8,logScale=FALSE,np=13),
         "ClusterRad"=range<-list(minVal=0, maxVal=1,logScale=FALSE,np=13),
         "SampleSD"=range<-list(minVal=1, maxVal=100,logScale=TRUE,np=13),
         "NoStudies"=range<-list(minVal=2,maxVal=200,logScale=TRUE,np=13),
         {range<-list(minVal=0,maxVal=1,logScale=FALSE,np=2)}
  )
  return(range)
}

summariseResult<-function(result) {
  if (!is.null(result$rIV)) {
    sigs<-isSignificant(method=braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,result$result$evidence)
    result$nSig<-sum(sigs)
    result$nFP<-sum(sigs & result$rpIV==0)
    result$rIV<-mean(result$rIV)
    result$pIV<-mean(result$pIV)
    result$rpIV<-mean(result$rpIV)
    result$roIV<-mean(result$roIV)
    result$poIV<-mean(result$poIV)
    result$nval<-mean(result$nval)
    result$df1<-mean(result$df1)

    if (!is.null(result$AIC)) {
      result$AIC<-mean(result$AIC)
      result$AICnull<-mean(result$AICnull)
    }
    if (!is.null(result$sem))
      result$sem<-result$sem[1,8]
    
    if (!is.null(result$iv)) {
      result$iv.mn<-mean(result$iv.mn)
      result$iv.sd<-mean(result$iv.sd)
      result$iv.sk<-mean(result$iv.sk)
      result$iv.kt<-mean(result$iv.kt)
      result$dv.mn<-mean(result$dv.mn)
      result$dv.sd<-mean(result$dv.sd)
      result$dv.sk<-mean(result$dv.sk)
      result$dv.kt<-mean(result$dv.kt)
      result$er.mn<-mean(result$er.mn)
      result$er.sd<-mean(result$er.sd)
      result$er.sk<-mean(result$er.sk)
      result$er.kt<-mean(result$er.kt)
    }
    
    if (!is.null(result$rIV2)){
      result$rIV2<-mean(result$rIV2)
      result$pIV2<-mean(result$pIV2)
      result$rIVIV2DV<-mean(result$rIVIV2DV)
      result$pIVIV2DV<-mean(result$pIVIV2DV)
      
      n<-length(result$r$direct)
      result$r$direct<-result$r$direct
      result$r$unique<-result$r$unique
      result$r$total<-result$r$total
      
      result$p$direct<-result$p$direct
      result$p$unique<-result$p$unique
      result$p$total<-result$p$total
    }
  } else {
    param1<-max(c(result$fixed$param1,result$random$param1,result$single$param1,result$gauss$param1,result$exp$param1),na.rm=TRUE)
    param2<-max(c(result$fixed$param2,result$random$param2,result$single$param2,result$gauss$param2,result$exp$param2),na.rm=TRUE)
    param3<-max(c(result$fixed$param3,result$random$param3,result$single$param3,result$gauss$param3,result$exp$param3),na.rm=TRUE)
    S<-max(c(result$fixed$Smax,result$random$Smax,result$single$Smax,result$gauss$Smax,result$exp$Smax),na.rm=TRUE)
    result$param1<-param1
    result$param2<-param2
    result$param3<-param3
    result$S<-S
    sigs<-isSignificant(braw.env$STMethod,result$result$pIV,result$result$rIV,result$result$nval,result$result$df1,result$result$evidence)
    nSig<-sum(sigs)
    result$nSig<-nSig
  }
  return(result)
  
  result<-list(rval=mean(res),pval=b,rpval=b,raval=b,roval=b,poval=b,nval=b,df1=b,
               nSig=b,nFP=b,
               AIC=b,AICnull=b,sem=b,
               iv.mn=b,iv.sd=b,iv.sk=b,iv.kt=b,
               dv.mn=b,dv.sd=b,dv.sk=b,dv.kt=b,
               er.mn=b,er.sd=b,er.sk=b,er.kt=b,
               rIV2=b,rIVIV2DV=b,pIV2=b,pIVIV2DV=b,
               r=list(direct=bm,unique=bm,total=bm),
               p=list(direct=bm,unique=bm,total=bm),
               param1=b,param2=b,param3=b,S=b
  )
  return(result)
}

resetExploreResult<-function(nsims,n_vals,oldResult=NULL) {
  
  if (nsims>0) {
    b<-array(NA,c(nsims,n_vals))
    bm<-array(NA,c(nsims,n_vals,3))
  } else {
    b<-NULL
    bm<-NULL
  }
  
  result<-list(rval=b,pval=b,rpval=b,raval=b,roval=b,poval=b,nval=b,df1=b,
               nSig=b,nFP=b,
               AIC=b,AICnull=b,sem=b,
               iv.mn=b,iv.sd=b,iv.sk=b,iv.kt=b,
               dv.mn=b,dv.sd=b,dv.sk=b,dv.kt=b,
               er.mn=b,er.sd=b,er.sk=b,er.kt=b,
               rIV2=b,rIVIV2DV=b,pIV2=b,pIVIV2DV=b,
               r=list(direct=bm,unique=bm,total=bm),
               p=list(direct=bm,unique=bm,total=bm),
               param1=b,param2=b,param3=b,S=b
  )
  if (!is.null(oldResult)) {
    result<-mergeExploreResult(oldResult,result)
  }
  return(result)
}
storeExploreResult<-function(result,res,ri,vi) {
  if (!is.null(res$rIV)) {
    result$rval[ri,vi]<-res$rIV
    result$pval[ri,vi]<-res$pIV
    result$rpval[ri,vi]<-res$rpIV
    result$roval[ri,vi]<-res$roIV
    result$poval[ri,vi]<-res$poIV
    result$nval[ri,vi]<-res$nval
    result$df1[ri,vi]<-res$df1
    result$nSig[ri,vi]<-res$nSig
    result$nFP[ri,vi]<-res$nFP
    
    if (!is.null(res$AIC)) {
      result$AIC[ri,vi]<-res$AIC
      result$AICnull[ri,vi]<-res$AICnull
    }
    if (!is.null(res$sem))
      result$sem[ri,vi]<-res$sem[1,8]
    
    if (!is.null(res$iv)) {
      result$iv.mn[ri,vi]<-res$iv.mn
      result$iv.sd[ri,vi]<-res$iv.sd
      result$iv.sk[ri,vi]<-res$iv.sk
      result$iv.kt[ri,vi]<-res$iv.kt
      result$dv.mn[ri,vi]<-res$dv.mn
      result$dv.sd[ri,vi]<-res$dv.sd
      result$dv.sk[ri,vi]<-res$dv.sk
      result$dv.kt[ri,vi]<-res$dv.kt
      result$er.mn[ri,vi]<-res$er.mn
      result$er.sd[ri,vi]<-res$er.sd
      result$er.sk[ri,vi]<-res$er.sk
      result$er.kt[ri,vi]<-res$er.kt
    }
    
    if (!is.null(res$rIV2)){
      result$rIV2[ri,vi]<-res$rIV2
      result$pIV2[ri,vi]<-res$pIV2
      result$rIVIV2DV[ri,vi]<-res$rIVIV2DV
      result$pIVIV2DV[ri,vi]<-res$pIVIV2DV
      
      n<-length(res$r$direct)
      result$r$direct[ri,vi,1:n]<-res$r$direct
      result$r$unique[ri,vi,1:n]<-res$r$unique
      result$r$total[ri,vi,1:n]<-res$r$total
      
      result$p$direct[ri,vi,1:n]<-res$p$direct
      result$p$unique[ri,vi,1:n]<-res$p$unique
      result$p$total[ri,vi,1:n]<-res$p$total
    }
  } else {
    param1<-max(c(res$fixed$param1,res$random$param1,res$single$param1,res$gauss$param1,res$exp$param1),na.rm=TRUE)
    param2<-max(c(res$fixed$param2,res$random$param2,res$single$param2,res$gauss$param2,res$exp$param2),na.rm=TRUE)
    param3<-max(c(res$fixed$param3,res$random$param3,res$single$param3,res$gauss$param3,res$exp$param3),na.rm=TRUE)
    S<-max(c(res$fixed$Smax,res$random$Smax,res$single$Smax,res$gauss$Smax,res$exp$Smax),na.rm=TRUE)
    sigs<-isSignificant(braw.env$STMethod,res$result$pIV,res$result$rIV,res$result$nval,res$result$df1,res$result$evidence)
    nSig<-sum(sigs)
    result$param1[ri,vi]<-param1
    result$param2[ri,vi]<-param2
    result$param3[ri,vi]<-param3
    result$S[ri,vi]<-S
    result$nSig[ri,vi]<-nSig
  }
  return(result)
}

mergeExploreResult<-function(res1,res2) {
  # abind<-function(a,b) array(c(a, b), dim = c(dim(a)[1]+dim(b)[1], dim(a)[2], dim(a)[3]))
  
  result<-res1
  result$rval<-rbind(res1$rval,res2$rval)
  result$pval<-rbind(res1$pval,res2$pval)
  result$rpval<-rbind(res1$rpval,res2$rpval)
  result$raval<-rbind(res1$raval,res2$raval)
  result$roval<-rbind(res1$roval,res2$roval)
  result$poval<-rbind(res1$poval,res2$poval)
  result$nval<-rbind(res1$nval,res2$nval)
  result$df1<-rbind(res1$df1,res2$df1)
  result$AIC<-rbind(res1$AIC,res2$AIC)
  result$AICnull<-rbind(res1$AICnull,res2$AICnull)
  if (!is.null(res1$sem))
    result$sem<-rbind(res1$sem,res2$sem)
  else 
    result$sem<-NULL
  
  result$iv.mn<-rbind(res1$iv.mn,res2$iv.mn)
  result$iv.sd<-rbind(res1$iv.sd,res2$iv.sd)
  result$iv.sk<-rbind(res1$iv.sk,res2$iv.sk)
  result$iv.kt<-rbind(res1$iv.kt,res2$iv.kt)
  result$dv.mn<-rbind(res1$dv.mn,res2$dv.mn)
  result$dv.sd<-rbind(res1$dv.sd,res2$dv.sd)
  result$dv.sk<-rbind(res1$dv.sk,res2$dv.sk)
  result$dv.kt<-rbind(res1$dv.kt,res2$dv.kt)
  result$er.mn<-rbind(res1$er.mn,res2$er.mn)
  result$er.sd<-rbind(res1$er.sd,res2$er.sd)
  result$er.sk<-rbind(res1$er.sk,res2$er.sk)
  result$er.kt<-rbind(res1$er.kt,res2$er.kt)
  
  result$r$direct<-abind(res1$r$direct,res2$r$direct,along=1)
  result$r$unique<-abind(res1$r$unique,res2$r$unique,along=1)
  result$r$total<-abind(res1$r$total,res2$r$total,along=1)
  
  result$p$direct<-abind(res1$p$direct,res2$p$direct,along=1)
  result$p$unique<-abind(res1$p$unique,res2$p$unique,along=1)
  result$p$total<-abind(res1$p$total,res2$p$total,along=1)
  
  if (!is.null(res1$rIV2)) {
    result$rIV2<-rbind(res1$rIV2,res2$rIV2)
    result$pIV2<-rbind(res1$pIV2,res2$pIV2)
    result$rIVIV2DV<-rbind(res1$rIVIV2DV,res2$rIVIV2DV)
    result$pIVIV2DV<-rbind(res1$pIVIV2DV,res2$pIVIV2DV)
  }
  
  result$param1<-rbind(res1$param1,res2$param1)
  result$param2<-rbind(res1$param2,res2$param2)
  result$param3<-rbind(res1$param3,res2$param3)
  result$S<-rbind(res1$S,res2$S)
  result$nSig<-rbind(res1$nSig,res2$nSig)
  result$nFP<-rbind(res1$nFP,res2$nFP)
  
  return(result)
}

#' make multiple samples whilst varying a parameter
#' 
#' @param exploreType "rIV","Heteroscedasticity","rIV2","rIVIV2","rIVIV2DV" \cr
#'                    "p(R+)","Lambda" \cr
#'                    "n","Method","Usage","WithinCorr","ClusterRad","SampleSD" \cr
#'                     "Dependence","Outliers","NonResponse","IVRange","IVRangeC","IVRangeE","DVRange" \cr
#'                     "Cheating","CheatingAmount" \cr
#'                     "Alpha","Transform","InteractionOn" \cr
#'                     "Power","Keep","Repeats" \cr
#' @returns exploreResult object
#' @seealso showExplore() 
#' @seealso reportExplore()
#' @examples
#' exploreResult<-doExplore(nsims=10,exploreResult=NULL,explore=braw.def$explore,
#'                              doingNull=FALSE,autoShow=FALSE,showType="Basic")
#' @export
doExplore<-function(nsims=10,exploreResult=NA,explore=braw.def$explore,
                    hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,metaAnalysis=braw.def$metaAnalysis,
                    doingNull=FALSE,doingMetaAnalysis=FALSE,autoShow=FALSE,showType="rs"
) {
  oldHypothesis<-braw.def$hypothesis
  oldDesign<-braw.def$design
  oldEvidence<-braw.def$evidence
  oldMetaAnalysis<-braw.def$metaAnalysis
  on.exit(setBrawDef("hypothesis",oldHypothesis))
  on.exit(setBrawDef("design",oldDesign),add=TRUE)
  on.exit(setBrawDef("evidence",oldEvidence),add=TRUE)
  on.exit(setBrawDef("metaAnalysis",oldMetaAnalysis),add=TRUE)
  oldAlpha<-braw.env$alphaSig
  on.exit(setBrawEnv("alphaSig",oldAlpha),add=TRUE)
  
  autoShowLocal<-braw.env$autoShow
  assign("autoShow",FALSE,braw.env)
  
  if (length(exploreResult)==1 && is.na(exploreResult)) exploreResult<-braw.res$explore
  
  if (length(exploreResult)>1) {
    if (identical(hypothesis,braw.res$explore$hypothesis) &&
        identical(design,braw.res$explore$design) &&
        identical(evidence,braw.res$explore$evidence) && 
        identical(explore,braw.res$explore$explore) 
         )
         exploreResult<-braw.res$explore
    else exploreResult<-NULL
  }
  if (doingMetaAnalysis && !identical(metaAnalysis,braw.res$explore$metaAnalysis))
      exploreResult<-NULL
      
  if (is.null(exploreResult)) {
    exploreResult<-list(type="explore",
                        count=0,
                        result=NULL,
                        nullcount=0,
                        nullresult=NULL,
                        vals=NA,
                        explore=explore,
                        hypothesis=hypothesis,
                        design=design,
                        evidence=evidence,
                        doingMetaAnalysis=doingMetaAnalysis,
                        metaAnalysis=metaAnalysis
    )
  }
  explore<-exploreResult$explore
  
  if (doingNull && !hypothesis$effect$world$On) {
    hypothesisNull<-hypothesis
    hypothesisNull$effect$rIV<-0
    # catch up - make enough null results to match results
    if (exploreResult$nullcount<exploreResult$count) {
      ns<-exploreResult$count-exploreResult$nullcount
      exploreResult <- runExplore(0,exploreResult,doingNull=TRUE,autoShow=FALSE)
    }
  }
  
  exploreResult <- runExplore(nsims=nsims,exploreResult,doingNull=doingNull,
                              doingMetaAnalysis=doingMetaAnalysis,
                              autoShow=autoShow,showType=showType)
  assign("autoShow",autoShowLocal,braw.env)
  
  return(exploreResult)
}

runExplore <- function(nsims,exploreResult,doingNull=FALSE,doingMetaAnalysis=FALSE,
                       autoShow=FALSE,showType="rs"){
  max_r<-0.9
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  design<-exploreResult$design
  evidence<-exploreResult$evidence
  metaAnalysis<-exploreResult$metaAnalysis
  
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  
  design$sNReps<-1
  
  if (hypothesis$effect$world$On && hypothesis$effect$world$pRPlus<1) 
    doingNull<-FALSE
  
  if (nsims==0) doingNonNull<-FALSE
  else          doingNonNull<-TRUE
  
  if (doingNull && exploreResult$nullcount<exploreResult$count) {
    nsims<-exploreResult$count-exploreResult$nullcount
  }
  
  npoints<-explore$exploreNPoints
  
  minVal<-explore$minVal
  maxVal<-explore$maxVal
  xlog<-explore$xlog
  if (length(minVal)>1) vals<-minVal
  else {
    if (xlog) {
    minVal<-log10(minVal)
    maxVal<-log10(maxVal)
  }
  switch (explore$exploreType,
          "IVType"={vals<-c("Interval","Ord7","Ord4","Cat2","Cat3")},
          "DVType"={vals<-c("Interval","Ord7","Ord4","Cat2")},
          "IVIV2Type"={vals<-c("IntInt","Cat2Int","Cat3Int","IntCat","Cat2Cat","Cat3Cat")},
          "IVDVType"={vals<-c("IntInt","Ord7Int","Cat2Int","Cat3Int","IntOrd","Ord7Ord","Cat2Ord","Cat3Ord","IntCat","Ord7Cat","Cat2Cat","Cat3Cat")},
          "IVcats"={vals<-minVal:maxVal},
          "IVlevels"={vals<-minVal:maxVal},
          "IVprop"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVskew"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVkurtosis"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "DVcats"={vals<-minVal:maxVal},
          "DVlevels"={vals<-minVal:maxVal},
          "DVprop"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "DVskew"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "DVkurtosis"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "rSD"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "rIV"={
            if (is.null(IV2)) vals<-seq(minVal,maxVal,length.out=npoints)
            else {
              b<-2*effect$rIV2*effect$rIVIV2
              c<-effect$rIV2^2+effect$rIVIV2DV^2-max_r
              r1<- max(minVal,(-b-sqrt(b^2-4*c))/2)
              r2<-min(maxVal,(-b+sqrt(b^2-4*c))/2)
              vals<-seq(r1,r2,length.out=npoints)
            }
          },
          "rIV2"={
            b<-2*effect$rIV*effect$rIVIV2
            c<-effect$rIV^2+effect$rIVIV2DV^2-max_r
            r1<- max(minVal,(-b-sqrt(b^2-4*c))/2)
            r2<-min(maxVal,(-b+sqrt(b^2-4*c))/2)
            vals<-seq(r1,r2,length.out=npoints)
          },
          "rIVIV2"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
            maxCov<-abs((max_r-effect$rIV^2-effect$rIV2^2-effect$rIVIV2DV^2)/(2*effect$rIV*effect$rIV2))
            maxCov<-min(maxCov,max_r)
            vals<-seq(-maxCov,maxCov,length.out=npoints)
          },
          "rIVIV2DV"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "sourceBias"={vals<-seq(0,1,length.out=npoints)},
          
          "PDF"={vals<-c("Single","Double","Uniform","Gauss","Exp","Gamma","GenExp")},
          "mean(R+)"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "no"={vals<-seq(10,250,length.out=npoints)},
          "p(R+)"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "n"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "NoSplits"={design$sNBudget<-design$sN
                     vals<-seq(minVal,maxVal,length.out=npoints)},
          "Method"={vals<-c("Random","Limited","Cluster","Snowball","Convenience")},
          "ClusterRad"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Usage"={vals<-c("Between","Within")},
          "WithinCorr"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "SampleSD"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "PoorSamplingAmount"=vals<-{seq(minVal,maxVal,length.out=npoints)},
          "Dependence"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Outliers"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "NonResponse"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Heteroscedasticity"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVRange"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVRangeC"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVRangeE"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "DVRange"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Cheating"={vals<-c("None","Grow","Prune","Replace")},
          "CheatingAmount"={vals<-seq(minVal*design$sN,maxVal*design$sN,length.out=npoints)},
          "Alpha"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "minRp"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Transform"={vals<-c("None","Log","Exp")},
          "InteractionOn"={vals<-c(FALSE,TRUE)},
          "EqualVar"={vals<-c(FALSE,TRUE)},
          
          "Keep"={vals<-c("Cautious", "MetaAnalysis", "LargeN")},
          "Power"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Repeats" ={ vals<-minVal:maxVal },
          
          "NoStudies"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "MetaType"={vals<-c("FF","FT","TF","TT")}
  )
  if (xlog) vals<-10^vals
  }
  if (is.element(explore$exploreType,c("IVlevels","DVlevels","n","NoStudies"))) vals<-round(vals)
  
  exploreResult$vals<-vals
  exploreResult$explore<-explore
  
  result<-resetExploreResult(nsims,length(vals),exploreResult$result)
  
  if (doingNull) {
    nullresult<-resetExploreResult(nsims,length(vals),exploreResult$nullresult)
  } else nullresult<-NULL
  
  if (doingNull && exploreResult$nullcount<exploreResult$count)
    nsims<-min(exploreResult$count,exploreResult$nullcount)+nsims
  else   nsims<-exploreResult$count+nsims
  
  time.at.start<-Sys.time()
  while (((doingNonNull && exploreResult$count<nsims) || (doingNull && exploreResult$nullcount<nsims)) && (Sys.time()-time.at.start)<braw.env$timeLimit){
    if (!autoShow) ns<-nsims
    else {
      if (exploreResult$count==0) ns<-1
      else                        ns<-10^floor(log10(exploreResult$count))
    }
    if (braw.env$timeLimit<100) ns<-1
    ns<-min(ns,100)
    if (exploreResult$count+ns>nsims) ns<-nsims-exploreResult$count
    for (ni in 1:ns) {
      for (vi in 1:length(vals)){
        
        switch (explore$exploreType,
                "IVType"={
                  switch (vals[vi],
                          "Cat2"={
                            IV$type<-"Categorical"
                            IV$ncats<-2
                            IV$cases<-c("C1","C2")
                            IV$proportions<-c(1,1)
                          },
                          "Cat3"={
                            IV$type<-"Categorical"
                            IV$ncats<-3
                            IV$cases<-c("C1","C2","C3")
                            IV$proportions<-c(1,1,1)
                          },
                          "Ord7"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-7
                          },
                          "Ord4"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-4
                          },
                          "Interval"={IV$type<-"Interval"}
                  )
                },
                "DVType"={
                  switch (vals[vi],
                          "Cat2"={
                            DV$type<-"Categorical"
                            DV$ncats<-2
                            DV$cases<-c("E1","E2")
                            DV$proportions<-c(1,1)
                          },
                          # "Cat3"={
                          #   DV$type<-"Categorical"
                          #   DV$ncats<-3
                          #   DV$cases<-c("D1","D2","D3")
                          # },
                          "Ord7"={
                            DV$type<-"Ordinal"
                            DV$nlevs<-7
                          },
                          "Ord4"={
                            DV$type<-"Ordinal"
                            DV$nlevs<-4
                          },
                          "Interval"={DV$type<-"Interval"}
                  )
                },
                "IVDVType"={
                  switch (vals[vi],
                          "IntInt"={
                            IV$type<-"Interval"
                            DV$type<-"Interval"
                          },
                          "Ord7Int"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-7
                            DV$type<-"Interval"
                          },
                          "Ord4Int"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-4
                            DV$type<-"Interval"
                          },
                          "Cat2Int"={
                            IV$type<-"Categorical"
                            IV$ncats<-2
                            IV$cases<-c("C1","C2")
                            IV$proportions<-c(1,1)
                            DV$type<-"Interval"
                          },
                          "Cat3Int"={
                            IV$type<-"Categorical"
                            IV$ncats<-3
                            IV$cases<-c("C1","C2","C3")
                            IV$proportions<-c(1,1,1)
                            DV$type<-"Interval"
                          },
                          "IntOrd"={
                            IV$type<-"Interval"
                            DV$type<-"Ordinal"
                            DV$nlevs<-7
                          },
                          "Ord7Ord"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-7
                            DV$type<-"Ordinal"
                            DV$nlevs<-7
                          },
                          "Ord4Ord"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-4
                            DV$type<-"Ordinal"
                            DV$nlevs<-7
                          },
                          "Cat2Ord"={
                            IV$type<-"Categorical"
                            IV$ncats<-2
                            IV$cases<-c("C1","C2")
                            IV$proportions<-c(1,1)
                            DV$type<-"Ordinal"
                            DV$nlevs<-7
                          },
                          "Cat3Ord"={
                            IV$type<-"Categorical"
                            IV$ncats<-3
                            IV$cases<-c("C1","C2","C3")
                            IV$proportions<-c(1,1,1)
                            DV$type<-"Ordinal"
                            DV$nlevs<-7
                          },
                          "IntCat"={
                            IV$type<-"Interval"
                            DV$type<-"Categorical"
                            DV$ncats<-2
                            DV$cases<-c("E1","E2")
                            DV$proportions<-c(1,1)
                          },
                          "Ord7Cat"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-7
                            DV$type<-"Categorical"
                            DV$ncats<-2
                            DV$cases<-c("E1","E2")
                            DV$proportions<-c(1,1)
                          },
                          "Ord4Cat"={
                            IV$type<-"Ordinal"
                            IV$nlevs<-4
                            DV$type<-"Categorical"
                            DV$ncats<-2
                            DV$cases<-c("E1","E2")
                            DV$proportions<-c(1,1)
                          },
                          "Cat2Cat"={
                            IV$type<-"Categorical"
                            IV$ncats<-2
                            IV$cases<-c("C1","C2")
                            IV$proportions<-c(1,1)
                            DV$type<-"Categorical"
                            DV$ncats<-2
                            DV$cases<-c("E1","E2")
                            DV$proportions<-c(1,1)
                          },
                          "Cat3Cat"={
                            IV$type<-"Categorical"
                            IV$ncats<-3
                            IV$cases<-c("C1","C2","C3")
                            IV$proportions<-c(1,1,1)
                            DV$type<-"Categorical"
                            DV$ncats<-2
                            DV$cases<-c("E1","E2")
                            DV$proportions<-c(1,1)
                          }
                  )
                },
                "IVIV2Type"={
                  switch (vals[vi],
                          "IntInt"={
                            IV$type<-"Interval"
                            IV2$type<-"Interval"
                          },
                          "Cat2Int"={
                            IV$type<-"Categorical"
                            IV$ncats<-2
                            IV$cases<-c("C1","C2")
                            IV2$type<-"Interval"
                          },
                          "Cat3Int"={
                            IV$type<-"Categorical"
                            IV$ncats<-3
                            IV$cases<-c("C1","C2","C3")
                            IV2$type<-"Interval"
                          },
                          "IntCat"={
                            IV$type<-"Interval"
                            IV2$type<-"Categorical"
                            IV2$ncats<-2
                            IV2$cases<-c("D1","D2")
                          },
                          "Cat2Cat"={
                            IV$type<-"Categorical"
                            IV$ncats<-2
                            IV$cases<-c("C1","C2")
                            IV2$type<-"Categorical"
                            IV2$ncats<-2
                            IV2$cases<-c("D1","D2")
                          },
                          "Cat3Cat"={
                            IV$type<-"Categorical"
                            IV$ncats<-3
                            IV$cases<-c("C1","C2","C3")
                            IV2$type<-"Categorical"
                            IV2$ncats<-2
                            IV2$cases<-c("D1","D2")
                          }
                  )
                },
                "IVprop"={
                  IV$type<-"Categorical"
                  IV$proportions<-c(vals[vi],1)
                },
                "IVskew"={
                  IV$type<-"Interval"
                  IV$skew<-vals[vi]
                },
                "IVkurtosis"={
                  IV$type<-"Interval"
                  IV$kurtosis<-10^vals[vi]
                },
                "IVcats"={
                  IV$type<-"Categorical"
                  IV$ncats<-vals[i]
                  IV$cases<-format(1:IV$ncats)
                },
                "DVprop"={
                  DV$type<-"Categorical"
                  DV$proportions<-c(vals[vi],1)
                },
                "DVlevels"={
                  DV$type<-"Ordinal"
                  DV$nlevs<-vals[vi]
                  DV$median<-(DV$nlevs+1)/2
                  DV$iqr<-(DV$nlevs-1)/2
                },
                "DVcats"={
                  DV$type<-"Categorical"
                  DV$ncats<-vals[vi]
                },
                "DVskew"={
                  DV$type<-"Interval"
                  DV$skew<-vals[vi]
                },
                "DVkurtosis"={
                  DV$type<-"Interval"
                  DV$kurtosis<-10^vals[vi]
                },
                "rSD"={
                  effect$rSD<-vals[vi]
                },
                "rIV"={
                  if (effect$world$On) {
                    effect$world$PDFk<-vals[vi]
                  } else {
                    effect$rIV<-vals[vi]
                  }
                },
                "rIV2"={effect$rIV2<-vals[vi]},
                "rIVIV2"={effect$rIVIV2<-vals[vi]},
                "rIVIV2DV"={effect$rIVIV2DV<-vals[vi]},
                
                "PDF"={
                  effect$world$On<-TRUE
                  effect$world$PDF<-vals[vi]
                },
                "mean(R+)"={
                  effect$world$On<-TRUE
                  effect$world$PDFk<-vals[vi]
                },
                "no"={
                  effect$world$On<-TRUE
                  effect$world$PDFk<-1/sqrt(vals[vi]-3)
                },
                "p(R+)"={
                  effect$world$On<-TRUE
                  effect$world$pRPlus<-vals[vi]
                  # metaAnalysis$modelNulls<-TRUE
                },
                
                "Heteroscedasticity"={effect$Heteroscedasticity<-vals[vi]},
                "n"={design$sN<-round(vals[vi])},
                "NoSplits"={design$sN<-round(design$sNBudget/vals[vi])
                      design$sNReps<-vals[vi]
                      },
                "Method"={design$sMethod<-makeSampling(vals[vi])},
                "ClusterRad"={design$sMethod$Cluster_rad<-vals[vi]},
                "Usage"={ switch(vals[vi],
                                 "Between"={
                                   design$sIV1Use<-"Between"
                                   originalN<-design$sN
                                   design$sN<-originalN
                                 },
                                 "Between2"={
                                   design$sIV1Use<-"Between"
                                   design$sN<-originalN*2
                                 },
                                 "Within0"={
                                   design$sIV1Use<-"Within"
                                   design$sWithinCor<-0
                                   design$sN<-originalN
                                 },
                                 "Within"={
                                   design$sIV1Use<-"Within"
                                   design$sWithinCor<-0.5
                                   design$sN<-originalN
                                 }
                )
                },
                "WithinCorr"={design$sWithinCor<-vals[vi]},
                "SampleSD"={
                  design$sNRand<-TRUE
                  design$sNRandSD<-vals[vi]
                },
                "PoorSamplingAmount"={design$sMethodSeverity<-vals[vi]},
                "Dependence"={design$sDependence<-vals[vi]},
                "Outliers"={design$sOutliers<-vals[vi]},
                "NonResponse"={design$sNonResponse<-vals[vi]},
                "IVRange"={
                  design$sIVRangeOn<-TRUE
                  design$sIVRange<-vals[vi]*c(-1,1)
                },
                "IVRangeC"={
                  design$sIVRangeOn<-TRUE
                  design$sIVRange<-vals[vi]*c(-1,1)
                },
                "IVRangeE"={
                  design$sIVRangeOn<-TRUE
                  design$sIVRange<-c(vals[vi],4)
                },
                "DVRange"={
                  design$sIVRangeOn<-TRUE
                  design$sDVRange<-vals[vi]*c(-1,1)
                },
                "Cheating"={
                  design$sCheating<-vals[vi]
                },
                "CheatingAmount"={
                  design$sCheatingAttempts<-vals[vi]
                },
                "Alpha"={
                  evidence$alphaSig<-vals[vi]
                },
                "minRp"={
                  evidence$minRp<-vals[vi]
                },
                "EqualVar"={
                  evidence$Welch<-!vals[vi]
                },
                "Transform"={evidence$Transform<-vals[vi]},
                "InteractionOn"={evidence$AnalysisTerms[3]<-vals[vi]},
                
                "Keep"={
                  design$Replication$Keep<-vals[vi]
                },
                "Power"={
                  design$Replication$Power<-vals[vi]
                },
                "Repeats"={
                  design$Replication$Repeats<-vals[vi]
                },
                
                "NoStudies"={
                  metaAnalysis$nstudies<-vals[vi]
                  doingMetaAnalysis<-TRUE
                },
                "sourceBias"={
                  metaAnalysis$sourceBias<-vals[vi]
                  doingMetaAnalysis<-TRUE
                },
                "MetaType"={
                  switch(vals[vi],
                         "FF"={metaAnalysis$modelNulls<-FALSE
                         metaAnalysis$analyseBias<-FALSE},
                         "FT"={metaAnalysis$modelNulls<-TRUE
                         metaAnalysis$analyseBias<-FALSE},
                         "TF"={metaAnalysis$modelNulls<-FALSE
                         metaAnalysis$analyseBias<-TRUE},
                         "TT"={metaAnalysis$modelNulls<-TRUE
                         metaAnalysis$analyseBias<-TRUE},
                  )
                  doingMetaAnalysis<-TRUE
                }
        )
        hypothesis$IV<-IV
        hypothesis$IV2<-IV2
        hypothesis$DV<-DV
        hypothesis$effect<-effect
        if (doingNonNull)      ri<-exploreResult$count+1
        else                   ri<-exploreResult$nullcount+1
        
        if (doingMetaAnalysis) {
          res<-doMetaAnalysis(NULL,metaAnalysis,hypothesis=hypothesis,design=design,evidence=evidence)
          result<-storeExploreResult(result,res,ri,vi)
        } else {
          if (doingNonNull) {
            res<-multipleAnalysis(design$sNReps,hypothesis,design,evidence)
            res<-summariseResult(res)
            result<-storeExploreResult(result,res,ri,vi)
          }
          
          if (doingNull) {
            nullhypothesis<-hypothesis
            nullhypothesis$effect$rIV<-0
            res_null<-multipleAnalysis(1,nullhypothesis,design,evidence)
            res_null<-summariseResult(res_null)
            nullresult<-storeExploreResult(nullresult,res_null,ri,vi)
          }
        }
      }
      exploreResult$doingMetaAnalysis<-doingMetaAnalysis
      if (doingNull) {
        exploreResult$nullcount<-exploreResult$nullcount+1
        exploreResult$nullresult<-nullresult
      } else {
        exploreResult$result<-result
        exploreResult$count<-exploreResult$count+1
      }
      setBrawRes("explore",exploreResult)
    }
    if (autoShow) print(showExplore(exploreResult,showType=showType))
  }
  
  return(exploreResult)
}
