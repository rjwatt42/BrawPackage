#' make multiple samples whilst varying a parameter
#' 
#' @param exploreType "rIV","Heteroscedasticity","rIV2","rIVIV2","rIVIV2DV" \cr
#'                    "pNull","Lambda" \cr
#'                    "n","Method","Usage","WithinCorr","ClusterRad","SampleGamma" \cr
#'                     "Dependence","Outliers","NonResponse","IVRange","IVRangeC","IVRangeE","DVRange" \cr
#'                     "Cheating","CheatingAmount" \cr
#'                     "Alpha","Transform","InteractionOn" \cr
#'                     "Power","Keep","Repeats" \cr
#' @returns explore object
#' @seealso doExplore() 
#' @seealso showExplore() 
#' @seealso reportExplore()
#' @examples
#' explore<-makeExplore(exploreType="n",exploreNPoints=13,
#'                              minVal=10,maxVal=250,xlog=FALSE)
#' @export
makeExplore<-function(exploreType="n",exploreNPoints=13,
                    minVal=NA,maxVal=NA,xlog=NA
) {
  if (exploreType=="alpha") exploreType<-"Alpha"
  explore<-list(exploreType=exploreType,
                exploreNPoints=exploreNPoints,
                minVal=minVal,maxVal=maxVal,xlog=xlog
  )
  range<-getExploreRange(explore)
  if (is.na(explore$minVal)) explore$minVal<-range$minVal
  if (is.na(explore$maxVal)) explore$maxVal<-range$maxVal
  if (is.na(explore$xlog)) explore$xlog<-range$logScale
  
  return(explore)
}

getExploreRange<-function(explore) {
  
  exploreType<-explore$exploreType
  if (is.element(exploreType,c("rIV","rIV2","rIVIV2","rIVIV2DV"))) exploreType<-"rs"
  if (is.element(exploreType,c("IVskew","DVskew","Heteroscedasticity","Dependence","Outliers","NonResponse"))) exploreType<-"anom"
  if (is.element(exploreType,c("IVRange","IVRangeC","DVRange"))) exploreType<-"anom1"
  if (is.element(exploreType,c("IVRangeE"))) exploreType<-"anom2"
  if (is.element(exploreType,c("IVkurtosis","DVkurtosis"))) exploreType<-"kurt"
  
  switch(exploreType,
         "n"=range<-list(minVal=10,maxVal=250,logScale=FALSE,np=13),
         "rs"=range<-list(minVal=-0.9,maxVal=0.9,logScale=FALSE,np=13),
         "anom"=range<-list(minVal=0,maxVal=1,logScale=FALSE,np=13),
         "anom1"=range<-list(minVal=0.1,maxVal=3,logScale=FALSE,np=13),
         "anom2"=range<-list(minVal=-3,maxVal=3,logScale=FALSE,np=13),
         "kurt"=range<-list(minVal=1.5,maxVal=10^5,logScale=TRUE,np=13),
         "IVprops"=range<-list(minVal=0.2,maxVal=0.8,logScale=FALSE,np=13),
         "DVprops"=range<-list(minVal=0.2,maxVal=0.8,logScale=FALSE,np=13),
         "IVlevels"=range<-list(minVal=3,maxVal=10,logScale=FALSE,np=8),
         "DVlevels"=range<-list(minVal=3,maxVal=10,logScale=FALSE,np=8),
         "IVcats"=range<-list(minVal=2,maxVal=6,logScale=FALSE,np=5),
         "DVcats"=range<-list(minVal=2,maxVal=6,logScale=FALSE,np=5),
         "WithinCorr"=range<-list(minVal=0,maxVal=1,logScale=FALSE,np=13),
         "Alpha"=range<-list(minVal=0.001,maxVal=0.5,logScale=TRUE,np=13),
         "Power"=range<-list(minVal=0.1,maxVal=0.9,logScale=FALSE,np=13),
         "Repeats"=range<-list(minVal=0,maxVal=8,logScale=FALSE,np=9),
         "pNull"=range<-list(minVal=0,maxVal=1,logScale=FALSE,np=13),
         "Lambda"=range<-list(minVal=0.1,maxVal=1,logScale=FALSE,np=13),
         "CheatingAmount"=range<-list(minVal=0, maxVal=0.8,logScale=FALSE,np=13),
         "ClusterRad"=range<-list(minVal=0, maxVal=1,logScale=FALSE,np=13),
         "SampleGamma"=range<-list(minVal=1, maxVal=10,logScale=FALSE,np=13),
         {range<-list(minVal=0,maxVal=1,logScale=FALSE,np=2)}
  )
  return(range)
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
               rIV2=b,rIVIV2DV=b,pIV2=b,pIVIV2DV=b,
               r=list(direct=bm,unique=bm,total=bm),
               p=list(direct=bm,unique=bm,total=bm)
  )
  if (!is.null(oldResult)) {
    result<-mergeExploreResult(oldResult,result)
  }
  return(result)
}
storeExploreResult<-function(result,res,ri,vi) {
  result$rval[ri,vi]<-res$rIV
  result$pval[ri,vi]<-res$pIV
  result$rpval[ri,vi]<-res$rpIV
  result$roval[ri,vi]<-res$roIV
  result$poval[ri,vi]<-res$poIV
  result$nval[ri,vi]<-res$nval
  result$df1[ri,vi]<-res$df1
  
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
    
  return(result)
}

#' make multiple samples whilst varying a parameter
#' 
#' @param exploreType "rIV","Heteroscedasticity","rIV2","rIVIV2","rIVIV2DV" \cr
#'                    "pNull","Lambda" \cr
#'                    "n","Method","Usage","WithinCorr","ClusterRad","SampleGamma" \cr
#'                     "Dependence","Outliers","NonResponse","IVRange","IVRangeC","IVRangeE","DVRange" \cr
#'                     "Cheating","CheatingAmount" \cr
#'                     "Alpha","Transform","InteractionOn" \cr
#'                     "Power","Keep","Repeats" \cr
#' @returns exploreResult object
#' @seealso showExplore() 
#' @seealso reportExplore()
#' @examples
#' exploreResult<-doExplore(nsims=10,exploreResult=NULL,explore=braw.def$explore,
#'                              doingNull=FALSE,autoShow=braw.env$autoShow,showType="Basic")
#' @export
doExplore<-function(nsims=10,exploreResult=NULL,explore=braw.def$explore,
                    hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,
                      doingNull=FALSE,autoShow=braw.env$autoShow,showType="rs"
) {
  autoShowLocal<-autoShow
  assign("autoShow",FALSE,braw.env)
  
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
                        evidence=evidence
    )
  }
  explore<-exploreResult$explore

  if (doingNull && !hypothesis$effect$world$worldOn) {
    hypothesisNull<-hypothesis
    hypothesisNull$effect$rIV<-0
    # catch up - make enough null results to match results
    if (exploreResult$nullcount<exploreResult$count) {
      ns<-exploreResult$count-exploreResult$nullcount
      exploreResult <- runExplore(nsims=ns,exploreResult,doingNull=TRUE,autoShow=FALSE)
      exploreResult$nullcount<-exploreResult$nullcount+ns
    }
  }
  
  exploreResult <- runExplore(nsims=nsims,exploreResult,doingNull=doingNull,
                              autoShow=autoShow,showType=showType)
  assign("autoShow",autoShow,braw.env)
  
  return(exploreResult)
}

runExplore <- function(nsims,exploreResult,doingNull=FALSE,
                       autoShow=braw.env$autoShow,showType="rs"){
  max_r<-0.9
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  design<-exploreResult$design
  evidence<-exploreResult$evidence
  
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  
  if (hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp>0) 
    doingNull<-FALSE
  
  if (nsims==0) doingNonNUll<-FALSE
  else          doingNonNUll<-TRUE
  
  if (doingNull && exploreResult$nullcount<exploreResult$count) {
    nsims<-exploreResult$count-exploreResult$nullcount
  }
  
  npoints<-explore$exploreNPoints

  minVal<-explore$minVal
  maxVal<-explore$maxVal
  xlog<-explore$xlog
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
          "rIV"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "rIV2"={
            b<-2*effect$rIV*effect$rIVIV2
            c<-effect$rIV^2+effect$rIVIV2DV^2-max_r
            r1<- (-b-sqrt(b^2-4*c))/2
            r2<-(-b+sqrt(b^2-4*c))/2
            vals<-seq(r1,r2,length.out=npoints)
          },
          "rIVIV2"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
            maxCov<-abs((max_r-effect$rIV^2-effect$rIV2^2-effect$rIVIV2DV^2)/(2*effect$rIV*effect$rIV2))
            maxCov<-min(maxCov,max_r)
            vals<-seq(-maxCov,maxCov,length.out=npoints)
          },
          "rIVIV2DV"={vals<-seq(minVal,maxVal,length.out=npoints)},
          
          "PDF"={vals<-c("Single","Double","Uniform","Gauss","Exp",">","<")},
          "Lambda"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "pNull"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "n"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Method"={vals<-c("Random","Stratified","Cluster","Snowball","Convenience")},
          "ClusterRad"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Usage"={vals<-c("Between","Within")},
          "WithinCorr"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "SampleGamma"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Dependence"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Outliers"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "NonResponse"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Heteroscedasticity"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVRange"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVRangeC"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "IVRangeE"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "DVRange"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Cheating"={vals<-c("None","Grow","Prune","Replace","Retry","Add")},
          "CheatingAmount"={vals<-seq(minVal*design$sN,maxVal*design$sN,length.out=npoints)},
          "Alpha"={vals<-vals<-seq(minVal,maxVal,length.out=npoints)},
          "Transform"={vals<-c("None","Log","Exp")},
          "InteractionOn"={vals<-c(FALSE,TRUE)},
          "EqualVar"={vals<-c(FALSE,TRUE)},
          
          "Keep"={vals<-c("cautious", "last", "largeN", "smallP", "median")},
          "Power"={vals<-seq(minVal,maxVal,length.out=npoints)},
          "Repeats" ={ vals<-minVal:maxVal }
  )
  if (substr(explore$exploreType,1,1)=="r" && braw.env$RZ=="z") vals<-tanh(vals)
  if (xlog) vals<-10^vals
  
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
  while (exploreResult$count<nsims && Sys.time()-time.at.start<braw.env$timeLimit){
    if (!autoShow) ns<-nsims
    else {
      if (exploreResult$count==0) ns<-1
      else                        ns<-10^floor(log10(exploreResult$count))
    }
    if (braw.env$timeLimit<100) ns<-1
    ns<-min(ns,100)
    if (exploreResult$count+ns>nsims) ns<-nsims-exploreResult$count
    for (ni in 1:ns) {
      if (doingNonNUll)      ri<-exploreResult$count+ni
      else                   ri<-exploreResult$nullcount+ni
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
                "rIV"={
                  if (effect$world$worldOn) {
                    effect$world$populationPDFk<-vals[vi]
                  } else {
                    effect$rIV<-vals[vi]
                  }
                  },
                "rIV2"={effect$rIV2<-vals[vi]},
                "rIVIV2"={effect$rIVIV2<-vals[vi]},
                "rIVIV2DV"={effect$rIVIV2DV<-vals[vi]},
                
                "PDF"={
                  effect$world$worldOn<-TRUE
                  effect$world$populationPDF<-vals[vi]
                },
                "Lambda"={
                  effect$world$worldOn<-TRUE
                  effect$world$populationPDFk<-vals[vi]
                },
                "pNull"={
                  effect$world$worldOn<-TRUE
                  effect$world$populationNullp<-vals[vi]
                  # metaAnalysis$includeNulls<-TRUE
                },
                
                "Heteroscedasticity"={effect$Heteroscedasticity<-vals[vi]},
                "n"={design$sN<-round(vals[vi])},
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
                "SampleGamma"={
                  design$sNRand<-TRUE
                  design$sNRandK<-vals[vi]
                },
                "Dependence"={design$sDependence<-vals[vi]},
                "Outliers"={design$sOutliers<-vals[vi]},
                "NonResponse"={design$sNonResponse<-vals[vi]},
                "IVRange"={
                  design$sRangeOn<-TRUE
                  design$sIVRange<-vals[vi]*c(-1,1)
                },
                "IVRangeC"={
                  design$sRangeOn<-TRUE
                  design$sIVRange<-vals[vi]*c(-1,1)
                },
                "IVRangeE"={
                  design$sRangeOn<-TRUE
                  design$sIVRange<-c(vals[vi],4)
                },
                "DVRange"={
                  design$sRangeOn<-TRUE
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
                "EqualVar"={
                  evidence$Welch<-!vals[vi]
                },
                "Transform"={evidence$Transform<-vals[vi]},
                "InteractionOn"={evidence$rInteractionOn<-vals[vi]},
                
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
                },
                "sigOnly"={
                  metaAnalysis$sigOnly<-vals[vi]
                }
        )
        hypothesis$IV<-IV
        hypothesis$IV2<-IV2
        hypothesis$DV<-DV
        hypothesis$effect<-effect
        
        if (doingNonNUll) {
          res<-multipleAnalysis(1,hypothesis,design,evidence)
          result<-storeExploreResult(result,res,ri,vi)
        }
        
        if (doingNull) {
          nullhypothesis<-hypothesis
          nullhypothesis$effect$rIV<-0
          res_null<-multipleAnalysis(1,nullhypothesis,design,evidence)
          nullresult<-storeExploreResult(nullresult,res_null,ri,vi)
        }
      }
    }
    exploreResult$count<-ri
    exploreResult$result<-result
    if (doingNull) {
    exploreResult$nullcount<-ri
    exploreResult$nullresult<-nullresult
    }
    if (autoShow) print(showExplore(exploreResult,showType=showType))
  }

  setBrawRes("explore",exploreResult)
  return(exploreResult)
}
