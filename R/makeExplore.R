abind<-function(a,b) array(c(a, b), dim = c(dim(a)[1], dim(a)[2], dim(a)[3]+dim(b)[3]))


resetExploreResult<-function(n_sims,n_vals,oldResult=NULL) {
  
  if (n_sims>0) {
    b<-array(NA,c(n_sims,n_vals))
    bm<-array(NA,c(n_sims,n_vals,3))
  } else {
    b<-NULL
    bm<-NULL
  }
  
  result<-list(rval=b,pval=b,rpval=b,raval=b,roval=b,poval=b,nval=b,df1=b,
               rIV1=b,rIVIV2DV=b,pIV2=b,pIVIV2DV=b,
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
  result$raval[ri,vi]<-res$raIV
  result$roval[ri,vi]<-res$roIV
  result$poval[ri,vi]<-res$poIV
  result$nval[ri,vi]<-res$nval
  result$df1[ri,vi]<-res$df1
  
  if (!is.null(res$rIV2)){
    result$rIV2[ri,vi]<-res$rIV2
    result$pIV2[ri,vi]<-res$pIV2
    result$rIVIV2DV[ri,vi]<-res$rIVIV2DV
    result$pIVIV2DV[ri,vi]<-res$rIVIV2DV
    
    result$r$direct[ri,vi,]<-res$r$direct
    result$r$unique[ri,vi,]<-res$r$unique
    result$r$total[ri,vi,]<-res$r$total

    result$p$direct[ri,vi,]<-res$p$direct
    result$p$unique[ri,vi,]<-res$p$unique
    result$p$total[ri,vi,]<-res$p$total
  }
  return(result)
}

mergeExploreResult<-function(res1,res2) {
  result$rval<-rbind(res1$rval,res2$rval)
  result$pval<-rbind(res1$pval,res2$pval)
  result$rpval<-rbind(res1$rpval,res2$rpval)
  result$raval<-rbind(res1$raval,res2$raval)
  result$roval<-rbind(res1$roval,res2$roval)
  result$poval<-rbind(res1$poval,res2$poval)
  result$nval<-rbind(res1$nval,res2$nval)
  result$df1<-rbind(res1$df1,res2$df1)
  # if (!is.null(res1$r)) {
    result$r$direct<-abind(res1$r$direct,res2$r$direct)
    result$r$unique<-abind(res1$r$unique,res2$r$unique)
    result$r$total<-abind(res1$r$total,res2$r$total)

    result$p$direct<-abind(res1$p$direct,res2$p$direct)
    result$p$unique<-abind(res1$p$unique,res2$p$unique)
    result$p$total<-abind(res1$p$total,res2$p$total)
  # }
  return(result)
}

#' make multiple samples whilst varying a parameter
#' 
#' @param exploreType "n","rIV","Heteroscedasticity","rIV2","rIVIV2","rIVIV2DV" \cr
#'                     "Method","Usage","WithinCorr","ClusterRad","SampleGamma" \cr
#'                     "Dependence","Outliers","IVRange","DVRange" \cr
#'                     "Cheating","CheatingAmount" \cr
#'                     "Alpha","Transform" \cr
#'                     "Power","SigOnly","Repeats" \cr
#' @returns exploreResult object
#' @seealso showExplore() 
#' @seealso reportExplore()
#' @examples
#' exploreResult<-makeExplore(nsims=10,exploreResult=NULL,exploreType="n",exploreNPoints=13,
#'                              min_n=10,max_n=250,max_r=0.9,max_anom=1,
#'                              xlog=FALSE,xabs=FALSE,mx_log=FALSE,
#'                              hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
#'                              doingNull=FALSE,autoShow=FALSE,showType="Basic")
#' @export
makeExplore<-function(nsim=10,exploreResult=NULL,exploreType="n",exploreNPoints=13,
                      min_n=10,max_n=250,max_r=0.9,max_anom=1,
                      xlog=FALSE,xabs=FALSE,mx_log=FALSE,
                      hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
                      adoingNull=FALSE,utoShow=FALSE,showType="r"
) {
  explore<-list(exploreType=exploreType,
                exploreNPoints=exploreNPoints,
                min_n=min_n,max_n=max_n,max_r=max_r,max_anom=max_anom,
                xlog=xlog,xabs=xabs,
                mx_log=mx_log,
                hypothesis=hypothesis,
                design=design,
                evidence=evidence
  )

  if (is.null(exploreResult)) {
    exploreResult<-list(count=0,
                        result=NULL,
                        nullcount=0,
                        nullresult=NULL,
                        vals=NA,
                        explore=explore
    )
  }
  
  exploreResult <- runExplore(nsim=nsim,exploreResult,doingNull=doingNull,
                              autoShow=autoShow,showType=showType)
  return(exploreResult)
}

runExplore <- function(nsim,exploreResult=NULL,doingNull=FALSE,
                       autoShow=FALSE,showType="r"){
  
  explore<-exploreResult$explore
  hypothesis<-explore$hypothesis
  design<-explore$design
  evidence<-explore$evidence
  
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  
  oldAlpha<-braw.env$alphaSig
  on.exit(braw.env$alphaSig<-oldAlpha)
  
  if (hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp>0) 
    doingNull<-FALSE
  
  npoints<-explore$exploreNPoints
  min_n<-explore$min_n
  max_n<-explore$max_n
  max_r<-explore$max_r
  if (braw.env$RZ=="z") {max_es<-max_es*braw.env$z_range}
  max_anom<-explore$max_anom
  kurtRange<-10^5
  
  xlog<-explore$xlog
  if (explore$xabs) {vals<-seq(0,1,length.out=npoints)}
  else              {vals<-seq(-1,1,length.out=npoints)}
  
  metaExplore<-is.element(explore$exploreType,c("NoStudies","sig_only"))
  switch (explore$exploreType,
          "IVType"={vals<-c("Interval","Ord7","Ord4","Cat2","Cat3")},
          "DVType"={vals<-c("Interval","Ord7","Ord4","Cat2")},
          "IVIV2Type"={vals<-c("IntInt","Cat2Int","Cat3Int","IntCat","Cat2Cat","Cat3Cat")},
          "IVDVType"={vals<-c("IntInt","Ord7Int","Cat2Int","Cat3Int","IntOrd","Ord7Ord","Cat2Ord","Cat3Ord","IntCat","Ord7Cat","Cat2Cat","Cat3Cat")},
          "IVcats"={vals<-2:7},
          "IVlevels"={vals<-2:10},
          "IVprop"={vals<-seq(0.2,1,length.out=npoints)},
          "IVskew"={vals<-vals},
          "IVkurtosis"={vals<-seq(0,log10(kurtRange),length.out=npoints)},
          "DVcats"={vals<-2:7},
          "DVlevels"={vals<-2:10},
          "DVprop"={vals<-seq(0.2,1,length.out=npoints)},
          "DVskew"={vals<-vals},
          "DVkurtosis"={vals<-seq(0,log10(kurtRange),length.out=npoints)},
          "rIV"={
            vals<-vals*max_es
            if (braw.env$RZ=="z") vals<-tanh(vals)
          },
          "rIV2"={
            b<-2*effect$rIV*effect$rIVIV2
            c<-effect$rIV^2+effect$rIVIV2DV^2-max_r
            r1<- (-b-sqrt(b^2-4*c))/2
            r2<-(-b+sqrt(b^2-4*c))/2
            vals<-seq(r1,r2,length.out=npoints)
          },
          "rIVIV2"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
            maxCov<-abs((maxESrange-effect$rIV^2-effect$rIV2^2-effect$rIVIV2DV^2)/(2*effect$rIV*effect$rIV2))
            maxCov<-min(maxCov,max_r)
            vals<-seq(-maxCov,maxCov,length.out=npoints)
          },
          "rIVIV2DV"={
            vals<-vals*max_es
          },
          
          "PDF"={vals<-c("Single","Double","Uniform","Gauss","Exp",">","<")},
          "k"={vals<-10^seq(-1,-0.1,length.out=npoints)},
          "pNull"={vals<-seq(0,1,length.out=npoints)},
          
          "n"={
            if (xlog){
              vals<-round(10^seq(log10(min_n),log10(max_n),length.out=npoints))
            }else{
              vals<-round(seq(min_n,max_n,length.out=npoints))
            }
          },
          "Method"={vals<-c("Random","Stratified","Cluster","Snowball","Convenience")},
          "ClusterRad"={vals<-seq(0,1,length.out=npoints)},
          "Usage"={vals<-c("Between","Between2","Within0","Within")},
          "WithinCorr"={vals<-seq(0,0.8,length.out=npoints)},
          "SampleGamma"={vals<-seq(1,10,length.out=npoints)},
          "Alpha"={
            if (xlog) {
              vals<-vals<-10^seq(log10(0.001),log10(0.5),length.out=npoints)
            } else {
              vals<-vals<-seq(0.001,0.1,length.out=npoints)
            }
          },
          "Dependence"={vals<-seq(0,max_anom,length.out=npoints)},
          "Outliers"={vals<-seq(0,max_anom,length.out=npoints)},
          "Heteroscedasticity"={vals<-seq(0,1,length.out=npoints)},
          "Transform"={vals<-c("None","Log","Exp")},
          "IVRange"={vals<-seq(3,0.5,length.out=npoints)},
          "DVRange"={vals<-seq(3,0.5,length.out=npoints)},
          "Cheating"={vals<-c("None","Grow","Prune","Replace","Retry","Add")},
          "CheatingAmount"={
            if (xlog){
              vals<-round(10^seq(log10(1),log10(max_n),length.out=npoints))
            }else{
              if ((max_n+1)<npoints) vals<-0:max_n
              else vals<-round(seq(0,max_n,length.out=npoints))
            }
          },
          
          "SigOnly"={vals<-c(FALSE,TRUE)},
          "Power"={vals<-seq(0.1,0.9,length.out=npoints)},
          "Repeats" ={
            if (design$ReplKeep=="median") vals<-seq(0,explore$Explore_nrRange,by=2)
            else vals<-seq(0,explore$Explore_nrRange)
          },
          
          "NoStudies"={
            if (explore$Explore_Mxlog){
              vals<-round(10^seq(log10(min_n),log10(explore$Explore_metaRange),length.out=npoints))
            }else{
              vals<-round(seq(min_n,explore$Explore_metaRange,length.out=npoints))
            }
          },
          "sig_only"={vals<-c(FALSE,TRUE)}
  )
  
  n_sims<-nsim
  exploreResult$vals<-vals
  exploreResult$explore<-explore
  
  result<-resetExploreResult(n_sims,length(vals),exploreResult$result)
  
  if (doingNull) {
    nullhypothesis<-hypothesis
    nullhypothesis$effect$rIV<-0
    nullresult<-resetExploreResult(n_sims,length(vals),exploreResult$nullresult)
  } else nullresult<-NULL
  n_sims<-exploreResult$count+n_sims
  
  while (exploreResult$count<n_sims){
    if (!autoShow) ns<-n_sims
    else {
      if (exploreResult$count==0) ns<-1
      else                        ns<-10^floor(log10(exploreResult$count))
    }
    ns<-min(ns,100)
    if (exploreResult$count+ns>n_sims) ns<-n_sims-exploreResult$count
    for (ni in 1:ns) {
      ri<-exploreResult$count+ni
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
                "rIV"={effect$rIV<-vals[vi]},
                "rIV2"={effect$rIV2<-vals[vi]},
                "rIVIV2"={effect$rIVIV2<-vals[vi]},
                "rIVIV2DV"={effect$rIVIV2DV<-vals[vi]},
                
                "PDF"={
                  effect$world$worldOn<-TRUE
                  effect$world$populationPDF<-vals[vi]
                },
                "k"={
                  effect$world$worldOn<-TRUE
                  effect$world$populationPDFk<-vals[vi]
                },
                "pNull"={
                  effect$world$worldOn<-TRUE
                  effect$world$populationNullp<-vals[vi]
                  metaAnalysis$meta_nullAnal<-TRUE
                },
                
                "Heteroscedasticity"={effect$Heteroscedasticity<-vals[vi]},
                "Transform"={evidence$Transform<-vals[vi]},
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
                "Alpha"={
                  braw.env$alphaSig<-vals[vi]
                },
                "Dependence"={design$sDependence<-vals[vi]},
                "Outliers"={design$sOutliers<-vals[vi]},
                "IVRange"={
                  design$sRangeOn<-TRUE
                  design$sIVRange<-vals[vi]*c(-1,1)
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
                
                "SigOnly"={
                  design$ReplSigOnly<-vals[vi]
                },
                "Power"={
                  design$ReplPower<-vals[vi]
                },
                "Repeats"={
                  design$ReplRepeats<-vals[vi]
                },
                
                "NoStudies"={
                  metaAnalysis$nstudies<-vals[vi]
                },
                "sig_only"={
                  metaAnalysis$sig_only<-vals[vi]
                  metaAnalysis$meta_psigAnal<-vals[vi]
                }
        )
        hypothesis$IV<-IV
        hypothesis$IV2<-IV2
        hypothesis$DV<-DV
        hypothesis$effect<-effect
        
        if (metaExplore) {
          res<-multipleAnalysis(metaAnalysis$nstudies,hypothesis,design,evidence,metaResult$result,sigOnly=metaAnalysis$sig_only)
          metaResult$result<-res
          metaResult<-runMetaAnalysis(metaAnalysis,metaResult)
          
          result$rval[ri,vi]<-metaResult$bestS
          result$S[ri,vi]<-metaResult$bestS
          result$k[ri,vi]<-metaResult$bestK
          result$pnull[ri,vi]<-metaResult$bestNull
          result$dist[ri,vi]<-metaResult$bestDist
        } else {
          res<-multipleAnalysis(1,hypothesis,design,evidence)
          result<-storeExploreResult(result,res,ri,vi)
          
          if (doingNull) {
            res_null<-multipleAnalysis(1,nullhypothesis,design,evidence)
            nullresult<-storeExploreResult(nullresult,res_null,ri,vi)
          }
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

  exploreResult
}
