
makeExplore<-function(nsim,type="SampleSize",hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
                      Explore_npoints=13,exploreResult=NULL,doingNull=FALSE,
                      autoShow=FALSE,Explore_show="EffectSize",
                      min_n=10,max_n=250,max_r=0.9,max_anom=1,
                      xlog=FALSE,xabs=FALSE,
                      mx_log=FALSE
) {
  explore<-list(hypothesis=hypothesis,
                design=design,
                evidence=evidence,
                type=type,
                Explore_npoints=Explore_npoints,
                min_n=min_n,max_n=max_n,max_r=max_r,max_anom=max_anom,
                xlog=xlog,xabs=xabs,
                mx_log=mx_log
  )

  if (is.null(exploreResult)) {
    exploreResult<-list(result=NULL,
                        nullresult=NULL,
                        count=0,
                        nullcount=0,
                        vals=NA,
                        explore=explore
    )
  }
  
  exploreResult <- runExplore(nsim=nsim,exploreResult,doingNull=doingNull,
                              autoShow=autoShow,Explore_show=Explore_show)
  return(exploreResult)
}

runExplore <- function(nsim,exploreResult=NULL,doingNull=FALSE,
                       autoShow=FALSE,Explore_show="EffectSize"){
  
  explore<-exploreResult$explore
  hypothesis<-explore$hypothesis
  design<-explore$design
  evidence<-explore$evidence
  
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  
  oldAlpha<-BrawOpts$alphaSig
  
  if (hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp>0) 
    doingNull<-FALSE
  
  npoints<-explore$Explore_npoints
  min_n<-explore$min_n
  max_n<-explore$max_n
  max_r<-explore$max_r
  if (RZ=="z") {max_es<-max_es*z_range}
  max_anom<-explore$max_anom
  kurtRange<-10^5
  
  xlog<-explore$xlog
  if (explore$xabs) {vals<-seq(0,1,length.out=npoints)}
  else              {vals<-seq(-1,1,length.out=npoints)}
  
  metaExplore<-is.element(explore$type,c("NoStudies","sig_only"))
  switch (explore$type,
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
          "EffectSize"={
            vals<-vals*max_es
            if (RZ=="z") vals<-tanh(vals)
          },
          "EffectSize1"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+
            b<-2*effect$rIV2*effect$rIVIV2
            c<-effect$rIV2^2+effect$rIVIV2DV^2-max_r
            r1<- (-b-sqrt(b^2-4*c))/2
            r2<-(-b+sqrt(b^2-4*c))/2
            vals<-seq(r1,r2,length.out=npoints)
          },
          "EffectSize2"={
            b<-2*effect$rIV*effect$rIVIV2
            c<-effect$rIV^2+effect$rIVIV2DV^2-max_r
            r1<- (-b-sqrt(b^2-4*c))/2
            r2<-(-b+sqrt(b^2-4*c))/2
            vals<-seq(r1,r2,length.out=npoints)
          },
          "Covariation"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
            maxCov<-abs((maxESrange-effect$rIV^2-effect$rIV2^2-effect$rIVIV2DV^2)/(2*effect$rIV*effect$rIV2))
            maxCov<-min(maxCov,max_r)
            vals<-seq(-maxCov,maxCov,length.out=npoints)
          },
          "Interaction"={
            vals<-vals*max_es
          },
          
          "PDF"={vals<-c("Single","Double","Uniform","Gauss","Exp",">","<")},
          "k"={vals<-10^seq(-1,-0.1,length.out=npoints)},
          "pNull"={vals<-seq(0,1,length.out=npoints)},
          
          "SampleSize"={
            if (xlog){
              vals<-round(10^seq(log10(min_n),log10(max_n),length.out=npoints))
            }else{
              vals<-round(seq(min_n,max_n,length.out=npoints))
            }
          },
          "Method"={vals<-c("Random","Stratified","Cluster","Convenience","Snowball")},
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
          
          "SigOnly"={vals<-c("Yes","No")},
          "Power"={vals<-seq(0.1,0.9,length.out=npoints)},
          "Repeats" ={
            if (design$sReplKeep=="median") vals<-seq(0,explore$Explore_nrRange,by=2)
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
  
  b<-matrix(NA,nrow=n_sims,ncol=length(vals))
  result<-list(rval=b,rpval=b,pval=b,roval=b,poval=b,nval=b,df1=b,
               r1=list(direct=b,unique=b,total=b),
               r2=list(direct=b,unique=b,total=b),
               r3=list(direct=b,unique=b,total=b)
  )
  if (doingNull) {
    nullhypothesis<-hypothesis
    nullhypothesis$effect$rIV<-0
    nullresult<-result
  } else nullresult<-NULL
  
  if (!is.null(exploreResult)) {
    result$rval<-rbind(exploreResult$result$rval,result$rval)
    result$rpval<-rbind(exploreResult$result$rpval,result$rpval)
    result$pval<-rbind(exploreResult$result$pval,result$pval)
    result$roval<-rbind(exploreResult$result$roval,result$roval)
    result$poval<-rbind(exploreResult$result$poval,result$poval)
    result$nval<-rbind(exploreResult$result$nval,result$nval)
    result$df1<-rbind(exploreResult$result$df1,result$df1)
    
    nullresult$rval<-rbind(exploreResult$nullresult$rval,result$rval)
    nullresult$rpval<-rbind(exploreResult$nullresult$rpval,result$rpval)
    nullresult$pval<-rbind(exploreResult$nullresult$pval,result$pval)
    nullresult$roval<-rbind(exploreResult$nullresult$roval,result$roval)
    nullresult$poval<-rbind(exploreResult$nullresult$poval,result$poval)
    nullresult$nval<-rbind(exploreResult$nullresult$nval,result$nval)
    nullresult$df1<-rbind(exploreResult$nullresult$df1,result$df1)
    
    n_sims<-exploreResult$count+n_sims
  }

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
        
        switch (explore$type,
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
                "EffectSize"={effect$rIV<-vals[vi]},
                "EffectSize1"={effect$rIV<-vals[vi]},
                "EffectSize2"={effect$rIV2<-vals[vi]},
                "Covariation"={effect$rIVIV2<-vals[vi]},
                "Interaction"={effect$rIVIV2DV<-vals[vi]},
                
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
                "SampleSize"={design$sN<-round(vals[vi])},
                "Method"={design$sMethod<-vals[vi]},
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
                  BrawOpts$alphaSig<<-vals[vi]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
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
                  design$sCheatingAmount<-vals[vi]
                },
                
                "SigOnly"={
                  design$sReplSigOnly<-vals[vi]
                },
                "Power"={
                  design$sReplPower<-vals[vi]
                },
                "Repeats"={
                  design$sReplRepeats<-vals[vi]
                },
                
                "NoStudies"={
                  metaAnalysis$nstudies<-vals[vi]
                },
                "sig_only"={
                  metaAnalysis$sig_only<-vals[vi]
                  metaAnalysis$meta_psigAnal<-vals[vi]
                }
        )
        
        if (metaExplore) {
          res<-multipleAnalysis(metaAnalysis$nstudies,hypothesis,design,evidence,metaResult$result,sigOnly=metaAnalysis$sig_only)
          metaResult$result<-res
          metaResult<-runMetaAnalysis(metaAnalysis,metaResult)
          
          result$k[ri,vi]<-metaResult$bestK
          result$pnull[ri,vi]<-metaResult$bestNull
          result$S[ri,vi]<-metaResult$bestS
          result$dist[ri,vi]<-metaResult$bestDist
          result$rval[ri,vi]<-metaResult$bestS
        } else {
          res<-multipleAnalysis(1,hypothesis,design,evidence)
          
          result$rval[ri,vi]<-res$rIV
          result$rpval[ri,vi]<-res$rpIV
          result$pval[ri,vi]<-res$pIV
          result$roval[ri,vi]<-res$roIV
          result$poval[ri,vi]<-res$poIV
          result$nval[ri,vi]<-res$nval
          result$df1[ri,vi]<-res$df1
          
          if (!is.null(IV2)){
            result$r1$direct[ri,vi]<-res$r$direct[,1]
            result$r1$unique[ri,vi]<-res$r$unique[,1]
            result$r1$total[ri,vi]<-res$r$total[,1]
            
            result$r2$direct[ri,vi]<-res$r$direct[,2]
            result$r2$unique[ri,vi]<-res$r$unique[,2]
            result$r2$total[ri,vi]<-res$r$total[,2]
            
            result$r3$direct[ri,vi]<-res$r$direct[,3]
            result$r3$unique[ri,vi]<-res$r$unique[,3]
            result$r3$total[ri,vi]<-res$r$total[,3]
            
            result$p1$direct[ri,vi]<-res$p$direct[,1]
            result$p1$unique[ri,vi]<-res$p$unique[,1]
            result$p1$total[ri,vi]<-res$p$total[,1]
            
            result$p2$direct[ri,vi]<-res$p$direct[,2]
            result$p2$unique[ri,vi]<-res$p$unique[,2]
            result$p2$total[ri,vi]<-res$p$total[,2]
            
            result$p3$direct[ri,vi]<-res$p$direct[,3]
            result$p3$unique[ri,vi]<-res$p$unique[,3]
            result$p3$total[ri,vi]<-res$p$total[,3]
          }
          
          if (doingNull) {
            res_null<-multipleAnalysis(1,nullhypothesis,design,evidence)
            
            nullresult$rval[ri,vi]<-res_null$rIV
            nullresult$rpval[ri,vi]<-res_null$rpIV
            nullresult$pval[ri,vi]<-res_null$pIV
            nullresult$roval[ri,vi]<-res_null$roIV
            nullresult$poval[ri,vi]<-res_null$poIV
            nullresult$nval[ri,vi]<-res_null$nval
            nullresult$df1[ri,vi]<-res_null$df1
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
    if (autoShow) print(showExplore(exploreResult,Explore_show=Explore_show))
  }
  
  BrawOpts$alphaSig<<-oldAlpha
  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
  
  exploreResult
}
