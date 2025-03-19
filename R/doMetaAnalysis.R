
# META-ANALYSIS
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    

#' @return metaResult object 
#' @examples
#' doMetaAnalysis<-function(metaSingle=NULL,metaAnalysis=makeMetaAnalysis(),
#'                          keepStudies=FALSE,shortHand=TRUE,
#'                          hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence)
#' @export
doMetaAnalysis<-function(metaSingle=braw.res$metaSingle,metaAnalysis=braw.def$metaAnalysis,
                         keepStudies=FALSE,shortHand=TRUE,
                         hypothesis=braw.def$hypothesis,design=NULL,evidence=braw.def$evidence
) {
  if (is.null(metaAnalysis)) metaAnalysis<-makeMetaAnalysis()
  if (is.null(design)) design<-getDesign("Psych")
  evidence$sigOnly<-metaAnalysis$sourceBias
  evidence$shortHand<-shortHand
  
  localHypothesis<-hypothesis
  if (hypothesis$effect$world$worldOn && is.element(metaAnalysis$analysisType,c("fixed","random")))
  {
    localHypothesis$effect$rIV<-getWorldEffect(1,localHypothesis$effect)
    localHypothesis$effect$world$worldOn<-FALSE
  }

  if (is.null(metaSingle) || !keepStudies)
    studies<-multipleAnalysis(metaAnalysis$nstudies,localHypothesis,design,evidence)
  else
    studies<-metaSingle$result
  metaSingle<-runMetaAnalysis(metaAnalysis,studies,hypothesis,NULL)
  
  metaSingle$hypothesis<-hypothesis
  metaSingle$design<-design
  setBrawRes("metaSingle",metaSingle)
  metaSingle
}

#' @return metaResult object 
#' @examples
#' doMetaMultiple<-function(nsims=100,metaMultiple=braw.res$metaMultiple,metaAnalysis=braw.def$metaAnalysis,
#'                          shortHand=TRUE,
#'                          hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence)
#' @export
doMetaMultiple<-function(nsims=100,metaMultiple=braw.res$metaMultiple,metaAnalysis=braw.def$metaAnalysis,
                         shortHand=TRUE,
                         hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence
) {
  if (is.null(metaAnalysis)) metaAnalysis<-makeMetaAnalysis()
  evidence$sigOnly<-metaAnalysis$sourceBias
  evidence$shortHand<-shortHand
  
  for (i in 1:nsims) {
    localHypothesis<-hypothesis
    if (hypothesis$effect$world$worldOn && is.element(metaAnalysis$analysisType,c("fixed","random")))
    {
      localHypothesis$effect$rIV<-getWorldEffect(1,localHypothesis$effect)
      localHypothesis$effect$world$worldOn<-FALSE
    }
    studies<-multipleAnalysis(metaAnalysis$nstudies,localHypothesis,design,evidence)
    metaMultiple<-runMetaAnalysis(metaAnalysis,studies,hypothesis,metaMultiple)
  }
  metaMultiple$hypothesis<-hypothesis
  metaMultiple$design<-design
  setBrawRes("metaMultiple",metaMultiple)
  metaMultiple
}

getTrimFill<-function(zs,ns,df1,dist,metaAnalysis,hypothesis) {
  sigs<-isSignificant(method="NHST",p=r2p(tanh(zs),ns),r=tanh(zs),n=ns)
  res<-tryCatch({
    switch(dist,
           "fixed"={
             q<-trimfill(zs,1/sqrt(ns-3),ma.common=TRUE,common=TRUE,random=FALSE)
             nFill<-q$k-length(zs)
             bias<-nFill/(nFill+sum(!sigs))
             Smax<-getLogLikelihood(zs,ns,df1,dist,q$TE.common,0,bias)
             list(param1Max=q$TE.common,param2Max=0,param3Max=bias,Smax=Smax,Svals=q$seTE)
           },
           "random"={
             q<-trimfill(zs,1/sqrt(ns-3),ma.common=FALSE,common=FALSE,random=TRUE)
             nFill<-q$k-length(zs)
             bias<-nFill/(nFill+sum(!sigs))
             Smax<-getLogLikelihood(zs,ns,df1,dist,q$TE.random,q$tau,bias)
             list(param1Max=q$TE.random,param2Max=q$tau,param3Max=bias,Smax=Smax,Svals=q$seTE)
           }
           )
  },
  error={
    list(param1Max=NA,param2Max=NA,param3Max=NA,Smax=NA,Svals=NA)
  },
  warning={},
  finally={}
  )
  if (is.infinite(res$param1Max)) res$param1Max<-NA
  if (is.infinite(res$param2Max)) res$param2Max<-NA
  if (is.infinite(res$param3Max)) res$param3Max<-NA
  return(res)
}

getMaxLikelihood<-function(zs,ns,df1,dist,metaAnalysis,hypothesis) {
  # param1 is kvals
  # param2 is normally nullvals
  
  np1points<-21
  np2points<-21
  np3points<-21
  
  niterations<-2
  # reInc1<-(np1points-1)/2/3
  # reInc2<-(np2points-1)/2/3
  reInc1<-2
  reInc2<-2
  reInc3<-2
  
  if (metaAnalysis$includeNulls) {
    param2<-seq(0,1,length.out=np2points)
  } else {
    param2<-0
  }
  if (dist=="Single") {
    param1<-seq(-1,1,length.out=np1points)
  } else {
    param1<-seq(0,1,length.out=np1points)
  }
  
  if (dist=="fixed") {
    param1<-seq(-1,1,length.out=np1points)
    param2<-0
  }
  if (dist=="random") {
    param1<-seq(-1,1,length.out=np1points)
    if (metaAnalysis$analysisVar=="sd") param2<-seq(0,0.5,length.out=np2points)^2
    else param2<-seq(-0.1,1,length.out=np2points)*(0.5^2)
  }
  if (metaAnalysis$analyseBias) param3<-seq(0,1,length.out=np2points)
  else param3<-0
  
  analyseBias<-metaAnalysis$analyseBias
  prior<-metaAnalysis$analysisPrior
  prior_z<-seq(min(param1),max(param1),length.out=101)
  zcrit<-atanh(p2r(braw.env$alphaSig,ns,1))
  priorVals<-0
  for (i in 1:length(ns)) {
    newDens<-pnorm(-zcrit[i],prior_z,1/sqrt(ns[i]-3))+(1-pnorm(zcrit[i],prior_z,1/sqrt(ns[i]-3)))
    priorVals<-priorVals+newDens
  }
  switch(metaAnalysis$analysisPrior,
         "none"={
           priorDens<-prior_z*0+1
           },
         "uniform"={
           priorDens<-rPopulationDist(tanh(prior_z),makeWorld(TRUE,"Uniform","r"))
           priorDens<-rdens2zdens(priorDens,tanh(prior_z))
           priorDens<-log(priorVals*priorDens)
         },
         "world"={
           priorDens<-rPopulationDist(tanh(prior_z),hypothesis$effect$world)
           priorDens<-rdens2zdens(priorDens,tanh(prior_z))
           priorDens<-log(priorVals*priorDens)
         }
  )
  
  llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,x[1],x[2],x[3])+approx(prior_z,priorDens,x[1])$y)}
  if(length(param3)==1)
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,x[1],x[2],0)+approx(prior_z,priorDens,x[1])$y)}
  if(length(param2)==1)
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,x[1],0,x[3])+approx(prior_z,priorDens,x[1])$y)}
  if(length(param2)==1 && length(param3)==1)
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,x[1],0,0)+approx(prior_z,priorDens,x[1])$y)}

  S<-array(0,c(length(param1),length(param2),length(param3)))
  for (re in 1:niterations) {
    for (p1 in 1:length(param1))
      for (p2 in 1:length(param2))
        for (p3 in 1:length(param3))
          S[p1,p2,p3]<-llfun(c(param1[p1],param2[p2],param3[p3]))

    Smax<- -min(S,na.rm=TRUE)
    use<-which(S==-Smax, arr.ind = TRUE)
    param1Max<-param1[use[1,1]]
    lb1<-param1[max(1,use[1,1]-reInc1)]
    ub1<-param1[min(length(param1),use[1,1]+reInc1)]
    param2Max<-param2[use[1,2]]
    lb2<-param2[max(1,use[1,2]-reInc2)]
    ub2<-param2[min(length(param2),use[1,2]+reInc2)]
    param3Max<-param3[use[1,3]]
    lb3<-param3[max(1,use[1,3]-reInc3)]
    ub3<-param3[min(length(param3),use[1,3]+reInc3)]
    
    param1<-seq(lb1,ub1,length.out=np1points)
    if (length(param2)>1) param2<-seq(lb2,ub2,length.out=np2points)
    if (length(param3)>1) param3<-seq(lb3,ub3,length.out=np3points)
  }

  if (niterations<3) {
    if (lb1<0.02) {
    }
    result<-fmincon(c(param1Max,param2Max,param3Max),llfun,ub=c(ub1,ub2,ub3),lb=c(lb1,lb2,lb3))
    param1Max<-result$par[1]
    param2Max<-result$par[2]
    param3Max<-result$par[3]
    Smax<- -result$value
  }
  Svals<-llfun(c(param1Max,param2Max,param3Max))
  if (dist=="random" && metaAnalysis$analysisVar=="sd") param2Max<-sign(param2Max)*sqrt(abs(param2Max))
  return(list(param1Max=param1Max,param2Max=param2Max,param3Max=param3Max,Smax=Smax,Svals=Svals))
}


runMetaAnalysis<-function(metaAnalysis,studies,hypothesis,metaResult){
  rs<-studies$rIV
  zs<-atanh(rs)
  ns<-studies$nval
  df1<-studies$df1
  
  fixed<-list(param1Max=NA,param2Max=NA,param3Max=NA,Smax=NA)
  random<-list(param1Max=NA,param2Max=NA,param3Max=NA,Smax=NA)
  single<-list(param1Max=NA,param2Max=NA,param3Max=NA,Smax=NA)
  gauss<-list(param1Max=NA,param2Max=NA,param3Max=NA,Smax=NA)
  exp<-list(param1Max=NA,param2Max=NA,param3Max=NA,Smax=NA)
  switch(metaAnalysis$analysisType,
         "fixed"={
           # a fixed analysis finds a single effect size
           metaAnalysis$includeNulls<-FALSE
           switch(metaAnalysis$method,
                  "MLE"={fixed<-getMaxLikelihood(zs,ns,df1,"fixed",metaAnalysis,hypothesis)},
                  "TF"={fixed<-getTrimFill(zs,ns,df1,"fixed",metaAnalysis,hypothesis)}
                  )
         },
         "random"={
           metaAnalysis$includeNulls<-FALSE
           switch(metaAnalysis$method,
                  "MLE"={random<-getMaxLikelihood(zs,ns,df1,"random",metaAnalysis,hypothesis)},
                  "TF"={random<-getTrimFill(zs,ns,df1,"random",metaAnalysis,hypothesis)}
           )
         },
         "world"={
           # doing world effects analysis
           
           # find best Single 
           if (metaAnalysis$modelPDF=="Single" || (metaAnalysis$modelPDF=="All")) 
             single<-getMaxLikelihood(zs,ns,df1,"Single",metaAnalysis,hypothesis)

           # find best Gauss
           if (metaAnalysis$modelPDF=="Gauss" || metaAnalysis$modelPDF=="All") 
             gauss<-getMaxLikelihood(zs,ns,df1,"Gauss",metaAnalysis,hypothesis)

           # find best Exp
           if (metaAnalysis$modelPDF=="Exp" || metaAnalysis$modelPDF=="All") 
             exp<-getMaxLikelihood(zs,ns,df1,"Exp",metaAnalysis,hypothesis)

         })
  
if (is.element(metaAnalysis$analysisType,c("fixed","random"))) {
  if (metaAnalysis$analysisType=="fixed") {
    fixed$param1Max<-c(metaResult$fixed$param1Max,tanh(fixed$param1Max))
    fixed$param2Max<-c(metaResult$fixed$param2Max,fixed$param2Max)
    fixed$param3Max<-c(metaResult$fixed$param3Max,fixed$param3Max)
    fixed$Smax<-c(metaResult$fixed$Smax,fixed$Smax)
    # if (metaAnalysis$includeNulls)
    #   fixed$param2Max<-c(metaResult$fixed$param2Max,fixed$param2Max)
    # else 
    #   fixed$param2Max<-NULL
    fixed$rpIV<-c(metaResult$fixed$rpIV,mean(studies$rpIV))
    fixed$rpSD<-c(metaResult$fixed$rpSD,0)
    bestParam1<-fixed$param1Max
    bestParam2<-0
    bestParam3<-fixed$param3Max
    bestDist<-"fixed"
    bestS<-fixed$Smax
    bestVals<-fixed$Svals
    count<-length(fixed$Smax)
  } else {
    rpSDex<-sqrt(mean((random$param1Max-studies$rIV)^2*(studies$nval-3)))-1
    random$param1Max<-c(metaResult$random$param1Max,tanh(random$param1Max))
    random$param2Max<-c(metaResult$random$param2Max,tanh(random$param2Max))
    random$param3Max<-c(metaResult$random$param3Max,tanh(random$param3Max))
    random$Smax<-c(metaResult$random$Smax,random$Smax)
    random$rpIV<-c(metaResult$random$rpIV,mean(studies$rpIV))
    random$rpSD<-c(metaResult$random$rpSD,std(studies$rpIV))
    random$rpSDex<-c(metaResult$random$rpSDex,rpSDex)
    bestParam1<-random$param1Max
    bestParam2<-random$param2Max
    bestParam3<-random$param3Max
    bestDist<-"random"
    bestS<-random$Smax
    bestVals<-random$Svals
    count<-length(random$Smax)
  }  
  
} else {
  use<-which.max(c(single$Smax,gauss$Smax,exp$Smax))
  switch(use,
         {bestDist<-"Single"
         bestParam1<-single$param1Max
         bestParam2<-single$param2Max
         bestParam3<-single$param3Max
         bestS<-single$Smax
         bestVals<-single$Svals
         },
         {bestDist<-"Gauss"
         bestParam1<-gauss$param1Max
         bestParam2<-gauss$param2Max
         bestParam3<-gauss$param3Max
         bestS<-gauss$Smax
         bestVals<-gauss$Svals
         },
         {bestDist<-"Exp"
         bestParam1<-exp$param1Max
         bestParam2<-exp$param2Max
         bestParam3<-exp$param3Max
         bestS<-exp$Smax
         bestVals<-exp$Svals
         }
  )

  if (!is.null(metaResult)) {
    bestDist<-c(metaResult$bestDist,bestDist)
    bestParam1<-c(metaResult$bestParam1,bestParam1)
    bestParam2<-c(metaResult$bestParam2,bestParam2)
    bestParam3<-c(metaResult$bestParam3,bestParam3)
    bestS<-c(metaResult$bestS,bestS)
    count<-length(bestS)
    single$param1Max<-c(metaResult$single$param1Max,single$param1Max)
    single$param2Max<-c(metaResult$single$param2Max,single$param2Max)
    single$param3Max<-c(metaResult$single$param3Max,single$param3Max)
    single$Smax<-c(metaResult$single$Smax,single$Smax)
    gauss$param1Max<-c(metaResult$gauss$param1Max,gauss$param1Max)
    gauss$param2Max<-c(metaResult$gauss$param2Max,gauss$param2Max)
    gauss$param3Max<-c(metaResult$gauss$param3Max,gauss$param3Max)
    gauss$Smax<-c(metaResult$gauss$Smax,gauss$Smax)
    exp$param1Max<-c(metaResult$exp$param1Max,exp$param1Max)
    exp$param2Max<-c(metaResult$exp$param2Max,exp$param2Max)
    exp$param3Max<-c(metaResult$exp$param3Max,exp$param3Max)
    exp$Smax<-c(metaResult$exp$Smax,exp$Smax)
  }
  count<-length(bestParam1)
}
  
  metaResult<-list(fixed=fixed,
                   random=random,
                   single=single,
                   gauss=gauss,
                   exp=exp,
                   bestDist=bestDist,
                   bestParam1=bestParam1,
                   bestParam2=bestParam2,
                   bestParam3=bestParam3,
                   bestS=bestS,
                   bestVals=bestVals,
                   count=count,
                   metaAnalysis=metaAnalysis,
                   result=studies
  )
  return(metaResult)
}

