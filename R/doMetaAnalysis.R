
# META-ANALYSIS
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    

#' @return metaResult object 
#' @examples
#' doMetaAnalysis<-function(nsims=50,metaAnalysis=makeMetaAnalysis(),
#'                          hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,
#'                          metaResult=NULL)
#' @export
doMetaAnalysis<-function(metaSingle=braw.res$metaSingle,metaAnalysis=braw.def$metaAnalysis,
                         keepStudies=FALSE,shortHand=TRUE,
                         hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence
) {
  if (is.null(metaAnalysis)) metaAnalysis<-makeMetaAnalysis()
  evidence$sigOnly<-metaAnalysis$sigOnlySource
  evidence$shortHand<-shortHand
  
  localHypothesis<-hypothesis
  if (hypothesis$effect$world$worldOn && is.element(metaAnalysis$analysisType,c("fixed","random")))
  {
    localHypothesis$effect$rIV<-getWorldEffect(localHypothesis$effect)
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

doMetaMultiple<-function(nsims=100,metaMultiple=braw.res$metaMultiple,metaAnalysis=braw.def$metaAnalysis,
                         keepStudies=FALSE,
                         hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence
) {
  if (is.null(metaAnalysis)) metaAnalysis<-makeMetaAnalysis()
  evidence$sigOnly<-metaAnalysis$sigOnlySource
  
  for (i in 1:nsims) {
    localHypothesis<-hypothesis
    if (hypothesis$effect$world$worldOn && is.element(metaAnalysis$analysisType,c("fixed","random")))
    {
      localHypothesis$effect$rIV<-getWorldEffect(localHypothesis$effect)
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


getMaxLikelihood<-function(zs,ns,df1,dist,metaAnalysis,hypothesis) {
  # param1 is kvals
  # param2 is normally nullvals
  
  np1points<-101
  np2points<-13
  
  niterations<-1
  reInc<-(np1points-1)/2/3
  
  if (metaAnalysis$includeNulls) {
    param2<-seq(0,1,length.out=np2points)
  } else {
    param2<-0
  }
  if (dist=="Single") {
    param1<-seq(-1,1,length.out=np1points)
  } else {
    param1<-seq(0.01,1,length.out=np1points)
  }
  
  if (dist=="fixed") {
    param1<-seq(-1,1,length.out=np1points)*atanh(0.95)
    param0<-0
  }
  if (dist=="random") {
    param1<-seq(-1,1,length.out=np1points)*atanh(0.95)
    if (metaAnalysis$analysisVar=="sd") param2<-seq(0,0.5,length.out=np2points)^2
    else param2<-seq(-0.1,1,length.out=np2points)*(0.5^2)
  }
  
  remove_nonsig<-metaAnalysis$includeBias
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
  
  for (re in 1:niterations) {
    S<-getLogLikelihood(zs,ns,df1,dist,param1,param2,remove_nonsig)+approx(prior_z,priorDens,param1)$y
    Smax<-max(S,na.rm=TRUE)
    
    use<-which(S==Smax, arr.ind = TRUE)
    param2Max<-param2[use[1,2]]
    lb2<-param2[max(1,use[1,2]-reInc)]
    ub2<-param2[min(length(param2),use[1,2]+reInc)]
    param1Max<-param1[use[1,1]]
    lb1<-param1[max(1,use[1,1]-reInc)]
    ub1<-param1[min(length(param1),use[1,1]+reInc)]
    
    param1<-seq(lb1,ub1,length.out=np1points)
    if (metaAnalysis$includeNulls) {
      param2<-seq(lb2,ub2,length.out=np2points)
    }
  }
  
  if (niterations==1) {      
    fun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,x[1],x[2],remove_nonsig)+approx(prior_z,priorDens,x[1])$y)}
    result<-fmincon(c(param1Max,param2Max),fun,ub=c(ub1,ub2),lb=c(lb1,lb2))
    param1Max<-result$par[1]
    param2Max<-result$par[2]
    Smax<- -result$value
  }
  Svals<-getLogLikelihood(zs,ns,df1,dist,param1Max,param2Max,remove_nonsig,returnVals=TRUE)
         +approx(prior_z,priorDens,param1Max)$y
  if (metaAnalysis$analysisVar=="sd") param2Max<-sign(param2Max)*sqrt(abs(param2Max))
  return(list(param1Max=param1Max,param2Max=param2Max,Smax=Smax,Svals=Svals))
}


runMetaAnalysis<-function(metaAnalysis,studies,hypothesis,metaResult){
  rs<-studies$rIV
  zs<-atanh(rs)
  ns<-studies$nval
  df1<-studies$df1
  
  fixed<-list(param1Max=NA,param2Max=NA,Smax=NA)
  random<-list(param1Max=NA,param2Max=NA,Smax=NA)
  single<-list(param1Max=NA,param2Max=NA,Smax=NA)
  gauss<-list(param1Max=NA,param2Max=NA,Smax=NA)
  exp<-list(param1Max=NA,param2Max=NA,Smax=NA)
  switch(metaAnalysis$analysisType,
         "fixed"={
           # a fixed analysis finds a single effect size
           metaAnalysis$includeNulls<-FALSE
           fixed<-getMaxLikelihood(zs,ns,df1,"fixed",metaAnalysis,hypothesis)
         },
         "random"={
           metaAnalysis$includeNulls<-FALSE
           random<-getMaxLikelihood(zs,ns,df1,"random",metaAnalysis,hypothesis)
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
    fixed$Smax<-c(metaResult$fixed$Smax,fixed$Smax)
    if (metaAnalysis$includeNulls)
      fixed$param2Max<-c(metaResult$fixed$param2Max,fixed$param2Max)
    else 
      fixed$param2Max<-NULL
    fixed$rpIV<-c(metaResult$fixed$rpIV,mean(studies$rpIV))
    fixed$rpSD<-c(metaResult$fixed$rpSD,0)
    bestParam1<-fixed$param1Max
    bestParam2<-fixed$param2Max
    bestDist<-"fixed"
    bestS<-fixed$Smax
    bestVals<-fixed$Svals
    count<-length(fixed$Smax)
  } else {
    rpSDex<-sqrt(mean((random$param1Max-studies$rIV)^2*(studies$nval-3)))-1
    random$param1Max<-c(metaResult$random$param1Max,tanh(random$param1Max))
    random$Smax<-c(metaResult$random$Smax,random$Smax)
    random$param2Max<-c(metaResult$random$param2Max,tanh(random$param2Max))
    random$rpIV<-c(metaResult$random$rpIV,mean(studies$rpIV))
    random$rpSD<-c(metaResult$random$rpSD,std(studies$rpIV))
    random$rpSDex<-c(metaResult$random$rpSDex,rpSDex)
    bestParam1<-random$param1Max
    bestParam2<-random$param2Max
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
         bestS<-single$Smax
         bestVals<-single$Svals
         },
         {bestDist<-"Gauss"
         bestParam1<-gauss$param1Max
         bestParam2<-gauss$param2Max
         bestS<-gauss$Smax
         bestVals<-gauss$Svals
         },
         {bestDist<-"Exp"
         bestParam1<-exp$param1Max
         bestParam2<-exp$param2Max
         bestS<-exp$Smax
         bestVals<-exp$Svals
         }
  )

  if (!is.null(metaResult)) {
    bestDist<-c(metaResult$bestDist,bestDist)
    bestParam1<-c(metaResult$bestParam1,bestParam1)
    bestParam2<-c(metaResult$bestParam2,bestParam2)
    bestS<-c(metaResult$bestS,bestS)
    count<-length(bestS)
    single$param1Max<-c(metaResult$single$param1Max,single$param1Max)
    single$Smax<-c(metaResult$single$Smax,single$Smax)
    single$param2Max<-c(metaResult$single$param2Max,single$param2Max)
    gauss$param1Max<-c(metaResult$gauss$param1Max,gauss$param1Max)
    gauss$Smax<-c(metaResult$gauss$Smax,gauss$Smax)
    gauss$param2Max<-c(metaResult$gauss$param2Max,gauss$param2Max)
    exp$param1Max<-c(metaResult$exp$param1Max,exp$param1Max)
    exp$Smax<-c(metaResult$exp$Smax,exp$Smax)
    exp$param2Max<-c(metaResult$exp$param2Max,exp$param2Max)
  }
}
  
  metaResult<-list(fixed=fixed,
                   random=random,
                   single=single,
                   gauss=gauss,
                   exp=exp,
                   bestDist=bestDist,
                   bestParam1=bestParam1,
                   bestParam2=bestParam2,
                   bestS=bestS,
                   bestVals=bestVals,
                   count=count,
                   metaAnalysis=metaAnalysis,
                   result=studies
  )
  return(metaResult)
}

