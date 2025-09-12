
# META-ANALYSIS
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    

#' do a meta-analysis
#' @return metaResult object 
#' @examples
#' doMetaAnalysis<-function(metaSingle=NULL,metaAnalysis=makeMetaAnalysis(),
#'                          keepStudies=FALSE,shortHand=TRUE,
#'                          hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence)
#' @export
doMetaAnalysis<-function(metaSingle=braw.res$metaSingle,metaAnalysis=braw.def$metaAnalysis,
                         keepStudies=FALSE,shortHand=TRUE,
                         hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence
) {
  if (is.null(metaAnalysis)) metaAnalysis<-makeMetaAnalysis()
  # if (is.null(design)) design<-getDesign("Psych")
  evidence$sigOnly<-metaAnalysis$sourceBias
  evidence$shortHand<-shortHand
  
  localHypothesis<-hypothesis
  if (hypothesis$effect$world$On && is.element(metaAnalysis$analysisType,c("fixed","random")))
  {
    localHypothesis$effect$rIV<-getWorldEffect(1,localHypothesis$effect)
    localHypothesis$effect$world$On<-FALSE
  }

  if (is.null(metaSingle) || !keepStudies)
    studies<-multipleAnalysis(metaAnalysis$nstudies,localHypothesis,design,evidence)
  else
    studies<-metaSingle$result
  metaSingle<-runMetaAnalysis(metaAnalysis,studies,hypothesis,NULL)
  
  metaSingle$hypothesis<-hypothesis
  metaSingle$design<-design
  metaSingle$evidence<-evidence
  setBrawRes("metaSingle",metaSingle)
  metaSingle
}

#' do multiple meta-analyses
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
    if (hypothesis$effect$world$On && is.element(metaAnalysis$analysisType,c("fixed","random")))
    {
      localHypothesis$effect$rIV<-getWorldEffect(1,localHypothesis$effect)
      localHypothesis$effect$world$On<-FALSE
    }
    studies<-multipleAnalysis(metaAnalysis$nstudies,localHypothesis,design,evidence)
    metaMultiple<-runMetaAnalysis(metaAnalysis,studies,hypothesis,metaMultiple)
  }
  metaMultiple$hypothesis<-hypothesis
  metaMultiple$design<-design
  metaMultiple$evidence<-evidence
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
             Smax<-getLogLikelihood(zs,ns,df1,dist,q$TE.common,spread=0,bias=bias)
             list(param1=q$TE.common,param2=0,param3=bias,Smax=Smax,Svals=q$seTE)
           },
           "random"={
             q<-trimfill(zs,1/sqrt(ns-3),ma.common=FALSE,common=FALSE,random=TRUE)
             nFill<-q$k-length(zs)
             bias<-nFill/(nFill+sum(!sigs))
             Smax<-getLogLikelihood(zs,ns,df1,dist,q$TE.random,spread=q$tau,bias=bias)
             list(param1=q$TE.random,param2=q$tau,param3=bias,Smax=Smax,Svals=q$seTE)
           }
           )
  },
  error=function(e){list(param1=NA,param2=NA,param3=NA,Smax=NA,Svals=NA)},
  warning={},
  finally={}
  )
  if (is.infinite(res$param1)) res$param1<-NA
  if (is.infinite(res$param2)) res$param2<-NA
  if (is.infinite(res$param3)) res$param3<-NA
  return(res)
}

getMaxLikelihood<-function(zs,ns,df1,dist,metaAnalysis,hypothesis) {
  # param1 is kvals
  # param2 is normally nullvals
  
  defaultnpoints<-11
  np1points<-defaultnpoints
  np2points<-defaultnpoints
  np3points<-defaultnpoints
  np4points<-defaultnpoints
  
  niterations<-10
  # reInc1<-(np1points-1)/2/3
  # reInc2<-(np2points-1)/2/3
  reInc1<-2
  reInc2<-2
  reInc3<-2
  reInc4<-2
  
  
  if (is.element(dist,c("Gamma","GenExp"))) {
    param4Use<-seq(0.1,2.5,length.out=np4points)
  } else {
    param4Use<-0
  }
  
  if (metaAnalysis$modelNulls) {
    param2Use<-seq(0,1,length.out=np2points)
  } else {
    param2Use<-1
  }
  
  switch(dist,
         "fixed"={
           param1Use<-seq(-1,1,length.out=np1points)*4
           param2Use<-0
         },
         "random"={
           param1Use<-seq(-1,1,length.out=np1points)*4
           if (metaAnalysis$analysisVar=="sd") 
             param2Use<-seq(0,0.5,length.out=np2points)^2
           else param2Use<-seq(-0.1,1,length.out=np2points)*(0.5^2)
         },
         "Single"={
           param1Use<-seq(-1,1,length.out=np1points)
         },
         {
           param1Use<-seq(0,2,length.out=np1points)
         }
  )
  
  if (metaAnalysis$analyseBias) {
    if (is.element(dist,c("fixed","random","Exp"))) {
      param3Use<-seq(0,1,length.out=np2points)
    } else param3Use<-1
  } else param3Use<-metaAnalysis$sourceBias
  
  prior<-metaAnalysis$analysisPrior
  prior_z<-seq(min(param1Use),max(param1Use),length.out=101)
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
  
  # location refers to lambda for world metaA
  # spread refers to nulls for world metaA
  if (length(param4Use)==1) {
  llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=x[2],bias=x[3],shape=param4Use)+approx(prior_z,priorDens,x[1])$y)}
  if (length(param2Use)==1)
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=param2Use,bias=x[3],shape=param4Use)+approx(prior_z,priorDens,x[1])$y)}
  if (length(param3Use)==1)
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=x[2],bias=param3Use,shape=param4Use)+approx(prior_z,priorDens,x[1])$y)}
  if (length(param2Use)==1 && length(param3Use)==1)
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=param2Use,bias=param3Use,shape=param4Use)+approx(prior_z,priorDens,x[1])$y)}
  } else {
    llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=x[2],bias=x[3],shape=x[4])+approx(prior_z,priorDens,x[1])$y)}
    if (length(param2Use)==1)
      llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=param2Use,bias=x[3],shape=x[4])+approx(prior_z,priorDens,x[1])$y)}
    if (length(param3Use)==1)
      llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=x[2],bias=param3Use,shape=x[4])+approx(prior_z,priorDens,x[1])$y)}
    if (length(param2Use)==1 && length(param3Use)==1)
      llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],spread=param2Use,bias=param3Use,shape=x[4])+approx(prior_z,priorDens,x[1])$y)}
  }
  S<-array(0,c(length(param1Use),length(param2Use),length(param3Use),length(param4Use)))
  for (re in 1:niterations) {
    # get an approx result
    for (p1 in 1:length(param1Use))
      for (p2 in 1:length(param2Use))
        for (p3 in 1:length(param3Use))
          for (p4 in 1:length(param4Use))
            S[p1,p2,p3,p4]<-llfun(c(param1Use[p1],param2Use[p2],param3Use[p3],param4Use[p4]))

    Smax<- -min(S,na.rm=TRUE)
    use<-which(S==-Smax, arr.ind = TRUE)
    param1<-param1Use[use[1,1]]
    lb1<-param1Use[max(1,use[1,1]-reInc1)]
    ub1<-param1Use[min(length(param1Use),use[1,1]+reInc1)]
    param2<-param2Use[use[1,2]]
    lb2<-param2Use[max(1,use[1,2]-reInc2)]
    ub2<-param2Use[min(length(param2Use),use[1,2]+reInc2)]
    param3<-param3Use[use[1,3]]
    lb3<-param3Use[max(1,use[1,3]-reInc3)]
    ub3<-param3Use[min(length(param3Use),use[1,3]+reInc3)]
    param4<-param4Use[use[1,4]]
    lb4<-param4Use[max(1,use[1,4]-reInc4)]
    ub4<-param4Use[min(length(param4Use),use[1,4]+reInc4)]
    
    # after 2 iterations, can we do a search?
    if (re==2) {
      result<-tryCatch( {
        fmincon(c(param1,param2,param3,param4),llfun,ub=c(ub1,ub2,ub3,ub4),lb=c(lb1,lb2,lb3,lb4))
      }, 
      error = function(e){NULL}
      )
      if (!is.null(result)) break
    }
    param1Use<-seq(lb1,ub1,length.out=np1points)
    if (length(param2Use)>1) param2Use<-seq(lb2,ub2,length.out=np2points)
    if (length(param3Use)>1) param3Use<-seq(lb3,ub3,length.out=np3points)
    if (length(param4Use)>1) param4Use<-seq(lb4,ub4,length.out=np4points)
    result<-list(par=c(param1,param2,param3,param4),value=-Smax)
  }
  param1<-result$par[1]
  param2<-result$par[2]
  param3<-result$par[3]
  param4<-result$par[4]
  Smax<- -result$value

  Svals<-llfun(c(param1,param2,param3,param4))
  if (dist=="random" && metaAnalysis$analysisVar=="sd") param2<-sign(param2)*sqrt(abs(param2))
  return(list(param1=param1,param2=param2,param3=param3,param4=param4,Smax=Smax,Svals=Svals))
}


runMetaAnalysis<-function(metaAnalysis,studies,hypothesis,metaResult){
  rs<-studies$rIV
  zs<-atanh(rs)
  ns<-studies$nval
  df1<-studies$df1
  
  fixed<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  random<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  single<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  gauss<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  exp<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  gamma<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  genexp<-list(param1=NA,param2=NA,param3=NA,Smax=NA)
  switch(metaAnalysis$analysisType,
         "none"={},
         "fixed"={
           # a fixed analysis finds a single effect size
           metaAnalysis$modelNulls<-FALSE
           switch(metaAnalysis$method,
                  "MLE"={fixed<-getMaxLikelihood(zs,ns,df1,"fixed",metaAnalysis,hypothesis)},
                  "TF"={fixed<-getTrimFill(zs,ns,df1,"fixed",metaAnalysis,hypothesis)}
                  )
         },
         "random"={
           metaAnalysis$modelNulls<-FALSE
           switch(metaAnalysis$method,
                  "MLE"={random<-getMaxLikelihood(zs,ns,df1,"random",metaAnalysis,hypothesis)},
                  "TF"={random<-getTrimFill(zs,ns,df1,"random",metaAnalysis,hypothesis)}
           )
         },
         "world"={
           # doing world effects analysis
           
           # find best Single 
           if (metaAnalysis$modelPDF=="Single" || (metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) 
             single<-getMaxLikelihood(zs,ns,df1,"Single",metaAnalysis,hypothesis)

           # find best Gauss
           if (metaAnalysis$modelPDF=="Gauss" || metaAnalysis$modelPDF=="All") 
             gauss<-getMaxLikelihood(zs,ns,df1,"Gauss",metaAnalysis,hypothesis)

           # find best Exp
           if (metaAnalysis$modelPDF=="Exp" || metaAnalysis$modelPDF=="All") 
             exp<-getMaxLikelihood(zs,ns,df1,"Exp",metaAnalysis,hypothesis)
           
           # find best Gamma 
           if (metaAnalysis$modelPDF=="Gamma" || (metaAnalysis$modelPDF=="All" && braw.env$includeGamma)) 
             gamma<-getMaxLikelihood(zs,ns,df1,"Gamma",metaAnalysis,hypothesis)
           
           # find best GenExp 
           if (metaAnalysis$modelPDF=="GenExp" || (metaAnalysis$modelPDF=="All" && braw.env$includeGenExp)) 
             genexp<-getMaxLikelihood(zs,ns,df1,"GenExp",metaAnalysis,hypothesis)
           
         })
  
  use<-which.max(c(fixed$Smax,random$Smax,single$Smax,gauss$Smax,exp$Smax,gamma$Smax,genexp$Smax))
  bestDist<-c("fixed","random","Single","Gauss","Exp","Gamma","GenExp")[use]
  if (metaAnalysis$analysisType=="none")
    bestR<-fixed
    else
      switch(use,
             {bestR<-fixed},
             {bestR<-random},
             {bestR<-single},
             {bestR<-gauss},
             {bestR<-exp},
             {bestR<-gamma},
             {bestR<-genexp}
      )
  bestParam1<-bestR$param1
  bestParam2<-bestR$param2
  bestParam3<-bestR$param3
  bestParam4<-bestR$param4
  bestS<-bestR$Smax
  bestVals<-bestR$Svals
  if (!is.null(metaResult)) {
    bestDist<-c(metaResult$best$dist,bestDist)
    bestParam1<-c(metaResult$best$param1,bestParam1)
    bestParam2<-c(metaResult$best$param2,bestParam2)
    bestParam3<-c(metaResult$best$param3,bestParam3)
    bestParam4<-c(metaResult$best$param4,bestParam4)
    bestS<-c(metaResult$best$S,bestS)
  }
  count<-length(bestS)
  
  switch(metaAnalysis$analysisType,
         "fixed"={
           fixed$param1<-c(metaResult$fixed$param1,tanh(fixed$param1))
           fixed$param2<-c(metaResult$fixed$param2,fixed$param2)
           fixed$param3<-c(metaResult$fixed$param3,fixed$param3)
           fixed$param4<-c(metaResult$fixed$param4,fixed$param4)
           fixed$Smax<-c(metaResult$fixed$Smax,fixed$Smax)
           fixed$rpIV<-c(metaResult$fixed$rpIV,mean(studies$rpIV))
           fixed$rpSD<-c(metaResult$fixed$rpSD,0)
         },
         "random"={
           rpSDex<-sqrt(mean((random$param1-studies$rIV)^2*(studies$nval-3)))-1
           random$param1<-c(metaResult$random$param1,tanh(random$param1))
           random$param2<-c(metaResult$random$param2,tanh(random$param2))
           random$param3<-c(metaResult$random$param3,tanh(random$param3))
           random$param4<-c(metaResult$random$param4,tanh(random$param4))
           random$Smax<-c(metaResult$random$Smax,random$Smax)
           random$rpIV<-c(metaResult$random$rpIV,mean(studies$rpIV))
           random$rpSD<-c(metaResult$random$rpSD,std(studies$rpIV))
           random$rpSDex<-c(metaResult$random$rpSDex,rpSDex)
         },
         "world"={
           single$param1<-c(metaResult$single$param1,single$param1)
           single$param2<-c(metaResult$single$param2,single$param2)
           single$param3<-c(metaResult$single$param3,single$param3)
           single$param4<-c(metaResult$single$param4,single$param4)
           single$Smax<-c(metaResult$single$Smax,single$Smax)
           gauss$param1<-c(metaResult$gauss$param1,gauss$param1)
           gauss$param2<-c(metaResult$gauss$param2,gauss$param2)
           gauss$param3<-c(metaResult$gauss$param3,gauss$param3)
           gauss$param4<-c(metaResult$gauss$param4,gauss$param4)
           gauss$Smax<-c(metaResult$gauss$Smax,gauss$Smax)
           exp$param1<-c(metaResult$exp$param1,exp$param1)
           exp$param2<-c(metaResult$exp$param2,exp$param2)
           exp$param3<-c(metaResult$exp$param3,exp$param3)
           exp$param4<-c(metaResult$exp$param4,exp$param4)
           exp$Smax<-c(metaResult$exp$Smax,exp$Smax)
           gamma$param1<-c(metaResult$gamma$param1,gamma$param1)
           gamma$param2<-c(metaResult$gamma$param2,gamma$param2)
           gamma$param3<-c(metaResult$gamma$param3,gamma$param3)
           gamma$param4<-c(metaResult$gamma$param4,gamma$param4)
           gamma$Smax<-c(metaResult$gamma$Smax,gamma$Smax)
           genexp$param1<-c(metaResult$genexp$param1,genexp$param1)
           genexp$param2<-c(metaResult$genexp$param2,genexp$param2)
           genexp$param3<-c(metaResult$genexp$param3,genexp$param3)
           genexp$param4<-c(metaResult$genexp$param4,genexp$param4)
           genexp$Smax<-c(metaResult$genexp$Smax,genexp$Smax)
         })
  
  metaResult<-list(fixed=fixed,
                   random=random,
                   single=single,
                   gauss=gauss,
                   exp=exp,
                   gamma=gamma,
                   genexp=genexp,
                   best=list(dist=bestDist,
                             S=bestS,
                             param1=bestParam1,
                             param2=bestParam2,
                             param3=bestParam3,
                             param4=bestParam4,
                             vals=bestVals),
                   count=count,
                   metaAnalysis=metaAnalysis,
                   result=studies
  )
  return(metaResult)
}

