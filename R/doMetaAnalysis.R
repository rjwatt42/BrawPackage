
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
  evidence$sigOnly<-(evidence$sigOnly || metaAnalysis$sourceBias)
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
  evidence$sigOnly<-(evidence$sigOnly || metaAnalysis$sourceBias)
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
  sourceAbs<-metaAnalysis$sourceAbs
  sigs<-isSignificant(method="NHST",p=r2p(tanh(zs),ns),r=tanh(zs),n=ns)
  res<-tryCatch({
    switch(dist,
           "fixed"={
             q<-trimfill(zs,1/sqrt(ns-3),ma.common=TRUE,common=TRUE,random=FALSE)
             nFill<-q$k-length(zs)
             bias<-nFill/(nFill+sum(!sigs))
             Smax<-getLogLikelihood(zs,ns,df1,dist,q$TE.common,spread=0,bias=bias,doAbs=sourceAbs)
             list(PDF="fixed",PDFk=q$TE.common,PDFshape=0,pRplus=0,sigOnly=bias,Smax=Smax,Svals=q$seTE)
           },
           "random"={
             q<-trimfill(zs,1/sqrt(ns-3),ma.common=FALSE,common=FALSE,random=TRUE)
             nFill<-q$k-length(zs)
             bias<-nFill/(nFill+sum(!sigs))
             Smax<-getLogLikelihood(zs,ns,df1,dist,q$TE.random,spread=q$tau,bias=bias,doAbs=sourceAbs)
             list(PDF="random",PDFk=q$TE.random,PDFshape=q$tau,pRplus=0,sigOnly=bias,Smax=Smax,Svals=q$seTE)
           }
           )
  },
  error=function(e){list(PDFk=NA,pRplus=NA,sigOnly=NA,Smax=NA,Svals=NA)},
  warning={},
  finally={}
  )
  if (is.infinite(res$PDFk)) res$PDFk<-NA
  if (is.infinite(res$pRplus)) res$pRplus<-NA
  if (is.infinite(res$sigOnly)) res$sigOnly<-NA
  return(res)
}

getMaxLikelihood<-function(zs,ns,df1,dist,metaAnalysis,hypothesis) {
  # PDFk is kvals
  # pRplus is normally nullvals
  
  defaultnpoints<-11
  np1points<-defaultnpoints
  np2points<-defaultnpoints
  np3points<-defaultnpoints
  np4points<-defaultnpoints
  np5points<-defaultnpoints
  
  niterations<-4
  minIterations<-3
  # reInc1<-(np1points-1)/2/3
  # reInc2<-(np2points-1)/2/3
  reInc1<-2
  reInc2<-2
  reInc3<-2
  reInc4<-2
  reInc5<-2

  sourceAbs<-metaAnalysis$sourceAbs
  
  if (metaAnalysis$analyseNulls) {
    param2Use<-seq(0,1,length.out=np2points)
  } else {
    param2Use<-1-metaAnalysis$sourceNulls
  }
  
  if (metaAnalysis$analyseBias) {
    param3Use<-seq(0,1,length.out=np2points)
  } else param3Use<-metaAnalysis$sourceBias
  
  param4Use<-0
  param5Use<-0

  switch(dist,
         "fixed"={
           if (sourceAbs) param1Use<-seq(0,1,length.out=np1points)*4
           else           param1Use<-seq(-1,1,length.out=np1points)*4
         },
         "random"={
           if (sourceAbs) param1Use<-seq(0,1,length.out=np1points)*4
           else           param1Use<-seq(-1,1,length.out=np1points)*4
           if (metaAnalysis$analysisVar=="sd") 
                param4Use<-seq(0,0.5,length.out=np4points)^2
           else param4Use<-seq(-0.1,1,length.out=np4points)*(0.5^2)
         },
         "Single"={
           if (sourceAbs) param1Use<-seq(0,1,length.out=np1points)*4
           else           param1Use<-seq(-1,1,length.out=np1points)*4
         },
         "Gauss"={
           param1Use<-seq(0,2,length.out=np1points)
         },
         "Exp"={
           param1Use<-seq(0,2,length.out=np1points)
         },
         "Gamma"={
           param1Use<-seq(0,2,length.out=np1points)
           param5Use<-seq(1,6,length.out=np5points)
         },
         "GenExp"={
           param1Use<-seq(0,2,length.out=np1points)
           param5Use<-seq(0.1,4,length.out=np5points)
         }
  )
  
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
  llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=x[2],bias=x[3],spread=x[4],shape=x[5],doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}

  S<-array(0,c(length(param1Use),length(param2Use),length(param3Use),length(param4Use),length(param5Use)))
  for (re in 1:niterations) {
    # get an approx result
    for (p1 in 1:length(param1Use))
      for (p2 in 1:length(param2Use))
        for (p3 in 1:length(param3Use))
          for (p4 in 1:length(param4Use))
            for (p5 in 1:length(param5Use))
              S[p1,p2,p3,p4,p5]<-llfun(c(param1Use[p1],param2Use[p2],param3Use[p3],param4Use[p4],param5Use[p5]))

    Smax<- -min(S,na.rm=TRUE)
    use<-which(S==-Smax, arr.ind = TRUE)
    PDFk<-param1Use[use[1,1]]
    lb1<-param1Use[max(1,use[1,1]-reInc1)]
    ub1<-param1Use[min(length(param1Use),use[1,1]+reInc1)]
    pRplus<-param2Use[use[1,2]]
    lb2<-param2Use[max(1,use[1,2]-reInc2)]
    ub2<-param2Use[min(length(param2Use),use[1,2]+reInc2)]
    sigOnly<-param3Use[use[1,3]]
    lb3<-param3Use[max(1,use[1,3]-reInc3)]
    ub3<-param3Use[min(length(param3Use),use[1,3]+reInc3)]
    PDFspread<-param4Use[use[1,4]]
    lb4<-param4Use[max(1,use[1,4]-reInc4)]
    ub4<-param4Use[min(length(param4Use),use[1,4]+reInc4)]
    PDFshape<-param5Use[use[1,5]]
    lb5<-param5Use[max(1,use[1,5]-reInc5)]
    ub5<-param5Use[min(length(param5Use),use[1,5]+reInc5)]
    
    # after 2 iterations, can we do a search?
    if (re>=minIterations) {
      params<-c(PDFk,pRplus,sigOnly,PDFspread,PDFshape)
      ub<-c(ub1,ub2,ub3,ub4,ub5)
      lb<-c(lb1,lb2,lb3,lb4,lb5)
      llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=x[2],bias=x[3],spread=x[4],shape=x[5],doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
      np<-1:5
      if (is.element(dist,c("fixed","random"))) {
        if (length(param3Use)==1 ) {
          llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=x[2],bias=param3Use,spread=x[3],shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
          np<-c(1,2,4)
        }
        if (length(param3Use)==1 && length(param2Use)==1) {
          llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=param2Use,bias=param3Use,spread=x[2],shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
          np<-c(1,4)
        }
        if (length(param3Use)==1 && length(param2Use)==1 && length(param4Use)==1) {
          llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=param2Use,bias=param3Use,spread=param4Use,shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
          np<-c(1)
        }
      }  else {
      if (length(param5Use)==1 ) {
        llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=x[2],bias=x[3],spread=x[4],shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
        np<-1:4
      }
      if (length(param4Use)==1 && length(param5Use)==1 ) {
        llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=x[2],bias=x[3],spread=param4Use,shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
        np<-1:3
      }
      if (length(param3Use)==1 && length(param4Use)==1 && length(param5Use)==1 ) {
        llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=x[2],bias=param3Use,spread=param4Use,shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
        np<-1:2
      }
      if (length(param2Use)==1 && length(param3Use)==1 && length(param4Use)==1 && length(param5Use)==1 ) {
        llfun<-function(x) { -(getLogLikelihood(zs,ns,df1,dist,location=x[1],prplus=param2Use,bias=param3Use,spread=param4Use,shape=param5Use,doAbs=sourceAbs)+approx(prior_z,priorDens,x[1])$y)}
        np<-1
      }
    }
      
    if (length(np)==1) {
      result<-optimize(llfun,interval=c(lb[np],ub[np]))$minimum
    } else{
      result<-tryCatch( {
        fminsearch(llfun,params[np],method='Hooke-Jeeves',lower=lb[np],upper=ub[np])$xmin
        # fmincon(params[np],llfun,ub=ub[np],lb=lb[np])
      }, 
      error = function(error_message){
        print(paste("fmincon error:  ",error_message))
        setBrawRes("debug",error_message)
        }
      )
    }
      if (!is.null(result)) break
    }
    param1Use<-seq(lb1,ub1,length.out=np1points)
    if (length(param2Use)>1) param2Use<-seq(lb2,ub2,length.out=np2points)
    if (length(param3Use)>1) param3Use<-seq(lb3,ub3,length.out=np3points)
    if (length(param4Use)>1) param4Use<-seq(lb4,ub4,length.out=np4points)
    if (length(param5Use)>1) param5Use<-seq(lb5,ub5,length.out=np5points)
  }

  if (!is.null(result)) {
    vals<-result
    # vals<-result$par
    for (i in 1:length(np)) 
      switch(np[i],
             PDFk<-vals[i],
             pRplus<-vals[i], 
             sigOnly<-vals[i], 
             PDFspread<-vals[i], 
             PDFshape<-vals[i], 
      )
  }
  Smax<-getLogLikelihood(zs,ns,df1,dist,
                   location=PDFk,
                   prplus=pRplus,bias=param3Use,
                   spread=PDFspread,shape=PDFshape,doAbs=sourceAbs)+approx(prior_z,priorDens,PDFk)$y
  Svals<- -S
  Spts<-list(PDFk=param1Use,
             pRplus=param2Use,
             bias=param3Use,
             PDFspread=param4Use,
             PDFshape=param5Use
  )
  
  # if (dist=="random" && metaAnalysis$analysisVar=="sd") PDFspread<-sign(PDFspread)*sqrt(abs(PDFspread))
  return(list(PDF=dist,PDFk=PDFk,pRplus=pRplus,sigOnly=sigOnly,PDFspread=PDFspread,PDFshape=PDFshape,Smax=Smax,Svals=Svals,Spts=Spts))
}

mergeMAResult<-function(multiple,single) {
  single$PDF<-c(multiple$PDF,single$PDF)
  single$PDFk<-c(multiple$PDFk,single$PDFk)
  single$pRplus<-c(multiple$pRplus,single$pRplus)
  single$sigOnly<-c(multiple$sigOnly,single$sigOnly)
  single$PDFspread<-c(multiple$PDFspread,single$PDFspread)
  single$PDFshape<-c(multiple$PDFshape,single$PDFshape)
  single$Smax<-c(multiple$Smax,single$Smax)
  return(single)
}

runMetaAnalysis<-function(metaAnalysis,studies,hypothesis,metaResult){
  if (!metaAnalysis$analyseBias && metaAnalysis$sourceBias) {
    p<-rn2p(studies$rIV,studies$nval)
    studies$rIV<-studies$rIV[p<0.05]
    studies$nval<-studies$nval[p<0.05]
    studies$df1<-studies$df1[p<0.05]
    studies$rpIV<-studies$rpIV[p<0.05]
  }
  rs<-studies$rIV
  zs<-atanh(rs)
  ns<-studies$nval
  df1<-studies$df1
  
  genexp<-gamma<-exp<-gauss<-single<-random<-fixed<-list(PDF=NA,PDFk=NA,pRplus=NA,sigOnly=NA,PDFspread=NA,PDFshape=NA,Smax=NA)
  switch(metaAnalysis$analysisType,
         "none"={},
         "fixed"={
           # a fixed analysis finds a single effect size
           metaAnalysis$analyseNulls<-FALSE
           switch(metaAnalysis$method,
                  "MLE"={fixed<-getMaxLikelihood(zs,ns,df1,"fixed",metaAnalysis,hypothesis)},
                  "TF"={fixed<-getTrimFill(zs,ns,df1,"fixed",metaAnalysis,hypothesis)}
                  )
         },
         "random"={
           metaAnalysis$analyseNulls<-FALSE
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
    best<-fixed
    else
      switch(use,
             {best<-fixed},
             {best<-random},
             {best<-single},
             {best<-gauss},
             {best<-exp},
             {best<-gamma},
             {best<-genexp}
      )
  best<-mergeMAResult(metaResult$best,best)
  
  switch(metaAnalysis$analysisType,
         "fixed"={
           fixed<-mergeMAResult(metaResult$fixed,fixed)
         },
         "random"={
           random<-mergeMAResult(metaResult$random,random)
         },
         "world"={
           single<-mergeMAResult(metaResult$single,single)
           gauss<-mergeMAResult(metaResult$gauss,gauss)
           exp<-mergeMAResult(metaResult$exp,exp)
           gamma<-mergeMAResult(metaResult$gamma,gamma)
           genexp<-mergeMAResult(metaResult$genexp,genexp)
         })
  
  metaResult<-list(fixed=fixed,
                   random=random,
                   single=single,
                   gauss=gauss,
                   exp=exp,
                   gamma=gamma,
                   genexp=genexp,
                   best=best,
                   count=length(best$Smax),
                   metaAnalysis=metaAnalysis,
                   result=studies
  )
  return(metaResult)
}

