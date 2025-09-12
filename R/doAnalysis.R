
#' makes a sample and analyses it
#' 
#' @returns result object
#' @examples
#' analysis<-doSingle(hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),autoShow=braw.env$autoShow)#' make a multiple samples
#' @export
doSingle<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,onlyReplication=FALSE,autoShow=braw.env$autoShow){
  # sample<-doSample(hypothesis=hypothesis,design=design,autoShow=FALSE)
  # result<-doAnalysis(sample,evidence=evidence,autoShow=autoShow)
  if (evidence$metaAnalysis$On) {
    metaSingle<-doMetaAnalysis(metaSingle=NULL,metaAnalysis=evidence$metaAnalysis,keepStudies=FALSE,
                             hypothesis=hypothesis,design=design,evidence=evidence)
    if (autoShow) print(showMetaSingle(metaSingle))
    return(metaSingle)
  } 
  
  evidence$shortHand<-FALSE
  oldResult<-NULL
  if (onlyReplication) {
    if (design$Replication$Keep=="MetaAnalysis") oldResult<-braw.res$result
    else {
    if (!is.null(braw.res$result$ResultHistory$original)) oldResult<-braw.res$result$ResultHistory$original
    else oldResult<-braw.res$result
    }
    if (is.null(oldResult)) {
      design$Replication$On<-FALSE
      oldResult<-doSingle(hypothesis=hypothesis,design=design,evidence=evidence)
      design$Replication$On<-TRUE
    }
    oldResult$design$Replication$On<-TRUE
  }
  result<-runSimulation(hypothesis=hypothesis,design=design,evidence=evidence,oldResult=oldResult,autoShow=FALSE)
  setBrawRes("result",result)
  if (autoShow) print(showSingle(result))
  
  return(result)
}


test2effectsize<-function(tname,tval,df1,df2) {
  switch(tname,
         "r"=return(tval),
         "t"=return(tval/(sqrt(tval^2+df2))),
         "F"=return(sqrt(tval*df1/(tval*df1+df2))),
         "chi2"=return(sqrt(tval/df2))
  )
}


check_r<-function(r,from=" ") {
  if (!is.numeric(r)) {
    print(paste(from,"r-exception",format(r)))
    r[!is.numeric(r)]<-0
  }
  if (any(abs(r)>1)) {
    print(paste(from,"r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  return(r)
}

check_n<-function(n,from=" ") {
  if (!is.numeric(n)) {
    print(paste(from,"n-exception",format(n)))
    return(0)
  }
  if (any(n<3)) {
    print(paste(from,"n-exception",format(min(abs(n)),digits=3)))
    n[n<3]<-4
    return(n)
  }
  return(n)
}


wsd<-function(x,w=1,na.rm=TRUE) {
  if (length(w)==1) {
    sqrt(mean((x-mean(x,na.rm=na.rm))^2))
  } else {
    sqrt(sum((x-mean(x,na.rm=na.rm))^2 *w)/sum(w))
  }
}


p2r<-function(p,n,df1=1) {
  n<-check_n(n,"p2r")
  df2<-n-(df1+1)
  
  Fvals <- qf(1-p,df1,df2)
  r <- sqrt(Fvals*df1)/sqrt(Fvals*df1+df2)
  return(r)
  
  t_vals <- qt(p/2,n-2)
  r_vals <- t_vals/sqrt(t_vals^2+(n-2))
  r_vals
}


r2p<-function(r,n,df1=1){
  r<-check_r(r,"r2p")
  n<-check_n(n,"r2p")
  df2<-n-(df1+1)
  
  Fvals<-r^2/(1-r^2)*df2/df1
  p<-(1-pf(Fvals,df1,df2))
  return(p)
  
  if (any(df1>1)) {
    Fvals<-r^2/(1-r^2)*df2/df1
    (1-pf(Fvals,df1,df2))
  } else {
    t_vals<-r/r2se(r,n)
    (1-pt(abs(t_vals),df2))*2
  }
  
}

r2se<-function(r,n){
  r<-check_r(r,"r2se")
  n<-check_n(n,"r2se")
  sqrt((1-r^2)/(as.vector(n)-2))
}

r2ci<-function(r,n,s=0){
  r<-check_r(r,"r2ci")
  n<-check_n(n,"r2ci")
  z<-atanh(r)
  zci<-qnorm(1-0.05/2)*sqrt(1/(n-3))
  if (s==0){
    cbind(tanh(z-zci),tanh(z+zci))
  } else {
    tanh(z+s*zci)
  }
}

res2llr<-function(analysis,method=braw.env$STMethod) {
  r2llr(analysis$rIV,analysis$nval,analysis$df1,method,analysis$evidence$llr,analysis$evidence$prior)
}

r2llr<-function(r,n,df1,method=braw.env$STMethod,llr=list(e1=c(),e2=0),world=NULL) {
  r<-check_r(r,from="r2llr")
  n<-check_n(n,from="r2llr")
  z<-atanh(r)
  if (method=="dLLR") {
    z<-abs(z)
    if (!is.matrix(z)) {
      z<-matrix(z,ncol=1)
    }
    if (!is.matrix(n)) {
      n<-matrix(n,ncol=1)
    }
    if (is.null(world) || is.null(world$On) || !world$On) {
      world$PDF<-"Single"
      world$pRPlus<-0.5
    }
    lk1<-getLogLikelihood(z,n,df1,world$PDF,world$PDFk,spread=0,bias=FALSE,returnVals=TRUE)
    lk2<-getLogLikelihood(z,n,df1,world$PDF,world$PDFk,spread=1,bias=FALSE,returnVals=TRUE)
    lk1<-lk1+log(world$pRPlus)
    lk2<-lk2+log(1-world$pRPlus)
    llk<-lk1-lk2
  } else {
    if (isempty(llr$e1) || is.na(llr$e1)) { llr1=z }
    else {llr1=llr$e1}
    lk1<-dnorm(llr1,mean=z,sd=1/sqrt(n-3))
    lk2<-dnorm(llr$e2,mean=z,sd=1/sqrt(n-3))
    llk<-log(lk1/lk2)
  }
  llk
  # for llr1=0 this can be simplified to just this: z^2*(n-3)/2
}



model2directeffect<-function(mF){
  if (any(class(mF)[1]==c("lmerMod","glmerMod")))
  {
    mTerms<-attr(terms(mF),"term.labels")
    data<-model.frame(mF)
    expected<-fitted(mF)
    residuals<-residuals(mF)
  } else {
    mTerms<-attr(mF$terms,"term.labels")
    data<-mF$model
    expected<-mF$fitted.values
    residuals<-mF$residuals
  }
  dv<-data[,1]
  n<-length(dv)
  
  if (is.numeric(dv)) {
    DVgain<-wsd(dv,na.rm=TRUE)
  } else {
    DVgain<-wsd(expected+residuals)
  }
  
  # we find effect-size by 
  # looking at the change in prediction around the centre of an interval predictor
  # looking at the sd of predictions for all possible cases of categorical predictors

  if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
    p1<-data$participant
    data$participant<-p1[1]
  }

  directEffects<-c()
  for (iTerm in 1:length(mTerms)) {
    if (!grepl(":",mTerms[iTerm])) {
        v1<-data[[mTerms[iTerm]]]
        if (is.numeric(v1)){
          m1<-mean(v1,na.rm=TRUE)+c(-1,1)*wsd(v1,na.rm=TRUE)
        } else {
          m1<-levels(v1)
        }
        rawData<-data
        v<-c()
        for (i in 1:length(m1)) {
          rawData[[mTerms[iTerm]]]<-m1[i]
          if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
            v<-c(v,mean(predict(mF,rawData)))
          } else {
            v<-c(v,mean(predict.lm(mF,rawData)))
          }
        }
        # this next happens when there is a negative correlation between the 
        # main effect term and the interaction term
        # if (wsd(v)/DVgain>1) browser()
        ncoeff1<-wsd(v) * sign(sum(diff(v)))
    } else {
      terms<-strsplit(mTerms[iTerm],":")
      terms<-terms[[1]]
      v1<-data[[terms[1]]]
      if (is.numeric(v1)){
        h1<-c(1,1)
        m1<-mean(v1,na.rm=TRUE)+c(-1,1)*wsd(v1,na.rm=TRUE)
      } else {
        m1<-levels(v1)
      }
      v2<-data[[terms[2]]]
      if (is.numeric(v2)){
        m2<-mean(v2,na.rm=TRUE)+c(-1,1)*wsd(v2,na.rm=TRUE)
      } else {
        m2<-levels(v2)
      }
      rawData<-data
      v<-c()
      for (i1 in 1:length(m1)) {
        rawData[[terms[1]]]<-m1[i1]
        for (i2 in 1:length(m2)) {
          rawData[[terms[2]]]<-m2[i2]
          if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
            v<-c(v,mean(predict(mF,rawData)))
          } else {
            v<-c(v,mean(predict.lm(mF,rawData)))
          }
        }
      }
      dim(v)<-c(length(m2),length(m1))
      ie<-t(t(v)-colMeans(v))-rowMeans(v)
      ncoeff1<-wsd(ie,1)*sign(sum(diff(-ie[1,])))
    }
    directEffects<-c(directEffects,ncoeff1)
  }
  directEffects[is.na(directEffects)]=0
  return(directEffects/DVgain)
}

model2uniqueeffect<-function(anU){
  # get the unique effects
  if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
  n2<-nrow(anU)
  uniqueEffects<-sqrt(anU$`Sum Sq`[n1:(n2-1)]/sum(anU$`Sum Sq`[n1:n2]))
  uniqueEffects<-uniqueEffects*sign(uniqueEffects)
  uniqueEffects[is.na(uniqueEffects)]=0

  return(uniqueEffects)
}

model2totaleffect<-function(mF){
  if (any(class(mF)[1]==c("lmerMod","glmerMod")))
  {
    mTerms<-attr(terms(mF),"term.labels")
    data<-model.frame(mF)
  } else {
    mTerms<-attr(mF$terms,"term.labels")
    data<-mF$model
  }
  dv<-data[,1]

  totalEffects<-c()
  for (iTerm in 1:length(mTerms)) {
    formula<-paste0("dv~",mTerms[iTerm])
    if (is.numeric(dv)) {
      # total effect sizes
      lm1total<-lm(formula=as.formula(formula),data=data)
    } else {
      lm1total<-glm(formula=as.formula(formula),data=data,family="binomial")
    }
    totalEffects<-c(totalEffects,model2directeffect(lm1total))
  }
  return(totalEffects)
}

model2fulleffect<-function(mF,anU) {
  if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
    # overall model effect-size
    if (class(mF)=="glmerMod") {
      r.full<-sqrt((sum(anU$Chisq)-anU$Chisq[1])/nlevels(mF$model$participant))
    } else {
      r.full<-sd(predict(mF))/sd(attributes(mF)$frame$dv)
    }
  } else {
    # r.full<-sd(mF$fitted.values)/sd(mF$fitted.values+mF$residuals)
    if (class(mF)[1]=="lm") r.full<-sd(mF$fitted.values)/sd(mF$model$dv)
    else {
    r.full<-sqrt(with(summary(mF), 1 - deviance/null.deviance))
    #   if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
    #   n2<-nrow(anU)-1
    #   r.full<-sqrt(sum(anU$`Sum Sq`[n1:n2])/sum(anU$`Sum Sq`))
    }
  }
  return(r.full)
}


appendList <- function (x1, x2) 
{
  xnames <- names(x1)
  for (v in names(x2)) {
    x2[[v]]<-if (is.null(x2[[v]])) NA else x2[[v]]
    x1[[v]] <- if (v %in% xnames && is.list(x1[[v]]) && is.list(x2[[v]])) 
      appendList(x1[[v]], x2[[v]])
    else c(x1[[v]], x2[[v]])
  }
  x1
}

multipleAnalysis<-function(nsims=1,hypothesis,design,evidence,newResult=c(),onlyReplication=NULL,oldResult=NULL){
  rho<-hypothesis$effect$rIV
  rho2<-hypothesis$effect$rIV2
  
  while (length(rho)<nsims) {rho<-rep(rho,nsims)}
  if (!is.null(hypothesis$IV2)) {
    if (length(rho2)<nsims) {rho2<-rep(rho2,nsims)}
  }

  offset<-sum(!is.na(newResult$rIV))
  for (i in 1:nsims){
    hypothesis$effect$rIV<-rho[i]
    if (!is.null(hypothesis$IV2)) {hypothesis$effect$rIV2<-rho2[i]}
    
    res<-runSimulation(hypothesis,design,evidence,oldResult=oldResult)
    
    if (is.na(res$rIV)) {
      res$rIV<-0
      res$pIV<-1
      res$nval<-0
    }
    if (is.null(newResult)) newResult<-res
    else {
      j<-i+offset
      newResult$rIV[j]<-res$rIV
      newResult$rpIV[j]<-res$rpIV
      newResult$pIV[j]<-res$pIV
      newResult$roIV[j]<-res$roIV
      newResult$poIV[j]<-res$poIV
      newResult$nval[j]<-res$nval
      newResult$noval[j]<-res$noval
      newResult$df1[j]<-res$df1
      newResult$llk[j]<-res$llk
      newResult$AIC[j]<-res$AIC
      newResult$AICnull[j]<-res$AICnull
      if (!is.null(hypothesis$IV2)) {
        if (evidence$AnalysisTerms[2]) {
          newResult$rIV2[j]<-res$rIV2
          newResult$pIV2[j]<-res$pIV2
        } else {
          newResult$rIV2[j]<-NA
          newResult$pIV2[j]<-NA
        }
        if (evidence$AnalysisTerms[3]) {
          newResult$rIVIV2DV[j]<-res$rIVIV2DV
          newResult$pIVIV2DV[j]<-res$pIVIV2DV
        } else {
          newResult$rIVIV2DV[j]<-NA
          newResult$pIVIV2DV[j]<-NA
        }

        nrep<-length(newResult$r$direct[j,])-length(res$r$direct)
        newResult$r$direct[j,]<-c(res$r$direct,rep(NA,nrep))
        newResult$r$unique[j,]<-c(res$r$unique,rep(NA,nrep))
        newResult$r$total[j,]<-c(res$r$total,rep(NA,nrep))
        newResult$p$direct[j,]<-c(res$p$direct,rep(NA,nrep))
        newResult$p$unique[j,]<-c(res$p$unique,rep(NA,nrep))
        newResult$p$total[j,]<-c(res$p$total,rep(NA,nrep))
      }
      if (!is.null(res$sem)){
      newResult$sem[j,]<-res$sem
      colnames(newResult$sem)<-colnames(res$sem)
      }
      newResult$iv.mn[j]<-res$iv.mn
      newResult$iv.sd[j]<-res$iv.sd
      newResult$iv.sk[j]<-res$iv.sk
      newResult$iv.kt[j]<-res$iv.kt
      newResult$dv.mn[j]<-res$dv.mn
      newResult$dv.sd[j]<-res$dv.sd
      newResult$dv.sk[j]<-res$dv.sk
      newResult$dv.kt[j]<-res$dv.kt
      newResult$er.mn[j]<-res$er.mn
      newResult$er.sd[j]<-res$er.sd
      newResult$er.sk[j]<-res$er.sk
      newResult$er.kt[j]<-res$er.kt
    }
  }
  
  newResult$hypothesis<-hypothesis
  newResult$design<-design
  newResult$evidence<-evidence
  newResult
}

convert2Interval<-function(var) {
  var$type<-"Interval"
  var$mu<-var$median
  var$sd<-var$iqr*qnorm(0.75)
}


#' analyse a sample
#' 
#' @returns result object
#' @examples
#' generalAnalysis<-function(allData,InteractionOn,withins=FALSE,ssqType="Type3",caseOrder="AsStated")
#' @export
generalAnalysis<-function(allData,AnalysisTerms,withins=FALSE,ssqType="Type3",caseOrder="AsStated") {
  
  if (ncol(allData)<3) {
    return(NULL)
  }
  
  no_ivs<-ncol(allData)-2
  n<-nrow(allData)
  
  catVars<-c()
  for (i in 2:ncol(allData)) {
    # get Categorical cases sorted
    if (is.factor(allData[,i])) {
      switch (caseOrder,
              "Alphabetic"={ref=sort(levels(allData[,i]))[1]},
              "AsFound"={ref=as.numeric(allData[1,i])},
              "Frequency"={ref=which.max(tabulate(match(allData[,i], levels(allData[,i]))))},
              "AsStated"={ref=levels(allData[,i])[[1]]}
      )
      allData[,i]<-relevel(allData[,i],ref=ref)
      catVars<-c(catVars,TRUE)
    } else {
      catVars<-c(catVars,FALSE)
    }
  }
  
  # MAKE MAIN DATA STORAGE
  analysisRawData<-data.frame(allData)
  names(analysisRawData)<-c("participant","dv",paste0("iv",1:no_ivs))
  
  n<-sum(apply(analysisRawData,1,function(x) all(!is.na(x))))
  
  #MAKE NORM DATA STORAGE
  # centre variables on zero
  # this helps with the interaction term
  analysisNormData<-analysisRawData
  for (i in 0:no_ivs) {
    if (!is.factor(analysisNormData[[i+2]]))  
      analysisNormData[[i+2]]=(analysisNormData[[i+2]]-mean(analysisNormData[[i+2]],na.rm=TRUE))
  }
  
  # CREATE FORMULA
  formula<-"dv~iv1"
  if (no_ivs>1)
    for (i in 2:no_ivs) {
      if (AnalysisTerms[2]) formula<-paste(formula,"+iv",i,sep="")
      if (AnalysisTerms[3]) formula<-paste(formula,"+iv1:iv",i,sep="")
    }
  if (any(withins)){
    doingWithin<-TRUE
    formula<-paste(formula,"+(1|participant)",sep="")
    if (all(withins)){
      formula<-paste(formula,"+(1|iv1:participant)+(1|iv2:participant)",sep="")
    }
  } else {
    doingWithin<-FALSE
  }
  
  # SET UP CONTRASTS
  # these are needed to make the anova type 3 work properly
  contrasts<-c()
  for (i in 1:no_ivs) {
    if (catVars[i+1])  {
      nm<-names(contrasts)
      contrasts<-c(contrasts,list(iv=contr.sum))
      names(contrasts)<-c(nm,paste0("iv",i))
    } 
  }
  
  # LINEAR MODELS  
  # get linear model and anova
  if (catVars[1]) {
    if (doingWithin) {
      lmRaw<-glmer(formula=as.formula(formula),data=analysisRawData,family="binomial")
      lmRawC<-glmer(formula=as.formula(formula),data=analysisRawData,family="binomial",contrasts=contrasts)
      lmNormC<-glmer(formula=as.formula(formula),data=analysisNormData,family="binomial",contrasts=contrasts)
      braw.env$anovaMethod<-"Chisq"
    } else {
      lmRaw<-glm(formula=as.formula(formula),data=analysisRawData,family="binomial")
      lmRawC<-glm(formula=as.formula(formula),data=analysisRawData,family="binomial",contrasts=contrasts)
      lmNormC<-glm(formula=as.formula(formula),data=analysisNormData,family="binomial",contrasts=contrasts)
      braw.env$anovaMethod<-"F"
    }
    pcol=3;prow=2
    
  } else { # Interval DV
    # lmRaw to report model
    if (doingWithin) {
      lmRaw<-lmer(formula=as.formula(formula),data=analysisRawData)
      lmRawC<-lmer(formula=as.formula(formula),data=analysisRawData,contrasts=contrasts)
      lmNormC<-lmer(formula=as.formula(formula),data=analysisNormData,contrasts=contrasts)
    } else {
      lmRaw<-lm(formula=as.formula(formula),data=analysisRawData)
      lmRawC<-lm(formula=as.formula(formula),data=analysisRawData,contrasts=contrasts)
      lmNormC<-lm(formula=as.formula(formula),data=analysisNormData,contrasts=contrasts)
    }
    braw.env$anovaMethod<-"F"
    pcol=4;prow=2;
  }
  
  #ANOVAS
  switch (ssqType,
          "Type1"={
            anNormC<-Anova(lmNormC,test=braw.env$anovaMethod)
          },
          "Type2"={
            anNormC<-Anova(lmNormC,test=braw.env$anovaMethod,type=2)
          },
          "Type3"={
            anNormC<-Anova(lmNormC,test=braw.env$anovaMethod,type=3,singular.ok=TRUE)
          }
  )
  
  if (grepl("Intercept",rownames(anNormC)[[1]])) {n1<-2} else {n1<-1}
  n2<-nrow(anNormC)
  if (grepl("Residuals",rownames(anNormC)[[n2]])) {n2<-n2-1}
  df<-anNormC$Df[n1:n2]

  # EFFECT SIZES  
  r.direct<-matrix(model2directeffect(lmNormC),nrow=1)
  if (doingWithin) {
    r.unique<-r.direct
    r.total<-r.direct
  } else {
    r.unique<-matrix(model2uniqueeffect(anNormC)*sign(r.direct),nrow=1)
    r.total<-matrix(model2totaleffect(lmNormC),nrow=1)
  }
  r.full<-model2fulleffect(lmNormC,anNormC)
  AIC<-AIC(lmNormC)
  BIC<-BIC(lmNormC)
  llk<-logLik(lmNormC)

  k<-no_ivs+2
  n_obs<-n
  n_data<-n
  use<-colSums(apply(analysisRawData,1,function(x)!is.na(x)))==ncol(analysisRawData)
  residsNull<-analysisRawData$dv[use]
  residLLK<-sum(log(dnorm(residsNull,mean(residsNull),sd(residsNull))))
  AICnull<-2*2-2*residLLK

  # if (length(r.direct)<3) {
  #   r.direct<-c(r.direct,0)
  #   r.unique<-c(r.unique,0)
  #   r.total<-c(r.total,0)
  #   df<-c(df,1)
  # }

  p.direct<-r2p(r.direct,n,df)
  p.unique<-r2p(r.unique,n,df)
  p.total<-r2p(r.total,n,df)
  
  return(list(r.direct=r.direct,
              r.unique=r.unique,
              r.total=r.total,
              r.full=r.full,
              nval=n,
              llk=llk,
              AIC=AIC,
              BIC=BIC,
              AICnull=AICnull,
              
              p.direct=p.direct,
              p.unique=p.unique,
              p.total=p.total,
              
              lmNormC=lmNormC,
              lmRawC=lmRawC,
              lmRaw=lmRaw,
              
              df=df,
              
              anNormC=anNormC
  ))
}

#' perform an analysis of a sample object
#' 
#' @returns analysis object
#' @examples
#' analysis<-doAnalysis(sample=doSample(),evidence=makeEvidence(),autoShow=braw.env$autoShow)#' make a multiple samples
#' @export
doAnalysis<-function(sample=doSample(autoShow=FALSE),evidence=braw.def$evidence,autoShow=FALSE){

  # if (evidence$AnalysisTerms<2) sample$hypothesis$IV2<-NULL
  
  design<-sample$design
  hypothesis<-sample$hypothesis
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  analysis<-sample

  switch (evidence$Transform,
          "Log"={allData<-data.frame(analysis$participant,log(analysis$dv))},
          "Exp"={allData<-data.frame(analysis$participant,exp(analysis$dv))},
          "None"={allData<-data.frame(analysis$participant,analysis$dv)}
  )
  if (!all(analysis$iv==analysis$iv[1])) 
    allData<-cbind(allData,analysis$iv)
  if (!is.null(IV2) && !all(analysis$iv2==analysis$iv2[1]))
    allData<-cbind(allData,analysis$iv2)
  no_ivs<-ncol(allData)-2
  n<-nrow(allData)

  withins<-c(design$sIV1Use=="Within" && IV$type=="Categorical",design$sIV2Use=="Within" && IV2$type=="Categorical")

  anResult<-generalAnalysis(allData,evidence$AnalysisTerms,withins,evidence$ssqType,evidence$caseOrder)
  
# MOVE RESULTS OUT TO BRAWSTATS  
  r_use<-anResult$r.direct
  p_use<-anResult$p.direct
  analysis$rIV<-r_use[1]
  analysis$pIV<-p_use[1]
  analysis$rIVCI<-r2ci(analysis$rIV,n)
  analysis$pIVCI<-r2p(analysis$rIVCI,n,anResult$df[1])
  if (analysis$rIV>0 && analysis$rIVCI[1]<0 && analysis$rIVCI[2]>0) analysis$pIVCI[1]<-1
  if (analysis$rIV<0 && analysis$rIVCI[1]<0 && analysis$rIVCI[2]>0) analysis$pIVCI[2]<-1
  analysis$rpIV<-sample$effectRho
  ri<-2

  if (!is.null(IV2) && any(evidence$AnalysisTerms[2:3])) {
    if (evidence$AnalysisTerms[2]) {
      analysis$rIV2<-r_use[ri]
      analysis$pIV2<-p_use[ri]
      analysis$rIV2CI<-r2ci(analysis$rIV2,n)
      analysis$pIV2CI<-r2p(analysis$rIV2CI,n,anResult$df[ri])
      if (analysis$rIV2>0 && analysis$rIV2CI[1]<0 && analysis$rIV2CI[2]>0) analysis$pIV2CI[1]<-1
      if (analysis$rIV2<0 && analysis$rIV2CI[1]<0 && analysis$rIV2CI[2]>0) analysis$pIV2CI[2]<-1
      ri<-3
    }
    
    #  interaction term
    if (evidence$AnalysisTerms[3]) {
      analysis$rIVIV2DV<-r_use[ri]
      analysis$pIVIV2DV<-p_use[ri]
      analysis$rIVIV2CI<-r2ci(analysis$rIVIV2DV,n)
      analysis$pIVIV2CI<-r2p(analysis$rIVIV2CI,n,anResult$df[ri])
      if (analysis$rIVIV2DV>0 && analysis$rIVIV2CI[1]<0 && analysis$rIVIV2CI[2]>0) analysis$pIVIV2CI[1]<-1
      if (analysis$rIVIV2DV<0 && analysis$rIVIV2CI[1]<0 && analysis$rIVIV2CI[2]>0) analysis$pIVIV2CI[2]<-1
    } else {
      analysis$rIVIV2DV<-NA
      analysis$pIVIV2DV<-NA
      analysis$rIVIV2CI<-NA
      analysis$pIVIV2CI<-NA
    }
  }  else hypothesis$IV2<-NULL
  analysis$rIVIV2<-0
  
  if (evidence$doSEM) {
    stages<-list(IV$name,IV2$name,DV$name)
    pathmodel<-list(path=
                      list(
                        stages=stages,
                        depth=2,
                        only_ivs=NULL,
                        only_dvs=NULL,
                        within_stage=FALSE,
                        add=NULL,
                        remove=NULL
                      )
    )
    use<-1:(ncol(allData)-1)
    model_data<-list(pid=allData[,1],
                     data=allData[,use+1],
                     varnames=c(DV$name,IV$name,IV2$name)[use],
                     varcat=c(DV$type=="Categorical",IV$type=="Categorical",IV2$type=="Categorical")[use]
    )
    if (!is.null(IV2) && ncol(allData)>3) {
      pathmodel$path$stages<-list(IV$name,IV2$name,DV$name)
      pathmodel$path$depth<-2
      sem6<-fit_sem_model(pathmodel,model_data)
      sem5<-fit_sem_model(pathmodel,model_data,fixedCoeffs=data.frame(v1=IV$name,v2=IV2$name))
      sem4<-fit_sem_model(pathmodel,model_data,fixedCoeffs=data.frame(v1=IV$name,v2=DV$name))
      sem2<-fit_sem_model(pathmodel,model_data,
                          fixedCoeffs=data.frame(v1=c(IV$name,IV$name),v2=c(IV2$name,DV$name)))
      
      pathmodel$path$stages<-list(IV2$name,IV$name,DV$name)
      sem3<-fit_sem_model(pathmodel,model_data,fixedCoeffs=data.frame(v1=IV2$name,v2=DV$name))
      sem1<-fit_sem_model(pathmodel,model_data,
                          fixedCoeffs=data.frame(v1=c(IV2$name,IV2$name),v2=c(IV$name,DV$name)))
      sem0<-fit_sem_model(pathmodel,model_data,
                          fixedCoeffs=data.frame(v1=c(IV2$name,IV2$name,IV$name),v2=c(IV$name,DV$name,DV$name)))
                          
      analysis$semRs<-t(rbind(
        c(sem0$ES_table[DV$name,IV$name],sem0$ES_table[DV$name,IV2$name],sem0$ES_table[IV$name,IV2$name]),
        c(sem1$ES_table[DV$name,IV$name],sem1$ES_table[DV$name,IV2$name],sem1$ES_table[IV$name,IV2$name]),
        c(sem2$ES_table[DV$name,IV$name],sem2$ES_table[DV$name,IV2$name],sem2$ES_table[IV$name,IV2$name]),
        c(sem3$ES_table[DV$name,IV$name],sem3$ES_table[DV$name,IV2$name],sem3$ES_table[IV$name,IV2$name]),
        c(sem4$ES_table[DV$name,IV$name],sem4$ES_table[DV$name,IV2$name],sem4$ES_table[IV2$name,IV$name]),
        c(sem5$ES_table[DV$name,IV$name],sem5$ES_table[DV$name,IV2$name],sem5$ES_table[IV2$name,IV$name]),
        c(sem6$ES_table[DV$name,IV$name],sem6$ES_table[DV$name,IV2$name],sem6$ES_table[IV2$name,IV$name])
      ))
      modelSEMs<-c(sem0$result[[evidence$useAIC]],
                   sem1$result[[evidence$useAIC]],
                   sem2$result[[evidence$useAIC]],
                   sem3$result[[evidence$useAIC]],
                   sem4$result[[evidence$useAIC]],
                   sem5$result[[evidence$useAIC]],
                   sem6$result[[evidence$useAIC]]
      )
      modelSEM1s<-c(sem0$result[[paste0(evidence$useAIC,"1")]],
                   sem1$result[[paste0(evidence$useAIC,"1")]],
                   sem2$result[[paste0(evidence$useAIC,"1")]],
                   sem3$result[[paste0(evidence$useAIC,"1")]],
                   sem4$result[[paste0(evidence$useAIC,"1")]],
                   sem5$result[[paste0(evidence$useAIC,"1")]],
                   sem6$result[[paste0(evidence$useAIC,"1")]]
      )
      analysis$semP<-c(sem0$P,sem1$P,sem2$P,sem3$P,sem4$P,sem5$P,sem6$P)
      analysis$semQ<-c(sem0$Q,sem1$Q,sem2$Q,sem3$Q,sem4$Q,sem5$Q,sem6$Q)
      analysis$semK1<-c(sem0$result$k1,sem1$result$k1,sem2$result$k1,sem3$result$k1,sem4$result$k1,sem5$result$k1,sem6$result$k1)
      analysis$semK<-c(sem0$result$k,sem1$result$k,sem2$result$k,sem3$result$k,sem4$result$k,sem5$result$k,sem6$result$k)
      analysis$semLLR<-c(sem0$result$llr,sem1$result$llr,sem2$result$llr,sem3$result$llr,sem4$result$llr,sem5$result$llr,sem6$result$llr)
      # analysis$semRESID2<-c(sem0$result$resid2,sem1$result$resid2,sem2$result$resid2,sem3$result$resid2,sem4$result$resid2,sem5$result$resid2,sem6$result$resid2)
      analysis$semSRMR<-c(sem0$stats$srmr,sem1$stats$srmr,sem2$stats$srmr,
                           sem3$stats$srmr,sem4$stats$srmr,sem5$stats$srmr,sem6$stats$srmr)
      analysis$semRMSEA<-c(sem0$stats$rmsea,sem1$stats$rmsea,sem2$stats$rmsea,
                           sem3$stats$rmsea,sem4$stats$rmsea,sem5$stats$rmsea,sem6$stats$rmsea)
      analysis$semCHI2<-c(sem0$stats$chisqr,sem1$stats$chisqr,sem2$stats$chisqr,
                          sem3$stats$chisqr,sem4$stats$chisqr,sem5$stats$chisqr,sem6$stats$chisqr)
      analysis$semDF<-c(sem0$stats$chi_df,sem1$stats$chi_df,sem2$stats$chi_df,
                          sem3$stats$chi_df,sem4$stats$chi_df,sem5$stats$chi_df,sem6$stats$chi_df)
    } else {
      pathmodel$path$stages<-list(DV$name)
      sem0<-fit_sem_model(pathmodel,model_data)
      
      pathmodel$path$stages<-list(IV$name,DV$name)
      sem1<-fit_sem_model(pathmodel,model_data)
      
      modelSEMs<-c(sem0$result[[evidence$useAIC]],
                   sem1$result[[evidence$useAIC]],
                   NA,NA,NA,NA,NA
                   )
      analysis$semP<-c(sem0$result$P,sem1$result$P,rep(NA,5))
      analysis$semQ<-c(sem0$result$Q,sem1$result$Q,rep(NA,5))
      analysis$semRs<-t(rbind(c(NA,NA,NA),c(sem1$ES_table[1,2],NA,NA)))
      analysis$semK<-c(sem0$result$k,sem1$result$k,rep(NA,5))
      analysis$semLLR<-c(sem0$result$llr,sem1$result$llr,rep(NA,5))
      # analysis$semRESID2<-c(sem0$result$resid2,sem1$result$resid2,rep(NA,5))
      analysis$semSRMR<-c(sem0$stats$srmr,sem1$stats$srmr,rep(NA,5))
      analysis$semRMSEA<-c(sem0$stats$rmsea,sem1$stats$rmsea,rep(NA,5))
      analysis$semCHI2<-c(sem0$stats$chisqr,sem1$stats$chisqr,rep(NA,5))
      analysis$semDF<-c(sem0$stats$chi_df,sem1$stats$chi_df,rep(NA,5))
    }
    rarrow<-'&#x2192'
    barrow<-'&#x2190&#x2192'
    analysis$sem<-matrix(c(modelSEMs,which.min(modelSEMs)),nrow=1)
    colnames(analysis$sem)<-c("DV",
                              paste0("IV",rarrow,"DV"),
                              paste0("IV2",rarrow,"DV"),
                              paste0("IV2",rarrow,"IV",rarrow,"DV"),
                              paste0("IV",rarrow,"IV2",rarrow,"DV"),
                              paste0("(IV + IV2)",rarrow,"DV"),
                              paste0("(IV" ,barrow, "IV2)",rarrow,"DV"),
                              "Best"
    )
    analysis$sem1<-matrix(c(modelSEM1s,which.min(modelSEM1s)),nrow=1)
    colnames(analysis$sem1)<-c("DV",
                              paste0("IV",rarrow,"DV"),
                              paste0("IV2",rarrow,"DV"),
                              paste0("IV2",rarrow,"IV",rarrow,"DV"),
                              paste0("IV",rarrow,"IV2",rarrow,"DV"),
                              paste0("(IV + IV2)",rarrow,"DV"),
                              paste0("(IV" ,barrow, "IV2)",rarrow,"DV"),
                              "Best"
    )
  } else {
    analysis$sem<-NULL
    analysis$sem1<-NULL
  }
  
  analysis$nval<-n
  analysis$llk<-anResult$llk
  analysis$AIC<-anResult$AIC
  analysis$AICnull<-anResult$AICnull
  analysis$rFull<-anResult$r.full
  analysis$pFull<-r2p(anResult$r.full,n,ncol(anResult$r.direct))
  analysis$rFullse<-r2se(analysis$rFull,n)
  analysis$rFullCI<-r2ci(analysis$rFull,n)
  analysis$wFull<-rn2w(analysis$rFull,n)
  analysis$wFulln80<-rw2n(analysis$rFull,0.8)
  
  iv1<-analysis$iv
  iv2<-analysis$iv2
  dv<-analysis$dv
  # prepare the output to look like Jamovi
  if (any(class(anResult$lmNormC)[1]==c("lmerMod","glmerMod"))) {
    # we need to sort this to match Jamovi etc
    # we use anNormC as the starting point, but replace any Interval predictors
    anRaw<-anResult$anNormC
    if (!is.factor(iv1)) {
      anRaw["iv1",]<-anResult$anNormC["iv1",]
    }
    if (no_ivs>1 && !is.factor(iv1)) {
      anRaw["iv2",]<-anResult$anNormC["iv2",]
    }
    # now we move cells around
    newRow<-nrow(anRaw)+1
    anRaw[newRow,]<-c(NA,anRaw$Df.res[2],0,NA)
    rownames(anRaw)[newRow]<-"Error"
    anRaw["Df.res"]<-NA
    anRaw<-anRaw[,c(3,2,1,4)]
    colnames(anRaw)[1]<-""
    anRaw[,1]<-NA
    anRaw[1,2]<-0 # intercept df
    if ((IV$deploy=="Within") && (no_ivs>1 && IV2$deploy=="Within")) {
      z1<-summary(aov(dv~iv1*iv2+Error(participant/(iv1*iv2)),analysisRawData))
      F<-c(0,
           z1$'Error: participant:iv1'[[1]]$'F value'[1],
           z1$'Error: participant:iv2'[[1]]$'F value'[1],
           z1$'Error: participant:iv1:iv2'[[1]]$'F value'[1],
           NA
      )
      p<-c(0,
           z1$'Error: participant:iv1'[[1]]$'Pr(>F)'[1],
           z1$'Error: participant:iv2'[[1]]$'Pr(>F)'[1],
           z1$'Error: participant:iv1:iv2'[[1]]$'Pr(>F)'[1],
           NA
      )
      anRaw[,3]<-F
      anRaw[,4]<-p
    }
    anResult$anRaw<-anRaw
  }
  
  lmRawC<-anResult$lmRawC
  lmNormC<-anResult$lmNormC
  anNormC<-anResult$anNormC
  
  anRaw<-anNormC
  # simulate the single IV analyses
  if (is.null(IV2)) {
    hypothesisType=paste(IV$type,DV$type,sep=" ")
    switch (hypothesisType,
            "Interval Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-analysis$rIV
            },
            "Ordinal Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-analysis$rIV
            },
            "Categorical Interval"={
              if (IV$ncats==2){
                if (design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  # tv<-t.test(dv~iv1,paired=TRUE,var.equal=!evidence$Welch)
                  # tv<-t.test(dv[as.numeric(iv1)==1],dv[as.numeric(iv1)==2],paired=TRUE,var.equal=!evidence$Welch)
                  tv<-t.test(dv[as.numeric(iv1)==1]-dv[as.numeric(iv1)==2],var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  df<-paste("(",format(tv$parameter),")",sep="")
                  analysis$pIV<-tv$p.value
                } else {
                  an_name<-"t-test: Independent Samples"
                  if (any(c(sum(iv1==levels(iv1)[1]),sum(iv1==levels(iv1)[2]))<3))
                  {             
                    tval<-0
                    analysis$pIV<-1
                    df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
                  } else {
                  tv<-t.test(dv~iv1,var.equal=!evidence$Welch)
                  tval<- -tv$statistic
                  analysis$pIV<-tv$p.value
                  df<-paste("(",format(tv$parameter),")",sep="")
                  }
                }
                t_name<-"t"
                # tval<-sqrt(anRaw$`F value`[2])*sign(analysis$rIV)
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"One-Way ANOVA: Repeated Measures"
                } else {
                  an_name<-"One-Way ANOVA: Independent Measures"
                }
                t_name<-"F"
                if (evidence$Welch) {
                  tv<-oneway.test(dv~iv1, data = analysisRawData, var.equal = FALSE)
                  tval<-tv$statistic
                  df<-paste("(",format(tv$parameter[1]),",",format(tv$parameter[2],digits=3),")",sep="")
                  analysis$pIV<-tv$p.value
                } else {
                  tval<-anRaw$`F value`[2]
                  if (is.null(tval)) {tval<-anRaw$F[2]}
                  df<-paste("(",format(anRaw$Df[2]),",",format(anRaw$Df[3]),")",sep="")
                  analysis$pIV<-anRaw$"Pr(>F)"[2]
                }
              }
            },
            "Interval Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              analysis$pIV<-tv$p.value
            },
            "Ordinal Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              analysis$pIV<-tv$p.value
            },
            "Categorical Ordinal"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Wilcoxon signed-rank Test: Paired Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"T"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,paired=TRUE,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  analysis$pIV<-tv$p.value
                } else {
                  an_name<-"Mann Whitney U test: Independent Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"U"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  analysis$pIV<-tv$p.value
                }
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Friedman Test: Repeated Measures"
                  op <- options(warn = (-1))
                  tv<-friedman(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  analysis$pIV<-tv$p.value
                } else {
                  an_name<-"Kruskal Wallis Test: Independent Measures"
                  op <- options(warn = (-1))
                  tv<-kruskal.test(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  analysis$pIV<-tv$p.value
                }
                df<-paste("(",format(anRaw$Df[2]),",n=",format(length(dv)),")",sep="")
              }
            },
            "Interval Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmNormC$null.deviance-lmNormC$deviance
              analysis$pIV<-1-pchisq(tval,1) # noCases-1
              if (evidence$McFaddens)
              analysis$rIV<-sign(analysis$rIV)*sqrt((lmNormC$null.deviance-lmNormC$deviance)/lmNormC$null.deviance)
            },
            "Ordinal Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmNormC$null.deviance-lmNormC$deviance
              analysis$pIV<-1-pchisq(tval,1) # noCases-1
              if (evidence$McFaddens)
                analysis$rIV<-sign(analysis$rIV)*sqrt((lmNormC$null.deviance-lmNormC$deviance)/lmNormC$null.deviance)
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"

              chiResult<-chisq.test(iv1,dv,correct = FALSE)
              df<-paste("(",format(chiResult$parameter),",","n=",format(length(analysis$participant)),")",sep="")
              
              nhold<-c()
              for (ini in 1:DV$ncats) {
                nhold<-c(nhold,sum(as.numeric(analysis$dv)==ini))
              }
              ncorrection<-(max(nhold)/min(nhold))
              analysis$rIV<-sqrt(unname(chiResult$statistic/n/ncorrection))*sign(analysis$rIV)
              analysis$pIV<-chiResult$p.value
              analysis$rFull<-analysis$rIV
              analysis$rFullse<-r2se(analysis$rFull,n)
              tval<-chiResult$statistic
              if (evidence$McFaddens)
                analysis$rIV<-sign(analysis$rIV)*sqrt((lmNormC$null.deviance-lmNormC$deviance)/lmNormC$null.deviance)
            }
    )
    if (is.na(analysis$pIV)) {analysis$pIV<-1}
  } else {
    switch (DV$type,
            "Interval"={
              an_name<-"General Linear Model"
              t_name<-"F"
              # df<-anResult$anRaw$Df
              tval<-anResult$anRaw$`F value`
            },
            "Ordinal"={
              an_name<-"General Linear Model"
              t_name<-"F"
              # df<-anResult$anRaw$Df
              tval<-anResult$anRaw$`F value`
            },
            "Categorical"={
              an_name<-"Generalized Linear Model"
              t_name<-"chi2"
              # df<-anResult$anRaw$Df
              tval<-anResult$anRaw$Deviance
            }
    )
    df<-anResult$df
    
    analysis$r=list(direct=anResult$r.direct,unique=anResult$r.unique,total=anResult$r.total)
    analysis$rse=list(direct=r2se(anResult$r.direct,n),unique=r2se(anResult$r.unique,n),total=r2se(anResult$r.total,n))
    analysis$p=list(direct=anResult$p.direct,unique=anResult$p.unique,total=anResult$p.total)
  }
  
  # adding fields to existing analysis
  analysis$model<-anResult$lmRaw
  switch (braw.env$modelType,
          "Raw"={
            analysis$anova<-anResult$anRawC
          },
          "Norm"={
            analysis$anova<-anResult$anNormC
          }
  )
  
  
  if (design$sIV1Use=="Within" || design$sIV2Use=="Within") {
    coeffs<-coef(summary(analysis$model))[,1]
    fitted<-fitted(analysis$model)
    residuals<-residuals(analysis$model)
  } else {
    coeffs<-analysis$model$coefficients
    fitted<-analysis$model$fitted
    residuals<-analysis$model$residuals
  }
  
  analysis$coefficients<-coeffs
  analysis$fitted<-fitted
  analysis$residuals<-residuals
  analysis$anova<-as.matrix(analysis$anova[,])
  analysis$model<-data.frame(summary(analysis$model)$coefficients)
  

  analysis$nval<-n
  if (IV$type=="Categorical") {
    analysis$df1<-IV$ncats-1
  } else {
    analysis$df1<-1
  }
  if (no_ivs>1) {
    if (IV2$type=="Categorical") {
      analysis$df2<-IV2$ncats-1
    } else {
      analysis$df2<-1
    }
  } else {
    analysis$df2<-0
  }
  analysis$df12<-analysis$df1*analysis$df2
  
  analysis$an_name<-an_name
  analysis$test_name<-t_name
  analysis$df<-df
  analysis$test_val<-tval
  # analysis$rCalc<-test2effectsize(t_name,tval,analysis$df1,analysis$df)
  
  if (IV$type=="Interval"){
    analysis$iv.mn<-mean(iv1,na.rm=TRUE)
    analysis$iv.sd<-sd(iv1,na.rm=TRUE)
    analysis$iv.sk<-skewness(iv1,na.rm=TRUE)
    analysis$iv.kt<-kurtosis(iv1,na.rm=TRUE)
  } else {
    analysis$iv.mn<-NA
    analysis$iv.sd<-NA
    analysis$iv.sk<-NA
    analysis$iv.kt<-NA
  }
  if (DV$type=="Interval"){
    analysis$dv.mn<-mean(dv,na.rm=TRUE)
    analysis$dv.sd<-sd(dv,na.rm=TRUE)
    analysis$dv.sk<-skewness(dv,na.rm=TRUE)
    analysis$dv.kt<-kurtosis(dv,na.rm=TRUE)
    analysis$er.mn<-mean(residuals,na.rm=TRUE)
    analysis$er.sd<-sd(residuals,na.rm=TRUE)
    analysis$er.sk<-skewness(residuals,na.rm=TRUE)
    analysis$er.kt<-kurtosis(residuals,na.rm=TRUE)
  } else {
    analysis$dv.mn<-NA
    analysis$dv.sd<-NA
    analysis$dv.sk<-NA
    analysis$dv.kt<-NA
    analysis$er.mn<-NA
    analysis$er.sd<-NA
    analysis$er.sk<-NA
    analysis$er.kt<-NA
  }
  
  analysis$hypothesis<-hypothesis
  analysis$design<-design
  analysis$evidence<-evidence
  
  analysis$Heteroscedasticity<-0

  if (autoShow) print(showDescription(analysis))
  # setBrawRes("result",analysis)
  
  return(analysis)
  
}

runSimulation<-function(hypothesis,design,evidence,onlyAnalysis=FALSE,oldResult=NULL,autoShow=FALSE) {
    if (onlyAnalysis && !is.null(oldResult)) {
    res<-doAnalysis(oldResult,evidence,autoShow=FALSE)
    return(res)
  }

  if (!is.null(oldResult) && design$Replication$On) res<-oldResult
  else {
    ntrials<-0
    p_min<-1
    while (1==1) {
      res<-getSample(hypothesis,design,evidence)
      res1<-res
      if (design$sBudgetOn) {
        if (res$pIV<p_min) {
          p_min<-res$pIV
          res1<-res
        } else {
          res<-res1
        }
        ntrials<-ntrials+res$nval
        if (ntrials>=design$sNBudget) {
          break
        }
      }
      if (isSignificant(braw.env$STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) break
      if (runif(1)>evidence$sigOnly) break
    }
  }
  
  # Replication?
  res<-replicateSample(hypothesis,design,evidence,sample,res)

  res
}

getSample<-function(hypothesis,design,evidence) {
  
    if (!evidence$shortHand) {
      sample<-doSample(hypothesis,design,autoShow=FALSE)
      res<-doAnalysis(sample,evidence,autoShow=FALSE)
    } else {
      res<-sampleShortCut(hypothesis,design,evidence,1,FALSE)
    }

  res$ResultHistory<-list(nval=res$nval,df1=res$df1,rIV=res$rIV,rpIV=res$rpIV,pIV=res$pIV,sequence=FALSE)
  # Cheating ?
  res<-cheatSample(hypothesis,design,evidence,sample,res)
  res
}
