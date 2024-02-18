
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
    tanh(z+c(-1,1)*zci)
  } else {
    tanh(z+s*zci)
  }
}

res2llr<-function(analysis,method=BrawOpts$STMethod) {
  r2llr(analysis$rIV,analysis$nval,analysis$df1,method,analysis$evidence$llr,analysis$evidence$prior)
}

r2llr<-function(r,n,df1,method=BrawOpts$STMethod,llr=list(e1=c(),e2=0),world=NULL) {
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
    if (is.null(world) || is.null(world$worldOn) || !world$worldOn) {
      world$populationPDF<-"Single"
      world$populationNullp<-0.5
    }
    lk<-getLogLikelihood(z,n,df1,world$populationPDF,world$populationPDFk,worldDistNullP=c(0,1),remove_nonsig=FALSE,doSeparate=TRUE)
    lk1<-lk[,,1]+log(1-world$populationNullp)
    lk2<-lk[,,2]+log(world$populationNullp)
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
      r.full<-r.squaredGLMM(mF)[[1]]
    }
  } else {
    if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
    n2<-nrow(anU)-1
    r.full<-sqrt(sum(anU$`Sum Sq`[n1:n2])/sum(anU$`Sum Sq`))
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

multipleAnalysis<-function(n_sims=1,hypothesis,design,evidence,newResult=c()){
  
  rho<-hypothesis$effect$rIV
  rho2<-hypothesis$effect$rIV2
  
  if (length(rho)<n_sims) {rho<-rep(rho,n_sims)}
  if (!is.null(hypothesis$IV2)) {
    if (length(rho2)<n_sims) {rho2<-rep(rho2,n_sims)}
  }
  
  offset<-sum(!is.na(newResult$rIV))
  for (i in 1:n_sims){
    hypothesis$effect$rIV<-rho[i]
    if (!is.null(hypothesis$IV2)) {hypothesis$effect$rIV2<-rho2[i]}
    
    res<-runSimulation(hypothesis,design,evidence,evidence$sigOnly)
    
    if (is.na(res$rIV)) {
      res$rIV<-0
      res$pIV<-1
      res$nval<-0
    }
    j<-i+offset
    newResult$rIV[j]<-res$rIV
    newResult$raIV[j]<-res$raIV
    newResult$rpIV[j]<-res$rpIV
    newResult$pIV[j]<-res$pIV
    newResult$roIV[j]<-res$roIV
    newResult$poIV[j]<-res$poIV
    newResult$nval[j]<-res$nval
    newResult$df1[j]<-res$df1
    if (!is.null(hypothesis$IV2)) {
      newResult$rIV2[j]<-res$rIV2
      newResult$pIV2[j]<-res$pIV2
      newResult$rIVIV2DV[j]<-res$rIVIV2DV
      newResult$pIVIV2DV[j]<-res$pIVIV2DV
      
      newResult$r$direct[j,]<-res$r$direct
      newResult$r$unique[j,]<-res$r$unique
      newResult$r$total[j,]<-res$r$total
      newResult$p$direct[j,]<-res$p$direct
      newResult$p$unique[j,]<-res$p$unique
      newResult$p$total[j,]<-res$p$total
    }
  }

  newResult$effect<-hypothesis$effect
  newResult$design<-design
  newResult$evidence<-evidence
  newResult
}

convert2Interval<-function(var) {
  var$type<-"Interval"
  var$mu<-var$median
  var$sd<-var$iqr*qnorm(0.75)
}

generalAnalysis<-function(allData,InteractionOn,withins,ssqType="Type3",caseOrder="Alphabetic") {
  
  if (ncol(allData)<3) {
    analysis$rIV<-NA
    analysis$pIV<-NA
    analysis$rpIV<-NA
    return(analysis)
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
              "Frequency"={ref=which.max(tabulate(match(allData[,i], levels(allData[,i]))))}
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
      formula<-paste(formula,"+iv",i,sep="")
      if (InteractionOn==1) formula<-paste(formula,"+iv1:iv",i,sep="")
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
      # lmNorm to calculate effect sizes
      lmNorm<-glmer(formula=as.formula(formula),data=analysisNormData,family="binomial")
      lmNormC<-glmer(formula=as.formula(formula),data=analysisNormData,family="binomial",contrasts=contrasts)
      BrawOpts$STMethod<-"Chisq"
    } else {
      lmRaw<-glm(formula=as.formula(formula),data=analysisRawData,family="binomial")
      lmRawC<-glm(formula=as.formula(formula),data=analysisRawData,family="binomial",contrasts=contrasts)
      lmNorm<-glm(formula=as.formula(formula),data=analysisNormData,family="binomial")
      lmNormC<-glm(formula=as.formula(formula),data=analysisNormData,family="binomial",contrasts=contrasts)
      BrawOpts$STMethod<-"F"
    }
    pcol=3;prow=2
    
  } else { # Interval DV
    # lmRaw to report model
    if (doingWithin) {
      # print(cor(analysisRawData[1:42,4],analysisRawData[43:84,4]))
      lmRaw<-lmer(formula=as.formula(formula),data=analysisRawData)
      lmRawC<-lmer(formula=as.formula(formula),data=analysisRawData,contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-lmer(formula=as.formula(formula),data=analysisNormData)
      lmNormC<-lmer(formula=as.formula(formula),data=analysisNormData,contrasts=contrasts)
    } else {
      lmRaw<-lm(formula=as.formula(formula),data=analysisRawData)
      lmRawC<-lm(formula=as.formula(formula),data=analysisRawData,contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-lm(formula=as.formula(formula),data=analysisNormData)
      lmNormC<-lm(formula=as.formula(formula),data=analysisNormData,contrasts=contrasts)
    }
    BrawOpts$STMethod<-"F"
    pcol=4;prow=2;
  }
  
  #ANOVAS
  switch (ssqType,
          "Type1"={
            anRaw<-Anova(lmRaw,test=BrawOpts$STMethod)
            anRawC<-Anova(lmRawC,test=BrawOpts$STMethod)
            anNorm<-Anova(lmNorm,test=BrawOpts$STMethod)
            anNormC<-Anova(lmNormC,test=BrawOpts$STMethod)
          },
          "Type2"={
            anRaw<-Anova(lmRaw,test=BrawOpts$STMethod,type=2)
            anRawC<-Anova(lmRawC,test=BrawOpts$STMethod,type=2)
            anNorm<-Anova(lmNorm,test=BrawOpts$STMethod,type=2)
            anNormC<-Anova(lmNormC,test=BrawOpts$STMethod,type=2)
          },
          "Type3"={
            anRaw<-Anova(lmRaw,test=BrawOpts$STMethod,type=3,singular.ok=TRUE)
            anRawC<-Anova(lmRawC,test=BrawOpts$STMethod,type=3,singular.ok=TRUE)
            anNorm<-Anova(lmNorm,test=BrawOpts$STMethod,type=3,singular.ok=TRUE)
            anNormC<-Anova(lmNormC,test=BrawOpts$STMethod,type=3,singular.ok=TRUE)
          }
  )
  
  if (grepl("Intercept",rownames(anRaw)[[1]])) {n1<-2} else {n1<-1}
  n2<-nrow(anRaw)
  if (grepl("Residuals",rownames(anRaw)[[n2]])) {n2<-n2-1}
  df<-anRaw$Df[n1:n2]

  # EFFECT SIZES  
  r.direct<-model2directeffect(lmNormC)
  if (doingWithin) {
    r.unique<-r.direct
    r.total<-r.direct
  } else {
    r.unique<-model2uniqueeffect(anNormC)*sign(r.direct)
    r.total<-model2totaleffect(lmNormC)
  }
  r.full<-model2fulleffect(lmNormC,anNormC)
  
  p.direct<-r2p(r.direct,n,df)
  p.unique<-r2p(r.unique,n,df)
  p.total<-r2p(r.total,n,df)
  
  return(list(r.direct=r.direct,
              r.unique=r.unique,
              r.total=r.total,
              r.full=r.full,
              
              p.direct=p.direct,
              p.unique=p.unique,
              p.total=p.total,
              
              lmRaw=lmRaw,
              lmNorm=lmNorm,
              lmRawC=lmRawC,
              lmNormC=lmNormC,
              
              df=df,
              
              anRaw=anRaw,
              anNorm=anNorm,
              anRawC=anRawC,
              anNormC=anNormC
  ))
}

makeAnalysis<-function(evidence=makeEvidence(),sample=makeSample(),autoShow=FALSE){
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

  withins<-c(design$sIV1Use=="Within",design$sIV2Use=="Within")
  
  anResult<-generalAnalysis(allData,evidence$rInteractionOn,withins,evidence$ssqType,evidence$caseOrder)
  
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

  if (no_ivs==2) {
    analysis$rIV2<-r_use[2]
    analysis$pIV2<-p_use[2]
    analysis$rIV2CI<-r2ci(analysis$rIV2,n)
    analysis$pIV2CI<-r2p(analysis$rIV2CI,n,anResult$df[2])
    if (analysis$rIV2>0 && analysis$rIV2CI[1]<0 && analysis$rIV2CI[2]>0) analysis$pIV2CI[1]<-1
    if (analysis$rIV2<0 && analysis$rIV2CI[1]<0 && analysis$rIV2CI[2]>0) analysis$pIV2CI[2]<-1
    
    #  interaction term
    if (evidence$rInteractionOn==1) {
      analysis$rIVIV2DV<-r_use[3]
      analysis$pIVIV2DV<-p_use[3]
      analysis$rIVIV2CI<-r2ci(analysis$rIVIV2DV,n)
      analysis$pIVIV2CI<-r2p(analysis$rIVIV2CI,n,anResult$df[3])
      if (analysis$rIVIV2DV>0 && analysis$rIVIV2CI[1]<0 && analysis$rIVIV2CI[2]>0) analysis$pIVIV2CI[1]<-1
      if (analysis$rIVIV2DV<0 && analysis$rIVIV2CI[1]<0 && analysis$rIVIV2CI[2]>0) analysis$pIVIV2CI[2]<-1
    } else {
      analysis$rIVIV2DV<-NA
      analysis$pIVIV2DV<-NA
      analysis$rIVIV2CI<-NA
      analysis$pIVIV2CI<-NA
    }
  }
  analysis$rIVIV2<-0
  
  analysis$rFull<-anResult$r.full
  analysis$rFullse<-r2se(analysis$rFull,n)
  analysis$rFullCI<-r2ci(analysis$rFull,n)
  analysis$wFull<-rn2w(analysis$rFull,n)
  analysis$wFulln80<-rw2n(analysis$rFull,0.8)
  
  iv1<-analysis$iv
  iv2<-analysis$iv2
  dv<-analysis$dv
  # prepare the output to look like Jamovi
  if (any(class(anResult$lmRaw)[1]==c("lmerMod","glmerMod"))) {
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
  
  lmRaw<-anResult$lmRaw
  lmNorm<-anResult$lmNorm
  lmRawC<-anResult$lmRawC
  lmNormC<-anResult$lmNormC
  
  anRaw<-anResult$anRaw
  anNorm<-anResult$anNorm
  anRawC<-anResult$anRawC
  anNormC<-anResult$anNormC
  
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
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  tv<-t.test(dv~iv1,paired=TRUE,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
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
                  tval<-tv$statistic
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
              tval<-lmRaw$null.deviance-lmRaw$deviance
              analysis$pIV<-1-pchisq(tval,1) # noCases-1
              
            },
            "Ordinal Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmRaw$null.deviance-lmRaw$deviance
              analysis$pIV<-1-pchisq(tval,1) # noCases-1
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"
              print(c(lmRaw$null.deviance,lmRaw$deviance))
              
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
  analysis$rawModel<-anResult$lmRaw
  analysis$normModel<-anResult$lmNorm
  analysis$rawModelC<-anResult$lmRawC
  analysis$normModelC<-anResult$lmNormC
  
  analysis$rawAnova<-anResult$anRaw
  analysis$normAnova<-anResult$anNorm
  analysis$rawAnovaC<-anResult$anRawC
  analysis$normAnovaC<-anResult$anNormC
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
  
  analysis$model<-analysis$rawModel
  analysis$anova<-analysis$rawAnovaC
  
  analysis$an_name<-an_name
  analysis$test_name<-t_name
  analysis$df<-df
  analysis$test_val<-tval
  
  analysis$hypothesis<-hypothesis
  analysis$design<-design
  analysis$evidence<-evidence
  
  # analysis$ResultHistory<-ResultHistory
  
  analysis$Heteroscedasticity<-0
  F<-analysis$rawAnova$F[[2]]
  df2<-analysis$rawAnova$Df[[3]]
  analysis$raIV<-sqrt(F/(F+df2))*sign(analysis$rIV)
  
  
  if (autoShow) print(showDescription(analysis))
  
  analysis
  
}

runSimulation<-function(hypothesis,design,evidence,sig_only=FALSE,onlyAnalysis=FALSE,oldResult=NULL) {
    if (onlyAnalysis && !is.null(oldResult)) {
    res<-makeAnalysis(evidence,oldResult)
    return(res)
  }
  
  ntrials<-0
  p_min<-1
  while (1==1) {
    if (!evidence$shortHand) {
      # sample<-makeSample(IV,IV2,DV,effect,design)
      # res<-makeAnalysis(IV,IV2,DV,effect,design,evidence,sample)
      res<-getSample(hypothesis,design,evidence)
    } else {
      res<-sampleShortCut(hypothesis,design,evidence,1,FALSE)
    }
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
    } else {
      break
    }
  }
  
  # sig only
  while (sig_only && !isSignificant(BrawOpts$STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
    if (!evidence$shortHand) {
      # sample<-makeSample(IV,IV2,DV,effect,design)
      # res<-makeAnalysis(IV,IV2,DV,effect,design,evidence,sample)
      res<-getSample(hypothesis,design,evidence)
    } else {
      res<-sampleShortCut(hypothesis,design,evidence,1,FALSE)
    }
  }
  # # Cheating ?
  # res<-cheatSample(IV,IV2,DV,effect,design,evidence,sample,res)
  # Replication?
  res<-replicateSample(hypothesis,design,evidence,sample,res)
  
  F<-res$rawAnova$F[[2]]
  df2<-res$rawAnova$Df[[3]]
  res$raIV<-sqrt(F/(F+df2))*sign(res$rIV)
  
  res
  
}

getSample<-function(hypothesis,design,evidence) {
  if (!evidence$shortHand) {
    sample<-makeSample(hypothesis,design)
    res<-makeAnalysis(evidence,sample)
  } else {
    res<-sampleShortCut(hypothesis,design,evidence,1,FALSE)
  }
  # Cheating ?
  res<-cheatSample(hypothesis,design,evidence,sample,res)
  res
}
