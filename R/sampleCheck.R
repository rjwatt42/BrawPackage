
cheatSample<-function(hypothesis,design,evidence,sample,result) {
  changeAmount<-1
  CheatingAttempts<-design$sCheatingAttempts

  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  
  if (design$sCheating=="None") return(result)
  if (CheatingAttempts==0) return(result)
  if (isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence)) return(result)

  # fix the hypothesis
  hypothesis$effect$world$worldOn<-FALSE
  hypothesis$effect$rIV<-result$rpIV
  
  if (is.element(design$sCheating,c("Retry"))) {
    ntrials<-0
    switch(design$sCheatingLimit,
           "Fixed"={limit<-CheatingAttempts},
           "Budget"={limit<-design$sCheatingBudget}
           )
    while (!isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<limit) {
      sample<-doSample(hypothesis,design)
      result<-doAnalysis(sample,evidence)
      switch(design$sCheatingLimit,
             "Fixed"={ntrials<-ntrials+1},
             "Budget"={ntrials<-ntrials+result$nval}
      )
    }
    return(result)
  }
  
  if (is.element(design$sCheating,c("Grow","Replace"))) {
    design2<-design
    design2$sN<-CheatingAttempts*changeAmount
    design2$sNRand<-FALSE
    
    effect2<-effect
    
    sample2<-doSample(makeHypothesis(IV,IV2,DV,effect2),design2)
  }

  if (is.element(design$sCheating,c("Prune","Replace"))) {
    ntrials<-0
    while (!isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) 
           && ntrials<CheatingAttempts) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]
        design$sN<-length(sample1$iv)
        result1<-doAnalysis(sample1,evidence)
        ps<-c(ps,result1$pIV)
      }
      switch(design$sCheating,
             "Prune"={
               keep<-ps>min(ps)
               sample$participant<-sample$participant[keep]
               sample$iv<-sample$iv[keep]
               sample$dv<-sample$dv[keep]
               sample$ivplot<-sample$ivplot[keep]
               sample$dvplot<-sample$dvplot[keep]},
             "Replace"={
               change<-which.min(ps)
               sample$iv[change]<-sample2$iv[ntrials+1]
               sample$dv[change]<-sample2$dv[ntrials+1]
               sample$ivplot[change]<-sample2$ivplot[ntrials+1]
               sample$dvplot[change]<-sample2$dvplot[ntrials+1]
             }
      )
      result<-doAnalysis(sample,evidence)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  if (design$sCheating=="Grow") {
    ntrials<-0
    while (!isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) 
           && ntrials<CheatingAttempts*changeAmount) {
      sample$participant<-c(sample$participant,length(sample$participant)+(1:changeAmount))
      sample$iv<-c(sample$iv,sample2$iv[ntrials+(1:changeAmount)])
      sample$dv<-c(sample$dv,sample2$dv[ntrials+(1:changeAmount)])
      sample$ivplot<-c(sample$ivplot,sample2$ivplot[ntrials+(1:changeAmount)])
      sample$dvplot<-c(sample$dvplot,sample2$dvplot[ntrials+(1:changeAmount)])
      design$sN<-design$sN+(1:changeAmount)
      
      result<-doAnalysis(sample,evidence)
      ntrials<-ntrials+changeAmount
    }
    return(result)
  }
  
  
  if (design$sCheating=="Replace") {
    ntrials<-0
    while (!isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<CheatingAttempts) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]

        result1<-doAnalysis(sample1,evidence)
        ps<-c(ps,result1$pIV)
      }
      
      result<-doAnalysis(sample,evidence)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
}

replicationNewN<-function(rs,n,hypothesis,design) {
  Replication<-design$Replication
  if (Replication$PowerOn && (Replication$Power>0)) {
    switch(Replication$PowerPrior,
           "None"={r<-rs},
           "World"={r<-rSamp2Pop(rs,n,hypothesis$effect$world)},
           "Prior"={r<-rSamp2Pop(rs,n,evidence$prior)}
    ) 
    # get the new sample size
    nrep<-rw2n(r,Replication$Power,Replication$Tails)
  } else nrep<-n
  nrep[nrep>Replication$maxN]<-Replication$maxN
  return(nrep)
}

replicateSample<-function(hypothesis,design,evidence,sample,res) {

  oldAlpha<-braw.env$alphaSig
  on.exit(setBrawEnv("alphaSig",oldAlpha),add=TRUE)

  Replication<-design$Replication
  resOriginal<-res
  ResultHistory<-list(nval=res$nval,df1=res$df1,rIV=res$rIV,rpIV=res$rpIV,pIV=res$pIV)
  
  if (Replication$On) {
    # are we asked to start with a significant first result?
    while (Replication$forceSigOriginal && !isSignificant(braw.env$STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
      res<-getSample(hypothesis,design,evidence)
      # if (!evidence$shortHand) {
      #   sample<-doSample(hypothesis,design,autoShow=FALSE)
      #   res<-doAnalysis(sample,evidence,autoShow=FALSE)
      # } else {
      #   res<-sampleShortCut(hypothesis,design,evidence,1,FALSE)
      # }
      resOriginal<-res
      ResultHistory<-list(nval=res$nval,df1=res$df1,rIV=res$rIV,rpIV=res$rpIV,pIV=res$pIV)
    }
    
    braw.env$alphaSig<-Replication$RepAlpha
        
    # now we freeze the population effect size
    hypothesis$effect$rIV<-res$rpIV
    hypothesis$effect$world$worldOn<-FALSE
    # and make the new design
    design1<-design
    design1$sNRand<-FALSE
    if (Replication$BudgetType=="Fixed") {
      Replication$Repeats<-1000
      budgetUse<-res$nval
    }
    
    resPrevious<-res
    # if (Replication$Keep!="MetaAnalysis" && Replication$applySigOriginal && !isSignificant(braw.env$STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence))
    #   Replication$Repeats<-0
    
    if (Replication$Repeats>0)
    for (i in 1:Replication$Repeats) {
      if (Replication$Keep=="Cautious" && !isSignificant(braw.env$STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
        break
      }
      # get the relevant sample effect size for the power calc
      design1$sN<-replicationNewN(res$rIV,res$nval,hypothesis,design)
      if (Replication$BudgetType=="Fixed") {
        design1$sN<-min(design1$sN,Replication$Budget-budgetUse)
      }
      
      # now do the replication
      res<-getSample(hypothesis,design1,evidence)
      # if (!evidence$shortHand) {
      #   sample<-doSample(hypothesis,design1,autoShow=FALSE)
      #   res<-doAnalysis(sample,evidence,autoShow=FALSE)
      # } else {
      #   res<-sampleShortCut(hypothesis,design1,evidence,1,FALSE)
      # }
      # if the result has the wrong sign, 
      if (Replication$forceSign && sign(res$rIV)!=sign(resOriginal$rIV)) {
        res$pIV<-1
        # res$rIV<-0
      }
      # save this result
      ResultHistory$nval<-c(ResultHistory$nval,res$nval)
      ResultHistory$df1<-c(ResultHistory$df1,res$df1)
      ResultHistory$rIV<-c(ResultHistory$rIV,res$rIV)
      ResultHistory$rpIV<-c(ResultHistory$rpIV,res$rpIV)
      ResultHistory$pIV<-c(ResultHistory$pIV,res$pIV)
      
      if (Replication$BudgetType=="Fixed") {
        budgetUse<-budgetUse+res$nval
        if (budgetUse>=Replication$Budget) break;
      }
      
      if (Replication$Keep=="FirstSuccess" && isSignificant(braw.env$STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
        break
      }
      
    }
    # is this result "better" than the previous ones
    if ((Replication$Keep=="LargeN" && res$nval>resPrevious$nval) || 
        (Replication$Keep=="SmallP" && res$pIV<resPrevious$pIV) || 
        Replication$Keep=="Last")
    { resPrevious<-res }
    
    
    if (Replication$Keep=="MetaAnalysis") {
      studies<-list(rIV=ResultHistory$rIV,nval=ResultHistory$nval,df1=ResultHistory$df1,
                    rpIV=ResultHistory$rpIV)
      metaAnalysis<-makeMetaAnalysis(TRUE,analysisType="fixed",
                                     method="MLE",
                                     modelNulls=FALSE,
                                     sourceBias=FALSE,
                                     analyseBias=1/length(studies$rIV))
      metaResult<-runMetaAnalysis(metaAnalysis,studies,hypothesis,metaResult=NULL)
      res$nval<-sum(ResultHistory$nval)
      res$rIV<-sum(ResultHistory$rIV*ResultHistory$nval)/res$nval
      res$rIV<-metaResult$fixed$param1
      res$rpIV<-ResultHistory$rpIV[1]
      res$df1<-ResultHistory$df1[1]
      res$pIV<-rn2p(res$rIV,res$nval)
      ResultHistory$nval<-c(ResultHistory$nval,res$nval)
      ResultHistory$df1<-c(ResultHistory$df1,res$df1)
      ResultHistory$rIV<-c(ResultHistory$rIV,res$rIV)
      ResultHistory$rpIV<-c(ResultHistory$rpIV,res$rpIV)
      ResultHistory$pIV<-c(ResultHistory$pIV,res$pIV)
    } else {
    switch(Replication$Keep,
           "Median"={
             use<-which(ResultHistory$pIV==sort(ResultHistory$pIV)[ceil(length(ResultHistory$pIV)/2)])
             use<-use[1]
           },
           "LargeN"={
             use<-which.max(ResultHistory$nval)
           },
           "SmallP"={
             use<-which.min(ResultHistory$pIV[2:length(ResultHistory$pIV)])
           },
           "Last"={
             use<-length(ResultHistory$pIV)
           },
           "FirstSuccess"={
             use<-length(ResultHistory$pIV)
           },
           "Cautious"={
             sigs<-isSignificant(braw.env$STMethod,ResultHistory$pIV,ResultHistory$rIV,ResultHistory$nval,ResultHistory$df1,evidence)
             if (!any(!sigs)) use<-length(ResultHistory$pIV)
             else            {
               use<-which(!sigs)
               use<-use[1]
             }
           }
    )
    res$rIV<-ResultHistory$rIV[use]
    res$nval<-ResultHistory$nval[use]
    res$df1<-ResultHistory$df1[use]
    res$pIV<-ResultHistory$pIV[use]
    }
  }
  
  res$ResultHistory<-ResultHistory
  res$roIV<-resOriginal$rIV
  res$noval<-resOriginal$nval
  res$df1o<-resOriginal$df1
  res$poIV<-resOriginal$pIV
  return(res)
}
