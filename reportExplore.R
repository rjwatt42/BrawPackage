reportExplore<-function(Iv,IV2,DV,effect,design,explore,exploreResult){
  oldAlpha<-BrawOpts$alphaSig
  max_cols<-8
  
  vals<-exploreResult$result$vals
  if (explore$Explore_type=="pNull" && pPlus) vals<-1-vals
  
  if (length(vals)>max_cols)  {
    use<-seq(1,length(vals),2)
  } else {
    use<-1:length(vals)
  }
  nc<-length(use)

  extra_y_label<-explore$Explore_show

  if (is.null(IV2)){
    rVals<-exploreResult$result$rIVs
    raVals<-exploreResult$result$raIVs
    pVals<-exploreResult$result$pIVs
  } else {
    if (explore$Explore_typeShow=="all") {explore$Explore_typeShow<-"direct"}
    if (explore$Explore_whichShow=="All") {explore$Explore_whichShow<-"Main 1"}
    switch (explore$Explore_whichShow,
            "Main 1"={
              rVals<-exploreResult$result$r1[[explore$Explore_typeShow]]
              pVals<-exploreResult$result$p1[[explore$Explore_typeShow]]
              extra_y_label<-paste("Main Effect 1:",explore$Explore_typeShow)
            },
            "Main 2"={
              rVals<-exploreResult$result$r2[[explore$Explore_typeShow]]
              pVals<-exploreResult$result$p2[[explore$Explore_typeShow]]
              extra_y_label<-paste("Main Effect 2:",explore$Explore_typeShow)
            },
            "Interaction"={
              rVals<-exploreResult$result$r3[[explore$Explore_typeShow]]
              pVals<-exploreResult$result$p3[[explore$Explore_typeShow]]
              extra_y_label<-paste("Interaction:",explore$Explore_typeShow)
            }
    )
  }
  nVals<-exploreResult$result$nvals
  df1Vals<-exploreResult$result$df1vals
  
  switch (explore$Explore_show,
          "EffectSize"={
            showVals<-rVals
            if (RZ=="z") showVals<-atanh(showVals)
          },
          "EffectSizeA"={
            showVals<-raVals
            if (RZ=="z") showVals<-atanh(showVals)
          },
          "p"={
            showVals<-pVals
          },
          "t"={
            showVals<-exploreResult$result$tvals
          },
          "w"={
            showVals<-rn2w(rVals,exploreResult$result$nvals)
          },
          "SampleSize"={
            showVals<-exploreResult$result$nvals
          },
          "p(sig)"={
            if (explore$Explore_type=="Alpha") {
              BrawOpts$alphaSig<-exploreResult$result$vals
            }
            ps<-isSignificant(BrawOpts$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,BrawOpts$alphaSig)
            if (ncol(ps)>1) {
              ps<-colMeans(ps)
            }
            y25<-ps-sqrt(ps*(1-ps)/nrow(pVals))
            y50<-ps
            y75<-ps+sqrt(ps*(1-ps)/nrow(pVals))
          },
          "n(sig)"={
            if (explore$Explore_type=="Alpha") {
              BrawOpts$alphaSig<-exploreResult$result$vals
            }
            ps<-isSignificant(BrawOpts$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,BrawOpts$alphaSig)
            if (ncol(ps)>1) {
              ps<-colMeans(ps)
            }
            y25<-ps-sqrt(ps*(1-ps)/nrow(pVals))
            y50<-ps
            y75<-ps+sqrt(ps*(1-ps)/nrow(pVals))
            y25<-y25*max(nVals)/colMeans(nVals)
            y50<-y50*max(nVals)/colMeans(nVals)
            y75<-y75*max(nVals)/colMeans(nVals)
          },
          "NHSTErrors"={
            extra_y_label<-"Type II errors"
            y50<-c()
            y25<-c()
            y75<-c()
            y50e<-c()
            y25e<-c()
            y75e<-c()
            if (effect$world$worldOn) {
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$result$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                sigs<-isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpIVs[,i]==0
                p<-sum(!sigs & !nulls,na.rm=TRUE)/length(sigs)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                p<-sum(sigs & nulls,na.rm=TRUE)/length(sigs)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
            } else {
              for (i in 1:length(exploreResult$result$vals)){
                p<-mean(isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
              
              peVals<-exploreResult$nullresult$pIVs
              reVals<-exploreResult$nullresult$rIVs
              neVals<-exploreResult$nullresult$nvals
              df1eVals<-exploreResult$nullresult$df1
              for (i in 1:length(exploreResult$result$vals)){
                p<-mean(isSignificant(BrawOpts$STMethod,peVals[,i],reVals[,i],neVals[,i],df1eVals[,i],exploreResult$evidence),na.rm=TRUE)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
              }
            }
          },
          "FDR"={
            y50<-c()
            y25<-c()
            y75<-c()
            if (effect$world$worldOn) {
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$result$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                sigs<-isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpIVs[,i]==0
                p<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
            } else {
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$result$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                p<-mean(isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
            }
          },
          "FDR;FMR"={
            y50<-c()
            y25<-c()
            y75<-c()
            y50e<-c()
            y25e<-c()
            y75e<-c()
            if (effect$world$worldOn) {
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$result$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                sigs<-isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpIVs[,i]==0
                p<-sum(!sigs & !nulls,na.rm=TRUE)/sum(!nulls)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                p<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
            } else {
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$result$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                p<-mean(isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
              
              peVals<-exploreResult$nullresult$pIVs
              reVals<-exploreResult$nullresult$rIVs
              neVals<-exploreResult$nullresult$nvals
              df1eVals<-exploreResult$nullresult$df1
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$result$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                p<-mean(isSignificant(BrawOpts$STMethod,peVals[,i],reVals[,i],neVals[,i],df1eVals[,i],exploreResult$evidence),na.rm=TRUE)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
              }
            }
            extra_y_label<-"FMR"
          },
          "log(lrs)"={
            ns<-exploreResult$result$nvals
            df1<-exploreResult$result$df1
            showVals<-r2llr(rVals,ns,df1,"sLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
          },
          "log(lrd)"={
            ns<-exploreResult$result$nvals
            df1<-exploreResult$result$df1
            showVals<-r2llr(rVals,ns,df1,"dLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
          },
          "k"={
            showVals<-exploreResult$result$ks
          },
          "pNull"={
            showVals<-exploreResult$result$pnulls
          },
          "PDF"={
            showVals<-exploreResult$result$dists==effect$world$populationPDF
            y50<-c()
            y25<-c()
            y75<-c()
            for (i in 1:length(exploreResult$result$vals)){
              p<-mean(showVals[,i],na.rm=TRUE)
              p_se<-sqrt(p*(1-p)/length(showVals[,i]))
              y50[i]<-p
              y75[i]<-p+p_se*qnorm(0.75)
              y25[i]<-p+p_se*qnorm(0.25)
            }
          },
          "S"={
            showVals<-exploreResult$result$Ss
            y50<-c()
            y25<-c()
            y75<-c()
          },
          "mean(DV)"={
            showVals<-exploreResult$result$dvMean
            y50<-c()
            y25<-c()
            y75<-c()
          },
          "sd(DV)"={
            showVals<-exploreResult$result$dvSD
            y50<-c()
            y25<-c()
            y75<-c()
          },
          "skew(DV)"={
            showVals<-exploreResult$result$dvSkew
            y50<-c()
            y25<-c()
            y75<-c()
          },
          "kurtosis(DV)"={
            showVals<-exploreResult$result$dvKurtosis
          }
          
  )

  if (is.element(explore$Explore_show,c("EffectSize","EffectSizeA","p","w","t","SampleSize","log(lrs)","log(lrd)","k","pNull","S","mean(DV)","sd(DV)","skew(DV)","kurtosis(DV)"))) {
    y75<-c()
    y50<-c()
    y25<-c()
    ymn<-c()
    ysd<-c()
    for (i in 1:length(exploreResult$result$vals)) {
      y75[i]<-quantile(showVals[,i],0.75,na.rm=TRUE)
      y50[i]<-quantile(showVals[,i],0.50,na.rm=TRUE)
      y25[i]<-quantile(showVals[,i],0.25,na.rm=TRUE)
      ymn[i]<-mean(showVals[,i],na.rm=TRUE)
      ysd[i]<-sd(showVals[,i],na.rm=TRUE)
    }
  }

  outputText<-rep("",nc+1)
  outputText[1]<-"\bExplore:"
  outputText[2]<-explore$Explore_type
  outputText[3]<-paste(" (nsims=",format(nrow(exploreResult$result$rIVs)),")",sep="")
  outputText<-c(outputText,rep("",nc+1))

  if (explore$Explore_show=="NHSTErrors" || explore$Explore_show=="FDR;FMR") {
    switch (BrawOpts$STMethod,
            "NHST"={outputText<-c(outputText,"NHST")},
            "sLLR"={outputText<-c(outputText,"sLLR")},
            "dLLR"={
              outputText<-c(outputText,paste0("dLLR",": ","prior=",exploreResult$evidence$usePrior,"(",exploreResult$evidence$prior$populationPDF,")" ))
            }
            )
    outputText<-c(outputText,rep("",nc))
  }
  
  outputText<-c(outputText," ")
  if (explore$Explore_type=="EffectSize" && RZ=="z") {
    vals<-atanh(vals)
  }
  if (explore$Explore_type=="EffectSizeA" && RZ=="z") {
    vals<-atanh(vals)
  }
  for (i in 1:nc) {
    outputText<-c(outputText,paste("\b",format(vals[use[i]],digits=report_precision),sep=""))
  }

  outputText<-c(outputText,paste0("!j\b", extra_y_label))
  outputText<-c(outputText,rep("",nc))
  outputText<-c(outputText,"!jlower 25%")
  for (i in 1:nc) {
    outputText<-c(outputText,format(y25[use[i]],digits=report_precision))
  }
  outputText<-c(outputText,"!j\bmedian")
  for (i in 1:nc) {
    outputText<-c(outputText,format(y50[use[i]],digits=report_precision))
  }
  outputText<-c(outputText,"!jupper 25%")
  for (i in 1:nc) {
    outputText<-c(outputText,format(y75[use[i]],digits=report_precision))
  }
  
  if (is.element(explore$Explore_show,c("EffectSize","EffectSizeA","p","w","t","SampleSize","log(lrs)","log(lrd)","k","pNull","S"))) {
    outputText<-c(outputText,rep(" ",nc+1))
    outputText<-c(outputText,"!jmean")
    for (i in 1:nc) {
      outputText<-c(outputText,format(ymn[use[i]],digits=report_precision))
    }
    outputText<-c(outputText,"!jsd")
    for (i in 1:nc) {
      outputText<-c(outputText,format(ysd[use[i]],digits=report_precision))
    }
  }    

  if (explore$Explore_show=="NHSTErrors" || explore$Explore_show=="FDR;FMR") {
    switch(explore$Explore_show,
           "NHSTErrors"={extra_y_label<-"Type I errors"},
           "FDR;FMR"={extra_y_label<-"FDR"}
    )
    if (is.null(IV2)){
      rVals<-exploreResult$nullresult$rIVs
      pVals<-exploreResult$nullresult$pIVs
    } else {
      if (explore$Explore_typeShow=="all") {explore$Explore_typeShow<-"direct"}
      if (explore$Explore_whichShow=="All") {explore$Explore_whichShow<-"Main 1"}
      switch (explore$Explore_whichShow,
              "Main 1"={
                rVals<-exploreResult$result$r1[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p1[[explore$Explore_typeShow]]
                extra_y_label<-paste("Main Effect 1:",explore$Explore_typeShow)
              },
              "Main 2"={
                rVals<-exploreResult$result$r2[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p2[[explore$Explore_typeShow]]
                extra_y_label<-paste("Main Effect 2:",explore$Explore_typeShow)
              },
              "Interaction"={
                rVals<-exploreResult$result$r3[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p3[[explore$Explore_typeShow]]
                extra_y_label<-paste("Interaction:",explore$Explore_typeShow)
              }
      )
    }

    outputText<-c(outputText,paste("!j\b", extra_y_label))
    for (i in 1:nc) {
      outputText<-c(outputText,paste("\b",format(vals[use[i]],digits=report_precision),sep=""))
    }
    outputText<-c(outputText,"!jlower 25%")
    for (i in 1:nc) {
      outputText<-c(outputText,format(y25e[use[i]],digits=report_precision))
    }
    outputText<-c(outputText,"!j\bmedian")
    for (i in 1:nc) {
      outputText<-c(outputText,format(y50e[use[i]],digits=report_precision))
    }
    outputText<-c(outputText,"!jupper 25%")
    for (i in 1:nc) {
      outputText<-c(outputText,format(y75e[use[i]],digits=report_precision))
    }
  }
  
  nc=nc+1
  nr=length(outputText)/nc
  BrawOpts$alphaSig<<-oldAlpha
  
  reportPlot(outputText,nc,nr)        

}
