reportExplore<-function(exploreResult,Explore_show="EffectSize",
                        Explore_whichShow="All",Explore_typeShow="All"
                        ){
  explore<-exploreResult$explore
  hypothesis<-explore$hypothesis
  effect<-hypothesis$effect
  
  oldAlpha<-BrawOpts$alphaSig
  max_cols<-8
  
  vals<-exploreResult$vals
  if (explore$type=="pNull" && pPlus) vals<-1-vals
  
  if (length(vals)>max_cols)  {
    use<-seq(1,length(vals),2)
  } else {
    use<-1:length(vals)
  }
  nc<-length(use)

  extra_y_label<-Explore_show

  if (is.null(hypothesis$IV2)){
    rVals<-exploreResult$result$rval
    raVals<-exploreResult$result$raval
    pVals<-exploreResult$result$pval
  } else {
    if (Explore_typeShow=="all") {Explore_typeShow<-"direct"}
    if (Explore_whichShow=="All") {Explore_whichShow<-"Main 1"}
    switch (Explore_whichShow,
            "Main 1"={
              rVals<-exploreResult$result$r[[Explore_typeShow]][,,1]
              pVals<-exploreResult$result$p[[Explore_typeShow]][,,1]
              extra_y_label<-paste("Main Effect 1:",Explore_typeShow)
            },
            "Main 2"={
              rVals<-exploreResult$result$r[[Explore_typeShow]][,,1]
              pVals<-exploreResult$result$p[[Explore_typeShow]][,,1]
              extra_y_label<-paste("Main Effect 2:",Explore_typeShow)
            },
            "Interaction"={
              rVals<-exploreResult$result$r[[Explore_typeShow]][,,3]
              pVals<-exploreResult$result$p[[Explore_typeShow]][,,3]
              extra_y_label<-paste("Interaction:",Explore_typeShow)
            }
    )
  }
  nVals<-exploreResult$result$nval
  df1Vals<-exploreResult$result$df1
  
  switch (Explore_show,
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
          "w"={
            showVals<-rn2w(rVals,exploreResult$result$nval)
          },
          "SampleSize"={
            showVals<-exploreResult$result$nval
          },
          "p(sig)"={
            if (explore$type=="Alpha") {
              BrawOpts$alphaSig<-exploreResult$vals
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
            if (explore$type=="Alpha") {
              BrawOpts$alphaSig<-exploreResult$vals
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
              for (i in 1:length(exploreResult$vals)){
                if (explore$type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                sigs<-isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpval[,i]==0
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
              for (i in 1:length(exploreResult$vals)){
                p<-mean(isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
              
              peVals<-exploreResult$nullresult$pval
              reVals<-exploreResult$nullresult$rval
              neVals<-exploreResult$nullresult$nval
              df1eVals<-exploreResult$nullresult$df1
              for (i in 1:length(exploreResult$vals)){
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
              for (i in 1:length(exploreResult$vals)){
                if (explore$type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                sigs<-isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpval[,i]==0
                p<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
            } else {
              for (i in 1:length(exploreResult$vals)){
                if (explore$type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$vals[i]
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
              for (i in 1:length(exploreResult$vals)){
                if (explore$type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$vals[i]
                  BrawOpts$alphaLLR<<-0.5*qnorm(1-BrawOpts$alphaSig/2)^2
                }
                sigs<-isSignificant(BrawOpts$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpval[,i]==0
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
              for (i in 1:length(exploreResult$vals)){
                if (explore$type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$vals[i]
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
              for (i in 1:length(exploreResult$vals)){
                if (explore$type=="Alpha") {
                  BrawOpts$alphaSig<<-exploreResult$vals[i]
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
            ns<-exploreResult$result$nval
            df1<-exploreResult$result$df1
            showVals<-r2llr(rVals,ns,df1,"sLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
          },
          "log(lrd)"={
            ns<-exploreResult$result$nval
            df1<-exploreResult$result$df1
            showVals<-r2llr(rVals,ns,df1,"dLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
          },
          "k"={
            showVals<-exploreResult$result$k
          },
          "pNull"={
            showVals<-exploreResult$result$pnull
          },
          "PDF"={
            showVals<-exploreResult$result$dist==effect$world$populationPDF
            y50<-c()
            y25<-c()
            y75<-c()
            for (i in 1:length(exploreResult$vals)){
              p<-mean(showVals[,i],na.rm=TRUE)
              p_se<-sqrt(p*(1-p)/length(showVals[,i]))
              y50[i]<-p
              y75[i]<-p+p_se*qnorm(0.75)
              y25[i]<-p+p_se*qnorm(0.25)
            }
          },
          "S"={
            showVals<-exploreResult$result$S
            y50<-c()
            y25<-c()
            y75<-c()
          }
          
  )

  if (is.element(Explore_show,c("EffectSize","EffectSizeA","p","w","SampleSize","log(lrs)","log(lrd)","k","pNull","S"))) {
    y75<-c()
    y50<-c()
    y25<-c()
    ymn<-c()
    ysd<-c()
    for (i in 1:length(exploreResult$vals)) {
      y75[i]<-quantile(showVals[,i],0.75,na.rm=TRUE)
      y50[i]<-quantile(showVals[,i],0.50,na.rm=TRUE)
      y25[i]<-quantile(showVals[,i],0.25,na.rm=TRUE)
      ymn[i]<-mean(showVals[,i],na.rm=TRUE)
      ysd[i]<-sd(showVals[,i],na.rm=TRUE)
    }
  }

  outputText<-rep("",nc+1)
  outputText[1]<-"\bExplore:"
  outputText[2]<-explore$type
  outputText[3]<-paste(" (nsims=",format(nrow(exploreResult$result$rval)),")",sep="")
  outputText<-c(outputText,rep("",nc+1))

  if (Explore_show=="NHSTErrors" || Explore_show=="FDR;FMR") {
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
  if (explore$type=="EffectSize" && RZ=="z") {
    vals<-atanh(vals)
  }
  if (explore$type=="EffectSizeA" && RZ=="z") {
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
  
  if (is.element(Explore_show,c("EffectSize","EffectSizeA","p","w","SampleSize","log(lrs)","log(lrd)","k","pNull","S"))) {
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

  if (Explore_show=="NHSTErrors" || Explore_show=="FDR;FMR") {
    switch(Explore_show,
           "NHSTErrors"={extra_y_label<-"Type I errors"},
           "FDR;FMR"={extra_y_label<-"FDR"}
    )
    if (is.null(IV2)){
      rVals<-exploreResult$nullresult$rIVs
      pVals<-exploreResult$nullresult$pIVs
    } else {
      if (Explore_typeShow=="all") {Explore_typeShow<-"direct"}
      if (Explore_whichShow=="All") {Explore_whichShow<-"Main 1"}
      switch (Explore_whichShow,
              "Main 1"={
                rVals<-exploreResult$result$r1[[Explore_typeShow]]
                pVals<-exploreResult$result$p1[[Explore_typeShow]]
                extra_y_label<-paste("Main Effect 1:",Explore_typeShow)
              },
              "Main 2"={
                rVals<-exploreResult$result$r2[[Explore_typeShow]]
                pVals<-exploreResult$result$p2[[Explore_typeShow]]
                extra_y_label<-paste("Main Effect 2:",Explore_typeShow)
              },
              "Interaction"={
                rVals<-exploreResult$result$r3[[Explore_typeShow]]
                pVals<-exploreResult$result$p3[[Explore_typeShow]]
                extra_y_label<-paste("Interaction:",Explore_typeShow)
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
