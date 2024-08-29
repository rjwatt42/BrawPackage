#' report the estimated population characteristics from varying parameter
#' 
#' @param showType        "rs","p","n","ws", "p(sig)" \cr
#'                        "NHST", "Hits", "Misses"
#' @return ggplot2 object - and printed
#' @examples
#' showExplore(exploreResult=doExplore(),
#'                        showType="rs",
#'                        whichEffect="All",effectType="All")
#' @export
reportExplore<-function(exploreResult=braw.res$explore,showType="rs",
                        whichEffect="All",effectType="All"
                        ){
  if (is.null(exploreResult)) exploreResult<-doExplore(autoShow=FALSE)
  
  showType<-strsplit(showType,";")[[1]]
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {showType<-c("rs","p")},
           "Power"=     {showType<-c("ws","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           {}
    )
  }
  showType<-showType[1]
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  
  oldAlpha<-braw.env$alphaSig
  on.exit(braw.env$alphaSig<-oldAlpha)

  max_cols<-8
  
  vals<-exploreResult$vals
  if (explore$exploreType=="pNull" && braw.env$pPlus) vals<-1-vals
  
  if (length(vals)>max_cols)  {
    use<-seq(1,length(vals),2)
  } else {
    use<-1:length(vals)
  }
  nc<-length(use)

  extra_y_label<-showType

  if (is.null(hypothesis$IV2)){
    rVals<-exploreResult$result$rval
    pVals<-exploreResult$result$pval
  } else {
    if (effectType=="all") {effectType<-"direct"}
    if (whichEffect=="All") {whichEffect<-"Main 1"}
    switch (whichEffect,
            "Main 1"={
              rVals<-exploreResult$result$r[[effectType]][,,1]
              pVals<-exploreResult$result$p[[effectType]][,,1]
              extra_y_label<-paste("Main Effect 1:",effectType)
            },
            "Main 2"={
              rVals<-exploreResult$result$r[[effectType]][,,1]
              pVals<-exploreResult$result$p[[effectType]][,,1]
              extra_y_label<-paste("Main Effect 2:",effectType)
            },
            "rIVIV2DV"={
              rVals<-exploreResult$result$r[[effectType]][,,3]
              pVals<-exploreResult$result$p[[effectType]][,,3]
              extra_y_label<-paste("Interaction:",effectType)
            }
    )
  }
  nVals<-exploreResult$result$nval
  df1Vals<-exploreResult$result$df1
  
  switch (showType,
          "rs"={
            showVals<-rVals
            if (braw.env$RZ=="z") showVals<-atanh(showVals)
          },
          "p"={
            showVals<-pVals
          },
          "ws"={
            showVals<-rn2w(rVals,exploreResult$result$nval)
          },
          "n"={
            showVals<-exploreResult$result$nval
          },
          "p(sig)"={
            if (explore$exploreType=="Alpha") {
              braw.env$alphaSig<-exploreResult$vals
            }
            ps<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,braw.env$alphaSig)
            if (ncol(ps)>1) {
              ps<-colMeans(ps)
            }
            y25<-ps-sqrt(ps*(1-ps)/nrow(pVals))
            y50<-ps
            y75<-ps+sqrt(ps*(1-ps)/nrow(pVals))
          },
          "n(sig)"={
            if (explore$exploreType=="Alpha") {
              braw.env$alphaSig<-exploreResult$vals
            }
            ps<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,braw.env$alphaSig)
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
          "NHST"={
            extra_y_label<-"Type II errors"
            y50<-c()
            y25<-c()
            y75<-c()
            y50e<-c()
            y25e<-c()
            y75e<-c()
            if (effect$world$worldOn) {
              for (i in 1:length(exploreResult$vals)){
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals[i]
                }
                sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
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
                p<-mean(!isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
              
              peVals<-exploreResult$nullresult$pval
              reVals<-exploreResult$nullresult$rval
              neVals<-exploreResult$nullresult$nval
              df1eVals<-exploreResult$nullresult$df1
              for (i in 1:length(exploreResult$vals)){
                p<-mean(isSignificant(braw.env$STMethod,peVals[,i],reVals[,i],neVals[,i],df1eVals[,i],exploreResult$evidence),na.rm=TRUE)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
              }
            }
          },
          "Hits"={
            y50<-c()
            y25<-c()
            y75<-c()
            if (effect$world$worldOn) {
              for (i in 1:length(exploreResult$vals)){
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals[i]
                }
                sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                nulls<-exploreResult$result$rpval[,i]==0
                p<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                y50[i]<-1-p
                y75[i]<-1-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-1-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
            } else {
              for (i in 1:length(exploreResult$vals)){
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals[i]
                }
                p<-mean(isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-1-p
                y75[i]<-1-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-1-p-sqrt(p*(1-p)/length(pVals[,i]))
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
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals[i]
                }
                sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
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
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals[i]
                }
                p<-mean(isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
              
              peVals<-exploreResult$nullresult$pIVs
              reVals<-exploreResult$nullresult$rIVs
              neVals<-exploreResult$nullresult$nvals
              df1eVals<-exploreResult$nullresult$df1
              for (i in 1:length(exploreResult$vals)){
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals[i]
                }
                p<-mean(isSignificant(braw.env$STMethod,peVals[,i],reVals[,i],neVals[,i],df1eVals[,i],exploreResult$evidence),na.rm=TRUE)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
              }
            }
            extra_y_label<-"Misses"
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
          "Lambda"={
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

  if (is.element(showType,c("rs","p","ws","n","log(lrs)","log(lrd)","Lambda","pNull","S"))) {
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
  outputText[1]<-paste0("!j\bExplore: ",explore$exploreType)
  outputText[3]<-paste("nsims = ",format(nrow(exploreResult$result$rval)),sep="")

  if (showType=="NHST" || showType=="FDR;FMR") {
    switch (braw.env$STMethod,
            "NHST"={outputText<-c(outputText," ","STMethod: ","NHST")},
            "sLLR"={outputText<-c(outputText," ","STMethod: ","sLLR")},
            "dLLR"={
              outputText<-c(outputText," ","STMethod: ",paste0("dLLR",": ","prior=","(",exploreResult$evidence$prior$populationPDF,")" ))
            }
            )
    outputText<-c(outputText,rep("",nc-2))
  }
  outputText<-c(outputText,rep("",nc+1))
  
  outputText<-c(outputText,paste0("\b", extra_y_label))
  outputText<-c(outputText,rep("",nc))
  
  if (explore$exploreType=="rIV" && braw.env$RZ=="z") {
    vals<-atanh(vals)
  }
  outputText<-c(outputText,paste0("!j\b",explore$exploreType))
  for (i in 1:nc) {
    if (is.numeric(vals[use[i]]))
      outputText<-c(outputText,paste0("!j\b",brawFormat(vals[use[i]],digits=braw.env$report_precision)," "))
    else 
      outputText<-c(outputText,paste0("!j\b",vals[use[i]]," "))
  }

  outputText<-c(outputText,"!j!ilower 25%")
  for (i in 1:nc) {
    outputText<-c(outputText,paste0("!j",brawFormat(y25[use[i]],digits=braw.env$report_precision)))
  }
  outputText<-c(outputText,"!j!i\bmedian")
  for (i in 1:nc) {
    outputText<-c(outputText,paste0("!j",brawFormat(y50[use[i]],digits=braw.env$report_precision)))
  }
  outputText<-c(outputText,"!j!iupper 25%")
  for (i in 1:nc) {
    outputText<-c(outputText,paste0("!j",brawFormat(y75[use[i]],digits=braw.env$report_precision)))
  }

  if (is.element(showType,c("rs","p","ws","n","log(lrs)","log(lrd)","Lambda","pNull","S"))) {
    outputText<-c(outputText,rep(" ",nc+1))
    outputText<-c(outputText,"!j!i\bmean")
    for (i in 1:nc) {
      outputText<-c(outputText,paste0("!j",brawFormat(ymn[use[i]],digits=braw.env$report_precision)))
    }
    outputText<-c(outputText,"!j!isd")
    for (i in 1:nc) {
      outputText<-c(outputText,paste0("!j",brawFormat(ysd[use[i]],digits=braw.env$report_precision)))
    }
  }    

  if (showType=="NHST" || showType=="FDR;FMR") {
    switch(showType,
           "NHST"={extra_y_label<-"Type I errors"},
           "FDR;FMR"={extra_y_label<-"Hits"}
    )
    if (is.null(hypothesis$IV2)){
      rVals<-exploreResult$nullresult$rIVs
      pVals<-exploreResult$nullresult$pIVs
    } else {
      if (effectType=="all") {effectType<-"direct"}
      if (whichEffect=="All") {whichEffect<-"Main 1"}
      switch (whichEffect,
              "Main 1"={
                rVals<-exploreResult$result$r1[[effectType]]
                pVals<-exploreResult$result$p1[[effectType]]
                extra_y_label<-paste("Main Effect 1:",effectType)
              },
              "Main 2"={
                rVals<-exploreResult$result$r2[[effectType]]
                pVals<-exploreResult$result$p2[[effectType]]
                extra_y_label<-paste("Main Effect 2:",effectType)
              },
              "rIVIV2DV"={
                rVals<-exploreResult$result$r3[[effectType]]
                pVals<-exploreResult$result$p3[[effectType]]
                extra_y_label<-paste("Interaction:",effectType)
              }
      )
    }

    outputText<-c(outputText,rep(" ",nc+1))
    outputText<-c(outputText,paste("\b", extra_y_label))
    outputText<-c(outputText,rep("",nc))
    
    outputText<-c(outputText,paste0("!j\b",explore$exploreType))
    for (i in 1:nc) {
      if (is.numeric(vals[use[i]]))
        outputText<-c(outputText,paste0("!j\b",brawFormat(vals[use[i]],digits=braw.env$report_precision)," "))
      else 
        outputText<-c(outputText,paste0("!j\b",vals[use[i]]," "))
    }
    outputText<-c(outputText,"!jlower 25%")
    for (i in 1:nc) {
      outputText<-c(outputText,paste0("!j",brawFormat(y25e[use[i]],digits=braw.env$report_precision)))
    }
    outputText<-c(outputText,"!j\bmedian")
    for (i in 1:nc) {
      outputText<-c(outputText,paste0("!j",brawFormat(y50e[use[i]],digits=braw.env$report_precision)))
    }
    outputText<-c(outputText,"!jupper 25%")
    for (i in 1:nc) {
      outputText<-c(outputText,paste0("!j",brawFormat(y75e[use[i]],digits=braw.env$report_precision)))
    }
  }

  nc=nc+1
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)        

}
