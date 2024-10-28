#' report the estimated population characteristics from varying parameter
#' 
#' @param showType        "rs","p","n","ws", "p(sig)" \cr
#'                        "NHST", "Hits", "Misses"
#' @return ggplot2 object - and printed
#' @examples
#' showExplore(exploreResult=doExplore(),
#'                        showType="rs",
#'                        whichEffect="All",effectType="all")
#' @export
reportExplore<-function(exploreResult=braw.res$explore,showType="rs",
                        whichEffect="All",effectType="all",quantileShow=0.5,report="Medians"
){
  if (is.null(exploreResult)) exploreResult<-doExplore(autoShow=FALSE)
  
  precision<-braw.env$report_precision-1
  
  reportMeans<-(report=="Means")
  reportQuants<-FALSE
  
  showType<-strsplit(showType,";")[[1]]
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {showType<-c("rs","p")},
           "Power"=     {showType<-c("ws","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           "DV"= {showType<-c("dv.mn","dv.sd","dv.sk","dv.kt")},
           "Residuals"= {showType<-c("rs.mn","rs.sd","rs.sk","rs.kt")},
           {}
    )
  }
  showTypes<-showType[1]
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  evidence<-exploreResult$evidence
  
  oldAlpha<-braw.env$alphaSig
  on.exit(braw.env$alphaSig<-oldAlpha)
  
  max_cols<-7
  
  vals<-exploreResult$vals
  if (explore$exploreType=="pNull" && braw.env$pPlus) vals<-1-vals
  
  if (length(vals)>max_cols)  {
    useVals<-seq(1,length(vals),2)
  } else {
    useVals<-1:length(vals)
  }
  nc<-length(useVals)+2

  exploreTypeShow<-explore$exploreType
  if (is.element(explore$exploreType,c("rIV","rIV2","rIVIV2","rIVIV2DV"))) {
    if (is.null(hypothesis$IV2)) {
      exploreTypeShow<-"r[p]"
    } else {
      exploreTypeShow<-paste0("r[p]",gsub("^r","",explore$exploreType))
    }
  } else exploreTypeShow<-explore$exploreType

  outputText<-rep("",nc)
  outputText[1]<-paste0("!TExplore: ")
  outputText[2]<-paste0("nsims = ",format(nrow(exploreResult$result$rval)))
  outputText<-c(outputText,rep("",nc))
  
  
  if (is.null(hypothesis$IV2))    {
    effectTypes<-"direct"
    whichEffects<-"Main 1"
  } else {
    if (effectType=="all") effectTypes<-c("direct","unique","total")
    else effectTypes<-effectType
    whichEffects<-whichEffect
    if (whichEffect=="All" && !evidence$rInteractionOn) whichEffect<-"Mains"
    if (whichEffect=="All")   {whichEffects<-c("Main 1","Main 2","Interaction")}
    if (whichEffect=="Mains") {whichEffects<-c("Main 1","Main 2")}
    if (whichEffect=="rIV") {whichEffects<-"Main 1"}
    if (whichEffect=="rIV2") {whichEffects<-"Main 2"}
    if (whichEffect=="rIVIV2DV") {whichEffects<-"Interaction"}
  }

  tableHeader<-FALSE
  for (whichEffect in whichEffects)  {
    for (effectType in effectTypes) {
      if (!tableHeader) {
        outputText<-c(outputText,rep(" ",2),paste0("!T",exploreTypeShow),rep(" ",nc-3))
        headerText<-c(paste0("!H!C"),"!D ")
        if (explore$exploreType=="rIV" && braw.env$RZ=="z")  vals<-atanh(vals)
        for (i in 1:length(useVals)) {
          if (is.numeric(vals[useVals[i]]))
            headerText<-c(headerText,brawFormat(vals[useVals[i]],digits=precision))
          else 
            headerText<-c(headerText,vals[useVals[i]])
        }
        outputText<-c(outputText,headerText)
        tableHeader<-TRUE
      }
      
      # if (!is.null(hypothesis$IV2))  outputText<-c(outputText,y_label2,rep("",nc-1))
  
      if (is.null(hypothesis$IV2)) y_label2<-" "
      else y_label2<-effectType
      
      for (showType in showTypes) {
        y_label<-showType
    extra_y_label<-NULL
    if (is.null(hypothesis$IV2)){
      rVals<-exploreResult$result$rval
      pVals<-exploreResult$result$pval
    } else {
      switch (whichEffect,
              "Main 1"={
                rVals<-exploreResult$result$r[[effectType]][,,1]
                pVals<-exploreResult$result$p[[effectType]][,,1]
                y_label<-"Main 1"
              },
              "Main 2"={
                rVals<-exploreResult$result$r[[effectType]][,,2]
                pVals<-exploreResult$result$p[[effectType]][,,2]
                y_label<-"Main 2"
              },
              "Interaction"={
                rVals<-exploreResult$result$r[[effectType]][,,3]
                pVals<-exploreResult$result$p[[effectType]][,,3]
                y_label<-"Interaction"
              }
      )
    }
    y_label<-gsub("^([rz]{1})([spoe]{1})$","\\1\\[\\2\\]",y_label)
    
    nVals<-exploreResult$result$nval
    df1Vals<-exploreResult$result$df1
    
    y75<-c()
    y50<-c()
    y25<-c()
    yiqr<-c()
    ymn<-c()
    ysd<-c()
    y50e<-c()
    y25e<-c()
    y75e<-c()
    yiqre<-c()
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
              yiqr<-sqrt(ps*(1-ps)/nrow(pVals))
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
              yiqr<-sqrt(ps*(1-ps)/nrow(pVals))*max(nVals)/colMeans(nVals)
            },
            "NHST"={
              y_label<-"Type II errors"
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
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                  p<-sum(sigs & nulls,na.rm=TRUE)/length(sigs)
                  y50e[i]<-p
                  y75e[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25e[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                  yiqre[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                }
              } else {
                for (i in 1:length(exploreResult$vals)){
                  p<-mean(!isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                  y50[i]<-p
                  y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
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
                  yiqre[i]<-sqrt(p*(1-p)/length(peVals[,i]))
                }
              }
            },
            "Hits"={
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
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
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
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                }
              }
              y_label<-"True Hits"
            },
            "Misses"={
              if (effect$world$worldOn) {
                for (i in 1:length(exploreResult$vals)){
                  if (explore$exploreType=="Alpha") {
                    braw.env$alphaSig<-exploreResult$vals[i]
                  }
                  sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                  nulls<-exploreResult$result$rpval[,i]==0
                  p<-sum(!sigs & nulls,na.rm=TRUE)/sum(!sigs)
                  y50[i]<-1-p
                  y75[i]<-1-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25[i]<-1-p-sqrt(p*(1-p)/length(pVals[,i]))
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                }
              } else {
                for (i in 1:length(exploreResult$vals)){
                  if (explore$exploreType=="Alpha") {
                    braw.env$alphaSig<-exploreResult$vals[i]
                  }
                  p<-mean(!isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                  y50[i]<-1-p
                  y75[i]<-1-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25[i]<-1-p-sqrt(p*(1-p)/length(pVals[,i]))
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                }
              }
              y_label<-"False Misses"
            },
            "FDR;FMR"={
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
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                  p<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                  y50e[i]<-p
                  y75e[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25e[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                  yiqre[i]<-sqrt(p*(1-p)/length(pVals[,i]))
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
                  yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
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
                  yiqre[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                }
              }
              y_label<-"Misses"
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
              for (i in 1:length(exploreResult$vals)){
                p<-mean(showVals[,i],na.rm=TRUE)
                p_se<-sqrt(p*(1-p)/length(showVals[,i]))
                y50[i]<-p
                y75[i]<-p+p_se*qnorm(0.75)
                y25[i]<-p+p_se*qnorm(0.25)
                yiqr[i]<-p_se
              }
            },
            "S"={
              showVals<-exploreResult$result$S
            },
            "iv.mn"={
              showVals<-exploreResult$result$iv$mn
            },
            "iv.sd"={
              showVals<-exploreResult$result$iv$sd
            },
            "iv.sk"={
              showVals<-exploreResult$result$iv$sk
            },
            "iv.kt"={
              showVals<-exploreResult$result$iv$kt
            },
            "dv.mn"={
              showVals<-exploreResult$result$dv$mn
            },
            "dv.sd"={
              showVals<-exploreResult$result$dv$sd
            },
            "dv.sk"={
              showVals<-exploreResult$result$dv$sk
            },
            "dv.kt"={
              showVals<-exploreResult$result$dv$kt
            },
            "rs.mn"={
              showVals<-exploreResult$result$rs$mn
            },
            "rs.sd"={
              showVals<-exploreResult$result$rs$sd
            },
            "rs.sk"={
              showVals<-exploreResult$result$rs$sk
            },
            "rs.kt"={
              showVals<-exploreResult$result$rs$kt
            }
    )
    if (is.element(showType,c("rs","p","ws","n","log(lrs)","log(lrd)","Lambda","pNull","S",
                              "iv.mn","iv.sd","iv.sk","iv.kt","dv.mn","dv.sd","dv.sk","dv.kt",
                              "rs.mn","rs.sd","rs.sk","rs.kt"))) {
      quants=(1-quantileShow)/2
      for (i in 1:length(exploreResult$vals)) {
        y75[i]<-quantile(showVals[,i],0.5+quants,na.rm=TRUE)
        y50[i]<-quantile(showVals[,i],0.5,na.rm=TRUE)
        y25[i]<-quantile(showVals[,i],0.5-quants,na.rm=TRUE)
        yiqr[i]<-IQR(showVals[,i],na.rm=TRUE)
        ymn[i]<-mean(showVals[,i],na.rm=TRUE)
        ysd[i]<-sd(showVals[,i],na.rm=TRUE)
      }
      quantsMade<-TRUE
      label1<-"median"
      label2<-"iqr"
    } else {
      quantsMade<-FALSE
      label1<-" "
      label2<-"\u00B1se"
    }
    
    if (reportMeans){
      if (is.element(showType,c("rs","p","ws","n","log(lrs)","log(lrd)","Lambda","pNull","S"))) {
        outputText<-c(outputText,rep(" ",nc))
        outputText<-c(outputText,"","mean")
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(ymn[useVals[i]],digits=precision)))
        }
        outputText<-c(outputText,"","sd")
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(ysd[useVals[i]],digits=precision)))
        }
      }    
    } else {
      if (reportQuants){
        if (quantsMade) 
          outputText<-c(outputText,"",paste0("lower ",format(quants*100),"%"))
        else
          outputText<-c(outputText,"","-se ")
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(y25[useVals[i]],digits=precision)))
        }
      }
      if (is.null(hypothesis$IV2)) outputText<-c(outputText,paste0("\b", y_label),"median")
      else {
        if (effectType==effectTypes[1]) 
          outputText<-c(outputText,paste0("\b", y_label),y_label2)
        else
          outputText<-c(outputText,"",y_label2)
      }
      for (i in 1:length(useVals)) {
        outputText<-c(outputText,paste0("!j",brawFormat(y50[useVals[i]],digits=precision)))
      }
      if (!reportQuants) {
        outputText<-c(outputText,"",label2)
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(yiqr[useVals[i]],digits=precision)))
        }
      }
      if (reportQuants){
        if (quantsMade) 
          outputText<-c(outputText,"",paste0("upper ",format(quants*100),"%"))
        else
          outputText<-c(outputText,"","+se ")
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(y75[useVals[i]],digits=precision)))
        }
      }
      
    }
    
    
    
    

    if (is.element(showType,c("NHST","FDR;FMR")) ){
      switch(showType,
             "NHST"={extra_y_label<-"\bType I errors"},
             "FDR;FMR"={extra_y_label<-"\bHits"}
      )

      if (reportQuants){
        outputText<-c(outputText,"","-se ")
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(y25e[useVals[i]],digits=precision)))
        }
      }
      outputText<-c(outputText,paste0("",extra_y_label),label1)
      for (i in 1:length(useVals)) {
        outputText<-c(outputText,paste0("!j",brawFormat(y50e[useVals[i]],digits=precision)))
      }
      if (!reportQuants) {
        outputText<-c(outputText,"",label2)
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(yiqre[useVals[i]],digits=precision)))
        }
      }
      if (reportQuants){
        outputText<-c(outputText,"","+se ")
        for (i in 1:length(useVals)) {
          outputText<-c(outputText,paste0("!j",brawFormat(y75e[useVals[i]],digits=precision)))
        }
      }
    }
    }
  }
  }
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
