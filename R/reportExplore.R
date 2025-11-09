#' report the estimated population characteristics from varying parameter
#' 
#' @param showType        "rs","p","n","ws", "p(sig)" \cr
#'                        "NHST", "Hits", "Misses"
#' @return ggplot2 object - and printed
#' @examples
#' showExplore(exploreResult=doExplore(),
#'                        showType="rs",
#'                        whichEffect="All",effectType="all",
#'                        quantileShow=0.5,reportStats="Medians"
#'                        )
#' @export
reportExplore<-function(exploreResult=braw.res$explore,showType="rs",
                        whichEffect="All",effectType="all",
                        quantileShow=0.5,reportStats="Medians",
                        returnDataFrame=FALSE
){
  if (is.null(exploreResult)) exploreResult<-doExplore(autoShow=FALSE)
  
  precision<-braw.env$report_precision
  reportMeans<-(reportStats=="Means")
  reportQuants<-FALSE
  
  if (exploreResult$doingMetaAnalysis) {
    switch(exploreResult$metaAnalysis$analysisType,
           "fixed"={
             showType<-"metaRiv"
             if (exploreResult$metaAnalysis$analyseBias) showType<-paste0(showType,";metaBias")
           },
           "random"={showType<-"metaRiv;metaRsd"},
           {
             if (is.element(exploreResult$hypothesis$effect$world$PDF,c("GenExp","Gamma")))
             showType<-"PDFk;PDFshape"
             else showType<-"PDFk"
             if (exploreResult$metaAnalysis$analyseNulls) showType<-paste0(showType,";metaNulls")
             if (exploreResult$metaAnalysis$analyseBias) showType<-paste0(showType,";metaBias")
           }
           )
    showType<-paste0(showType,";metaSmax")
  }
  
  showType<-strsplit(showType,";")[[1]]
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {showType<-c("rs","p")},
           "Power"=     {showType<-c("ws","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           "DV"= {showType<-c("dv.mn","dv.sd","dv.sk","dv.kt")},
           "Residuals"= {showType<-c("er.mn","er.sd","er.sk","er.kt")},
           {}
    )
  }
  showTypes<-showType
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  design<-exploreResult$design
  evidence<-exploreResult$evidence
  
  oldAlpha<-braw.env$alphaSig
  on.exit(setBrawEnv("alphaSig",oldAlpha),add=TRUE)

  max_cols<-10
  
  vals<-exploreResult$vals
  if (explore$exploreType=="p(R+)") vals<-1-vals
  
  if (length(vals)>max_cols)  {
    useVals<-seq(1,length(vals),2)
  } else {
    useVals<-1:length(vals)
  }
  nc<-length(useVals)+2
  
  if (is.null(hypothesis$IV2))    {
    effectTypes<-"direct"
    whichEffects<-"Main 1"
  } else {
    if (effectType=="all") effectTypes<-c("direct","unique","total")
    else effectTypes<-effectType
    whichEffects<-whichEffect
    if (whichEffect=="All" && sum(evidence$AnalysisTerms)==2) whichEffect<-"Mains"
    if (whichEffect=="All")   {whichEffects<-c("Main 1","Main 2","Interaction")}
    if (whichEffect=="Mains") {whichEffects<-c("Main 1","Main 2")}
    if (whichEffect=="rIV") {whichEffects<-"Main 1"}
    if (whichEffect=="rIV2") {whichEffects<-"Main 2"}
    if (whichEffect=="rIVIV2DV") {whichEffects<-"Interaction"}
  }
  if (showTypes[1]=="SEM") {
    whichEffects<-"Main 1"
    effectTypes<-"direct"
  }
  
  exploreTypeShow<-explore$exploreType
  if (is.element(explore$exploreType,c("rIV","rIV2","rIVIV2","rIVIV2DV"))) {
    if (is.null(hypothesis$IV2)) {
      exploreTypeShow<-"r[p]"
    } else {
      exploreTypeShow<-paste0("r[p]",gsub("^r","",explore$exploreType))
    }
  } else exploreTypeShow<-explore$exploreType
  
  returnData<-data.frame(vals=vals)
  names(returnData)[1]<-explore$exploreType
  
  outputText<-rep("",nc)
  if (is.element(showTypes[1],c("NHST","Hits","Misses")) && sum(!is.na(exploreResult$nullresult$rval[,1]))>0) {
    outputText[1:2]<-c(paste0("!TExplore: ",exploreTypeShow),paste("  nsims = ",brawFormat(exploreResult$count),"+",brawFormat(exploreResult$nullcount),sep=""))
  } else {
    outputText[1:2]<-c(paste0("!TExplore: ",exploreTypeShow),paste("  nsims = ",brawFormat(exploreResult$count),sep=""))
  }
  outputText<-c(outputText,rep("",nc))
  
  
  tableHeader<-FALSE
  for (whichEffect in whichEffects)  {
    for (effectType in effectTypes) {
      if (!tableHeader) {
        outputText<-c(outputText,"!T"," ",exploreTypeShow,rep(" ",nc-3))
        headerText<-c(paste0("!H"),"!D ")
        if (explore$exploreType=="rIV")
          switch(braw.env$RZ,
                 "r"={},
                 "z"={vals<-atanh(vals)}
          )
        for (i in 1:length(useVals)) {
          if (is.numeric(vals[useVals[i]]))
            headerText<-c(headerText,brawFormat(vals[useVals[i]],digits=precision))
          else 
            headerText<-c(headerText,vals[useVals[i]])
        }
        outputText<-c(outputText,headerText)
        tableHeader<-TRUE
      }
      
      if (is.null(hypothesis$IV2)) y_label2<-" "
      else y_label2<-effectType
      
      for (showType in showTypes) {
        y_label<-plotAxis(showType,hypothesis,design,result=exploreResult$result$Smax)$label
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
                  switch(braw.env$RZ,
                         "r"={showVals<-rVals},
                         "z"={showVals<-atanh(rVals)}
                         )
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
                  y50<-colMeans(exploreResult$result$nSig)
                  y51<-colMeans(exploreResult$result$nFP)
                  y75<-apply(exploreResult$result$nSig,1,max)
                  y25<-apply(exploreResult$result$nSig,1,min)
                },
                "AIC"={
                  showVals<-exploreResult$result$AIC-exploreResult$result$AICnull
                  y_label<-"diff(AIC)"
                },
                "NHST"={
                  ng<-2
                  showLabels<-c("sig","ns")
                  if (explore$exploreType=="minRp") evidence$minRp<-exploreResult$vals
                  else evidence$minRp<-rep(evidence$minRp,length(exploreResult$vals))
                  
                  nulls<-abs(exploreResult$result$rpval)<=matrix(evidence$minRp,nrow(exploreResult$result$rpval),ncol(exploreResult$result$rpval),byrow=TRUE)
                  sigs<-nulls*0
                  for (i in 1:length(exploreResult$vals)){
                    if (explore$exploreType=="Alpha") {
                      braw.env$alphaSig<-exploreResult$vals[i]
                    }
                    sigs[,i]<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                  }
                  
                  propsNonNull<-c()
                  propsNull<-c()
                  if (all(nulls) || all(!nulls)) {
                    for (ig in ng:1) propsNonNull<-rbind(propsNonNull,colMeans((sigs==(ig-1))))
                  } else {
                    for (ig in ng:1) propsNull<-rbind(propsNull,colSums((sigs==(ig-1))*nulls)/colSums(nulls | !nulls))
                    for (ig in ng:1) propsNonNull<-rbind(propsNonNull,colSums((sigs==(ig-1))*(!nulls))/colSums(nulls | !nulls))
                  }
                },
                "Hits"={
                  if (explore$exploreType=="minRp") evidence$minRp<-exploreResult$vals
                  else evidence$minRp<-rep(evidence$minRp,length(exploreResult$vals))
                  if (effect$world$On) {
                    for (i in 1:length(exploreResult$vals)){
                      if (explore$exploreType=="Alpha") {
                        braw.env$alphaSig<-exploreResult$vals[i]
                      }
                      sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                      nulls<-abs(exploreResult$result$rpval[,i])<=evidence$minRp[i]
                      p<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                      y50[i]<-1-p
                      y75[i]<-1-p+sqrt(p*(1-p)/length(pVals[,i]))
                      y25[i]<-1-p-sqrt(p*(1-p)/length(pVals[,i]))
                      yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                    }
                  } else {
                    peVals<-exploreResult$nullresult$pval
                    reVals<-exploreResult$nullresult$rval
                    neVals<-exploreResult$nullresult$nval
                    df1eVals<-exploreResult$nullresult$df1
                    for (i in 1:length(exploreResult$vals)){
                      if (explore$exploreType=="Alpha") {
                        braw.env$alphaSig<-exploreResult$vals[i]
                      }
                      p<-mean(isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                      pe<-mean(isSignificant(braw.env$STMethod,peVals[,i],reVals[,i],neVals[,i],df1eVals[,i],exploreResult$evidence),na.rm=TRUE)
                      p<-p/(p+pe)
                      y50[i]<-p
                      y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                      y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                      yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                    }
                  }
                  y_label<-"True Hits"
                },
                "Misses"={
                  if (explore$exploreType=="minRp") evidence$minRp<-exploreResult$vals
                  else evidence$minRp<-rep(evidence$minRp,length(exploreResult$vals))
                  if (effect$world$On) {
                    for (i in 1:length(exploreResult$vals)){
                      if (explore$exploreType=="Alpha") {
                        braw.env$alphaSig<-exploreResult$vals[i]
                      }
                      sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                      nulls<-abs(exploreResult$result$rpval[,i])<=evidence$minRp[i]
                      p<-sum(!sigs & nulls,na.rm=TRUE)/sum(!sigs)
                      y50[i]<-1-p
                      y75[i]<-1-p+sqrt(p*(1-p)/length(pVals[,i]))
                      y25[i]<-1-p-sqrt(p*(1-p)/length(pVals[,i]))
                      yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                    }
                  } else {
                    peVals<-exploreResult$nullresult$pval
                    reVals<-exploreResult$nullresult$rval
                    neVals<-exploreResult$nullresult$nval
                    df1eVals<-exploreResult$nullresult$df1
                    for (i in 1:length(exploreResult$vals)){
                      if (explore$exploreType=="Alpha") {
                        braw.env$alphaSig<-exploreResult$vals[i]
                      }
                      p<-mean(!isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence),na.rm=TRUE)
                      pe<-mean(!isSignificant(braw.env$STMethod,peVals[,i],reVals[,i],neVals[,i],df1eVals[,i],exploreResult$evidence),na.rm=TRUE)
                      p<-p/(p+pe)
                      y50[i]<-p
                      y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                      y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                      yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                    }
                  }
                  y_label<-"False Misses"
                },
                "Inference"={
                  if (explore$exploreType=="minRp") evidence$minRp<-exploreResult$vals
                  else evidence$minRp<-rep(evidence$minRp,length(exploreResult$vals))
                  if (effect$world$On) {
                    for (i in 1:length(exploreResult$vals)){
                      if (explore$exploreType=="Alpha") {
                        braw.env$alphaSig<-exploreResult$vals[i]
                      }
                      sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                      nulls<-abs(exploreResult$result$rpval[,i])<=evidence$minRp[i]
                      p<-sum(sigs & !nulls,na.rm=TRUE)/sum(sigs)
                      y50[i]<-p
                      y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                      y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                      yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                      p<-sum(!sigs & nulls,na.rm=TRUE)/sum(!sigs)
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
                "Source"={
                  if (explore$exploreType=="minRp") evidence$minRp<-exploreResult$vals
                  else evidence$minRp<-rep(evidence$minRp,length(exploreResult$vals))
                  if (effect$world$On) {
                    for (i in 1:length(exploreResult$vals)){
                      if (explore$exploreType=="Alpha") {
                        braw.env$alphaSig<-exploreResult$vals[i]
                      }
                      if (explore$exploreType=="minRp") {
                        evidence$minRp<-exploreResult$vals
                      }
                      sigs<-isSignificant(braw.env$STMethod,pVals[,i],rVals[,i],nVals[,i],df1Vals[,i],exploreResult$evidence)
                      nulls<-abs(exploreResult$result$rpval[,i])<=evidence$minRp[i]
                      p<-sum(sigs & !nulls,na.rm=TRUE)/sum(!nulls)
                      y50[i]<-p
                      y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                      y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                      yiqr[i]<-sqrt(p*(1-p)/length(pVals[,i]))
                      p<-sum(sigs & nulls,na.rm=TRUE)/sum(nulls)
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
                
                "SEM"={
                  rarrow<-'\u2192'
                  barrow<-'\u2190\u2192'
                  showLabels<-c("DV",
                                paste0("IV",rarrow,"DV"),
                                paste0("IV2",rarrow,"DV"),
                                paste0("IV2",rarrow,"IV",rarrow,"DV"),
                                paste0("IV",rarrow,"IV2",rarrow,"DV"),
                                paste0("(IV + IV2)",rarrow,"DV"),
                                paste0("(IV" ,barrow, "IV2)",rarrow,"DV")
                  )
                  if (is.null(exploreResult$hypothesis$IV2)) ng<-2 else ng<-7
                  showLabels<-showLabels[ng:1]
                  
                  nulls<-abs(exploreResult$result$rpval)<=evidence$minRp
                  propsNonNull<-c()
                  propsNull<-c()
                  if (all(nulls) || all(!nulls)) {
                    for (ig in ng:1) propsNonNull<-rbind(propsNonNull,colMeans(exploreResult$result$sem==ig))
                  } else {
                    for (ig in ng:1) propsNull<-rbind(propsNull,colSums((exploreResult$result$sem==ig)*nulls)/colSums(nulls | !nulls))
                    for (ig in ng:1) propsNonNull<-rbind(propsNonNull,colSums((exploreResult$result$sem==ig)*(!nulls))/colSums(nulls | !nulls))
                  }
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
                "metaBias"={
                  showVals<-exploreResult$result$sigOnly
                },
                "metaRiv"={
                  showVals<-exploreResult$result$PDFk
                },
                "metaRsd"={
                  showVals<-exploreResult$result$pRplus
                },
                "mean(R+)"={
                  showVals<-exploreResult$result$PDFk
                },
                "PDFk"={
                  showVals<-exploreResult$result$PDFk
                },
                "PDFshape"={
                  showVals<-exploreResult$result$PDFshape
                },
                "p(R+)"={
                  showVals<-exploreResult$result$pRplus
                },
                "PDF"={
                  showVals<-exploreResult$result$dist==effect$world$PDF
                  for (i in 1:length(exploreResult$vals)){
                    p<-mean(showVals[,i],na.rm=TRUE)
                    p_se<-sqrt(p*(1-p)/length(showVals[,i]))
                    y50[i]<-p
                    y75[i]<-p+p_se*qnorm(0.75)
                    y25[i]<-p+p_se*qnorm(0.25)
                    yiqr[i]<-p_se
                  }
                },
                "metaSmax"={
                  showVals<-exploreResult$result$Smax
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
                "er.mn"={
                  showVals<-exploreResult$result$rs$mn
                },
                "er.sd"={
                  showVals<-exploreResult$result$rs$sd
                },
                "er.sk"={
                  showVals<-exploreResult$result$rs$sk
                },
                "er.kt"={
                  showVals<-exploreResult$result$rs$kt
                }
        )
        if (is.element(showType,c("rs","p","ws","n","AIC","log(lrs)","log(lrd)",
                                  "metaBias","metaRiv","metaRsd","Lambda","p(R+)","metaSmax",
                                  "metaK","metaShape","PDFk","PDFshape",
                                  "iv.mn","iv.sd","iv.sk","iv.kt","dv.mn","dv.sd","dv.sk","dv.kt",
                                  "er.mn","er.sd","er.sk","er.kt"))) {
          quants=(1-quantileShow)/2
          for (i in 1:length(exploreResult$vals)) {
            y75[i]<-quantile(showVals[,i],0.5+quants,na.rm=TRUE)
            y50[i]<-quantile(showVals[,i],0.5,na.rm=TRUE)
            y25[i]<-quantile(showVals[,i],0.5-quants,na.rm=TRUE)
            yiqr[i]<-IQR(showVals[,i],na.rm=TRUE)
            ymn[i]<-mean(showVals[,i],na.rm=TRUE)
            ysd[i]<-sd(showVals[,i],na.rm=TRUE)
          }
          
          oldNames<-names(returnData)
          returnData<-cbind(returnData,data.frame(means=ymn,sds=ysd))
          names(returnData)<-c(oldNames,paste0('mean(',showType,')'),paste0('sd(',showType,')'))
          
          if (reportMeans){
            outputText<-c(outputText,rep(" ",nc))
            outputText<-c(outputText,paste0("\b", y_label),"mean")
            for (i in 1:length(useVals)) {
              outputText<-c(outputText,paste0("!j",brawFormat(ymn[useVals[i]],digits=precision)))
            }
            outputText<-c(outputText,"!U","sd")
            for (i in 1:length(useVals)) {
              outputText<-c(outputText,paste0("!j",brawFormat(ysd[useVals[i]],digits=precision)))
            }
          } else {
            if (reportQuants){
              outputText<-c(outputText,"",paste0("lower ",format(quants*100),"%"))
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
              outputText<-c(outputText,"!U","iqr")
              for (i in 1:length(useVals)) {
                outputText<-c(outputText,paste0("!j",brawFormat(yiqr[useVals[i]],digits=precision)))
              }
            } else {
              outputText<-c(outputText,"!U",paste0("upper ",format(quants*100),"%"))
              for (i in 1:length(useVals)) {
                outputText<-c(outputText,paste0("!j",brawFormat(y75[useVals[i]],digits=precision)))
              }
            }
          }
        }
        
        
        if (is.element(showType,c("p(sig)","n(sig)","Hits","Misses")) ){
          if (returnDataFrame) {
            d<-data.frame(vals=vals,psig=y50)
            names(d)[1]<-explore$exploreType
            return(d)
          }
          
          if (showType=="n(sig)") outputText<-c(outputText,"","!jmin")
          else outputText<-c(outputText,"","-se ")
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(y25[useVals[i]],digits=precision)))
          }
          if (showType=="n(sig)") {
            outputText<-c(outputText," ",paste0("","n(False Discoveries)"))
            for (i in 1:length(useVals)) {
              outputText<-c(outputText,paste0("!j",brawFormat(y51[useVals[i]],digits=precision)))
            }
          }
          if (showType=="n(sig)") outputText<-c(outputText," ",paste0("","n(Significant Results)"))
            else outputText<-c(outputText," ",paste0("",y_label))
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(y50[useVals[i]],digits=precision)))
          }
          if (showType=="n(sig)") outputText<-c(outputText,"!U","!jmax")
          else           outputText<-c(outputText,"!U","+se ")
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(y75[useVals[i]],digits=precision)))
          }
        }
        
        if (is.element(showType,c("Inference","Source")) ){
          switch(showType,
                 "Inference"={
                   y1_label<-"\bSig"
                   y2_label<-"\bNS"
                   yp_label<-"!jcorrect"
                   yn_label<-"!jerror"
                 },
                 "Source"={
                   if (exploreResult$evidence$minRp!=0) {
                     y1_label<-paste0("\b",braw.env$activeTitle)
                     y2_label<-paste0("\b",braw.env$inactiveTitle)
                   } else {
                     y1_label<-paste0("\b",braw.env$nonnullTitle)
                     y2_label<-paste0("\b",braw.env$nullTitle)
                   }
                   yp_label<-"!jsig"
                   yn_label<-"!jns"
                 }
          )
          
          # if (reportQuants){
          #   outputText<-c(outputText,"","-se ")
          #   for (i in 1:length(useVals)) {
          #     outputText<-c(outputText,paste0("!j",brawFormat(y25[useVals[i]],digits=precision)))
          #   }
          # }
          outputText<-c(outputText,y1_label,rep("",nc-1))
          outputText<-c(outputText," ",yp_label)
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(y50[useVals[i]],digits=precision)))
          }
          outputText<-c(outputText," ",yn_label)
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(1-y50[useVals[i]],digits=precision)))
          }
          if (reportQuants) {
            outputText<-c(outputText,"!U","\u00B1se")
            for (i in 1:length(useVals)) {
              outputText<-c(outputText,paste0("!j",brawFormat(yiqr[useVals[i]],digits=precision)))
            }
          } else {
            # outputText<-c(outputText,"!U","+se ")
            # for (i in 1:length(useVals)) {
            #   outputText<-c(outputText,paste0("!j",brawFormat(y75[useVals[i]],digits=precision)))
            # }
          }
          
          if (reportQuants){
            # outputText<-c(outputText,"","-se ")
            # for (i in 1:length(useVals)) {
            #   outputText<-c(outputText,paste0("!j",brawFormat(y25e[useVals[i]],digits=precision)))
            # }
          }
          outputText<-c(outputText,y2_label,rep("",nc-1))
          outputText<-c(outputText," ",yp_label)
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(y50e[useVals[i]],digits=precision)))
          }
          outputText<-c(outputText," ",yn_label)
          for (i in 1:length(useVals)) {
            outputText<-c(outputText,paste0("!j",brawFormat(1-y50e[useVals[i]],digits=precision)))
          }
          if (reportQuants) {
            outputText<-c(outputText,"!U","\u00B1se")
            for (i in 1:length(useVals)) {
              outputText<-c(outputText,paste0("!j",brawFormat(yiqre[useVals[i]],digits=precision)))
            }
          } else {
            # outputText<-c(outputText,"!U","+se ")
            # for (i in 1:length(useVals)) {
            #   outputText<-c(outputText,paste0("!j",brawFormat(y75e[useVals[i]],digits=precision)))
            # }
          }
        }
        if (is.element(showType[1],c("NHST","SEM"))) {
          if (is.null(propsNull)) {
            if (returnDataFrame) {
              d<-data.frame(vals=vals)
              for (ig in 1:nrow(propsNonNull)) {
                d<-cbind(d,propsNonNull[ig,])
                }
              names(d)<-c(explore$exploreType,showLabels)
              return(d)
            }
            
            for (ig in 1:nrow(propsNonNull)) {
              outputText<-c(outputText,paste0("!j",showLabels[ig])," ")
              for (i in useVals)  outputText<-c(outputText,paste0(brawFormat(100*propsNonNull[ig,i],digits=1),"%"))
            }
          } else {
            if (returnDataFrame) {
              d<-data.frame(vals=vals)
              for (ig in 1:nrow(propsNonNull)) {
                d<-cbind(d,propsNonNull[ig,])
              }
              for (ig in 1:nrow(propsNonNull)) {
                d<-cbind(d,propsNull[ig,])
              }
              names(d)<-c(explore$exploreType,paste0("NonNulls",showLabels),paste0("Nulls",showLabels))
              return(d)
            }
            
            outputText<-c(outputText,braw.env$nonnullTitle,rep("",nc-1))
            for (ig in 1:nrow(propsNonNull)) {
              outputText<-c(outputText,paste0("!j",showLabels[ig])," ")
              for (i in useVals)  outputText<-c(outputText,paste0(brawFormat(100*propsNonNull[ig,i],digits=1),"%"))
            }
            outputText<-c(outputText,braw.env$nullTitle,rep("",nc-1))
            for (ig in 1:nrow(propsNull)) {
              outputText<-c(outputText,paste0("!j",showLabels[ig])," ")
              for (i in useVals)  outputText<-c(outputText,paste0(brawFormat(100*propsNull[ig,i],digits=1),"%"))
            }
          }
        }
      }
    }
  }
  
  if (returnDataFrame) {
    return(returnData)
  }
  
  outputText<-c(outputText,rep("",nc))
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
