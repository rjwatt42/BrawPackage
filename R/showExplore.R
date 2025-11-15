drawNHSTBar<-function(i,npts,pts1,bwidth,col1) {
  barx<-c(-1,-1,1,1)*bwidth
  bary<-c(i,npts*2-i+1,npts*2-i+1,i)
  
  y1<-pts1$y[bary]
  x1<-pts1$x[i]+barx
  pts<-data.frame(x=x1,y=y1)
  dataPolygon(data=pts,fill=col1)
}
drawNHSTLine<-function(i,npts,pts1,bwidth,linewidth=1) {
  barx<-c(-1,1)*bwidth
  bary<-c(i,i)
  
  y1<-pts1$y[bary]
  x1<-pts1$x[i]+barx
  pts<-data.frame(x=x1,y=y1)
  dataLine(data=pts,linewidth=linewidth)
}
drawNHSTLabel<-function(lb1,lb1xy,xoff,col1,vjust=NULL) {
  if (sum(col2rgb(col1))>128*3) col<-"#000000" else col<-"white"
  lb1xy$x<-lb1xy$x+xoff
  if (is.null(vjust))
    if (lb1xy$y>0.5) vjust<-1 else vjust<-0
  dataLabel(data=lb1xy,label=lb1,
            hjust=0,vjust=vjust,
            size=0.6,
            fill=col1,colour=col)
}


trimExploreResult<-function(result,nullresult) {
  
  result$rval<-rbind(result$rval,nullresult$rval)
  result$pval<-rbind(result$pval,nullresult$pval)
  result$rpval<-rbind(result$rpval,nullresult$rpval)
  result$raval<-rbind(result$raval,nullresult$raval)
  result$roval<-rbind(result$roval,nullresult$roval)
  result$poval<-rbind(result$poval,nullresult$poval)
  result$nval<-rbind(result$nval,nullresult$nval)
  result$df1<-rbind(result$df1,nullresult$df1)
  
  use<- !is.na(result$rval[,1])
  nr=sum(use)
  nc=ncol(result$rval)
  
  result$rval =matrix(result$rval[use,],nrow=nr,ncol=nc)
  result$pval =matrix(result$pval[use,],nrow=nr,ncol=nc)
  result$rpval=matrix(result$rpval[use,],nrow=nr,ncol=nc)
  result$raval=matrix(result$raval[use,],nrow=nr,ncol=nc)
  result$roval=matrix(result$roval[use,],nrow=nr,ncol=nc)
  result$poval=matrix(result$poval[use,],nrow=nr,ncol=nc)
  result$nval =matrix(result$nval[use,],nrow=nr,ncol=nc)
  result$df1  =matrix(result$df1[use,],nrow=nr,ncol=nc)
  
  return(result)
}

#' show the estimated population characteristics from varying parameter
#' 
#' @param showType        "Basic","p(sig)","NHST", "Hits","Misses" \cr
#'  or one or two of: "rs","p","wp","ws","nw" eg "p;ws"
#' @return ggplot2 object - and printed
#' @examples
#' showExplore(exploreResult=doExplore(),
#'                        showType="Basic",dimension="1D",
#'                        effectType="unique",whichEffect="All",
#'                        quantileShow=0.5,fixedYlim=TRUE,showHist=TRUE)
#' @export
showExplore<-function(exploreResult=braw.res$explore,showType="Basic",dimension="1D",showTheory=FALSE,
                      effectType="unique",whichEffect="All",quantileShow=0.5,
                      fixedYlim=braw.env$fixedYlim,showHist=FALSE,fixNulls=TRUE){

# do we need more simulations  
  if (is.null(exploreResult)) exploreResult=doExplore()
  if (!exploreResult$hypothesis$effect$world$On && is.element(showType[1],c("NHST","Inference","Source","Hits","Misses"))) {
    if (exploreResult$nullcount<exploreResult$count) {
      exploreResult<-doExplore(0,exploreResult,doingNull=TRUE)
    }
  }
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  design<-exploreResult$design
  evidence<-exploreResult$evidence
  
# sort out what we are showing
  
  if (is.element(showType[1],c("NHST","Inference","Source","Hits","Misses","p(sig)","n(sig)","n(fd)"))) showHist<-FALSE
  if (exploreResult$count>50) showHist<-FALSE
  
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
  if (exploreResult$doingMetaAnalysis) {
    switch(exploreResult$metaAnalysis$analysisType,
           "none"={
             showType<-"n(sig)"
             showHist<-FALSE
           },
           "fixed"={
             if (is.null(showType))
               showType<-c("metaRiv")
             if (exploreResult$metaAnalysis$analyseBias) showType<-c(showType,"metaBias")
             },
           "random"={
             if (is.null(showType))
               showType<-c("metaRiv","metaRsd")
             if (exploreResult$metaAnalysis$analysisVar=="var") showType[2]<-"LambdaRn"
               },
           {
             if (is.null(showType)) {
               if (exploreResult$metaAnalysis$analyseNulls) 
                 showType<-c("mean(R+)","p(R+)")
               else showType<-c("mean(R+)")
             }
            if (exploreResult$hypothesis$effect$world$On)
              if (is.element(exploreResult$hypothesis$effect$world$PDF,c("GenExp","Gamma")))
                showType<-c("PDFk","PDFshape")
               }
           )
  }
  
  if (length(showType)>1 && showType[2]==" ") showType<-showType[1]
  
  if (length(showType)==2 && dimension=="2D") {
    g<-showExplore2D(exploreResult=exploreResult,showType=showType,showTheory=showTheory,
                     effectType=effectType,whichEffect=whichEffect)
    return(g)
  }
  
  quants<-quantileShow/2
  showPower<-TRUE # show power calculations?
  showPowerR<-FALSE # show power calculations?
  
  oldAlpha<-braw.env$alphaSig
  on.exit(setBrawEnv("alphaSig",oldAlpha),add=TRUE)

  vals<-exploreResult$vals
  if (explore$exploreType=="rIV")
    switch(braw.env$RZ,
           "r"={},
           "z"={vals<-atanh(vals)}
    )

  if (showType[1]=="SEM") whichEffect<-"Main 1"
  if (whichEffect=="All" && sum(evidence$AnalysisTerms)<3) whichEffect<-"Mains"
  if ((whichEffect=="All" || whichEffect=="Mains") && is.null(hypothesis$IV2)) whichEffect<-"Main 1"
  
# dimensions & position of graph area  
  plotYOffset<-matrix(0)
  plotHeight<-1
  if (!is.null(hypothesis$IV2)) {
    switch(whichEffect,
           "All"={
             plotYOffset<-matrix(c(0.5,0.5,0),nrow=1,byrow=TRUE)
             plotHeight<-0.47
           },
           "Mains"={
             plotYOffset<-matrix(c(0.5,0),nrow=1,byrow=TRUE)
             plotHeight<-0.47
           },
           {}
    )
  } else {
    if (length(showType)==2) {
      plotYOffset<-matrix(c(0,0),nrow=1,byrow=TRUE)
    }
    if (length(showType)==4) {
      plotYOffset<-matrix(c(0.5,0.5, 0, 0),nrow=1,byrow=TRUE)
      plotHeight<-0.475
    }
  }
  
  plotXOffset<-matrix(0)
  plotWidth<-1
  if (length(showType)==2) {
    plotXOffset<-matrix(c(0,0.55),nrow=2,byrow=FALSE)
    plotWidth<-0.45
  }
  if (length(showType)==4) {
    plotXOffset<-matrix(c(0,  0.55, 0, 0.55),nrow=4,byrow=FALSE)
    plotWidth<-0.45
  }
  
  if (length(showType)==1 && whichEffect=="All"){
    plotXOffset<-matrix(c(0.0,0.5,0.25),nrow=3,byrow=FALSE)
    plotWidth<-0.45
    plotYOffset<-matrix(c(0.5,0.5,0),nrow=1,byrow=TRUE)
    plotHeight<-0.475
  }
  
  if (length(showType)==1 && whichEffect=="Mains"){
    plotXOffset<-matrix(c(0,0.55),nrow=2,byrow=FALSE)
    plotWidth<-0.45
    plotYOffset<-matrix(c(0,0),nrow=1,byrow=TRUE)
    plotHeight<-0.475
  }
  
# which effects are we showing (main1, main2, interaction)?
  if (is.null(hypothesis$IV2)) whichEffects<-1
  else
    switch (whichEffect,
            "Main 1"=whichEffects<-1,
            "Main 2"=whichEffects<-2,
            "Interaction"=whichEffects<-3,
            "rIV"=whichEffects<-1,
            "rIV2"=whichEffects<-2,
            "rIVIV2DV"=whichEffects<-3,
            "Mains"=whichEffects<-1:2,
            "All"=whichEffects<-1:3
    )
  # and effect types
  if (effectType=="all") {
    if (!is.null(hypothesis$IV2)) effectTypes<-c("direct","unique","total")
    else effectTypes<-"direct"
  } 
  else effectTypes<-effectType
  
# prepare the x-axis  
  if (is.character(vals[1]) || length(vals)<=10 || (length(vals)>=25 && exploreResult$count>500/length(vals))) {
    if (is.character(vals[1])) {
      vals<-1:length(vals)
      xlim<-c(0,length(vals)+1)
      xbreaks<-vals
      xnames<-exploreResult$vals
    } else {
      if (explore$xlog) vals<-log10(vals)
      xlim<-c(min(vals),max(vals))
      valsRange<-diff(xlim)
      xlim<-xlim+c(-1,1)*valsRange/10
      xbreaks<-NULL
      xnames<-NULL
    }
    doLine=FALSE
  } else {
    if (explore$xlog) vals<-log10(vals)
    xlim<-c(min(vals),max(vals))
    valsRange<-diff(xlim)
    xlim<-xlim+c(-1,1)*valsRange/10
    xbreaks<-NULL
    xnames<-NULL
    doLine=TRUE
  }
  if (showHist) doLine<-FALSE
  if (!doLine)  quants<-0.95/2
  
  g<-NULL
  
  for (si in 1:length(showType)) {
    
  yaxis<-plotAxis(showType[si],hypothesis,design,result=exploreResult$result$Smax)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ycols<-yaxis$cols
  ylines<-yaxis$lines
  ySecond<-NULL
  
  if (is.element(showType[si],c("p(sig)","p(w80)","NHST","Inference","Source","Hits","Misses","SEM"))) 
    ylim<-c(0,1)
  if (showType[si]=="AIC") {
    ylim<-c(-1,0.1)*design$sN*hypothesis$DV$sd
    ylabel<-"diff(AIC)"
  }
  if (showType[si]=="p" && braw.env$pPlotScale=="log10" && any(exploreResult$result$pval>0)) {
    ylim<-c(-4,0)  
    while (mean(log10(exploreResult$result$pval)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
    fixedYlim<-FALSE
  }
  
  col2<-desat(braw.env$plotColours$infer_nsigNonNull,0.5)
  col3<-braw.env$plotColours$infer_nsigNull
  col5<-braw.env$plotColours$infer_sigNull
  switch (braw.env$STMethod,
          "NHST"={
            col1<-NA
            col0<-braw.env$plotColours$infer_sigNonNull
            col4<-NA
          },
          "sLLR"={
            col1<-NA
            col0<-braw.env$plotColours$infer_nsigNonNull
            col4<-NA
          },
          "dLLR"={
            col1<-braw.env$plotColours$infer_isigNonNull
            col0<-braw.env$plotColours$infer_nsdNonNull
            col4<-braw.env$plotColours$infer_isigNull
          }
  )
  lb0<-braw.env$nonNullPositive
  lb1<-braw.env$nonNullNegative
  lb2<-braw.env$nonNullNS
  if (evidence$minRp!=0) lb3<-braw.env$ignoreNS else lb3<-braw.env$nullNS
  lb4<-braw.env$nullNegative
  lb5<-braw.env$nullPositive
  if (braw.env$STMethod=="NHST") {
    lb0<-braw.env$nonNullSig
    if (evidence$minRp!=0) lb5<-braw.env$ignoreSig else lb5<-braw.env$nullSig
  }
  
  exploreTypeShow<-explore$exploreType
  if (is.element(explore$exploreType,c("rIV","rIV2","rIVIV2","rIVIV2DV"))) {
    if (is.null(hypothesis$IV2)) {
      exploreTypeShow<-"r[p]"
    } else {
      exploreTypeShow<-paste0("r[p]~",gsub("^r","",explore$exploreType))
    }
  } else {
    if (explore$exploreType=="minRp") exploreTypeShow<-"min(r[p])"
    if (explore$exploreType=="p(R+)") exploreTypeShow<-braw.env$Plabel
    if (explore$exploreType=="mean(R+)") exploreTypeShow<-braw.env$Llabel
    if (explore$exploreType=="PDFk") exploreTypeShow<-"PDF[k]"
    if (explore$exploreType=="PDFshape") exploreTypeShow<-"PDF[shape]"
  }
    
  for (whichEffect in whichEffects) {
    yi<-which(whichEffect == whichEffects)
    if (length(showType)==1 && !is.null(hypothesis$IV2) && showType[1]!="SEM")  {
      useLabel<-c("Main 1","Main 2","Interaction")[whichEffect]
    } else {
      useLabel<-""
    }
    if (length(whichEffects)==1)
      braw.env$plotArea<-c(plotXOffset[si,1],plotYOffset[1,si],plotWidth,plotHeight)
    else {
      if (length(showType)==1 && is.element(length(whichEffects),c(2,3)))
        braw.env$plotArea<-c(plotXOffset[yi,1],plotYOffset[1,yi],plotWidth,plotHeight)
      else
        braw.env$plotArea<-c(plotXOffset[si,1],plotYOffset[1,yi],plotWidth,plotHeight)
    }
    if ((showType[si]=="rs") && (!is.null(hypothesis$IV2))) 
      switch(whichEffect,ylabel<-"Main 1",ylabel<-"Main 2",ylabel<-"Interaction")

    col<-darken(ycols[1],off=-0.2)
    col<-ycols[1]
    for (effectType in effectTypes) {
      switch(effectType,
             "direct"={},
             "unique"={col<-darken(desat(col,0.7),1.3)},
             "total"={col<-darken(desat(col,0.7),0.7)}
      )
      
    theoryVals<-NULL
    theoryUpper<-NULL
    theoryLower<-NULL
    theoryVals0<-c()
    theoryVals1<-c()
    theoryVals2<-c()
    if (showTheory) {
      # if (!hypothesis$effect$world$On)
      #   hypothesis$effect$world<-list(On=TRUE,PDF="Single",RZ="r",PDFk=hypothesis$effect$rIV,pRplus<-1)
      n75=qnorm(0.75)
      newvals<-seq(min(vals),max(vals),length.out=51)
      if (explore$xlog) newvals<-10^newvals
      alphas<-braw.env$alphaSig
      if (hypothesis$effect$world$On)  rVal<-hypothesis$effect$world$PDFk
      else                                  rVal<-hypothesis$effect$rIV
      switch(explore$exploreType,
             "rIV"={
               rVals<-newvals
               nVals<-rep(design$sN,length(newvals))
               alphas<-rep(braw.env$alphaSig,length(newvals))
               rplusPs<-rep(hypothesis$effect$world$pRplus,length(newvals))
             },
             "n"={
               rVals<-rep(rVal,length(newvals))
               nVals<-newvals
               alphas<-rep(braw.env$alphaSig,length(newvals))
               rplusPs<-rep(hypothesis$effect$world$pRplus,length(newvals))
             },
             "Alpha"={
               rVals<-rep(rVal,length(newvals))
               nVals<-rep(design$sN,length(newvals))
               alphas<-newvals
               rplusPs<-rep(hypothesis$effect$world$pRplus,length(newvals))
             },
             "p(R+)"={
               rVals<-rep(rVal,length(newvals))
               nVals<-rep(design$sN,length(newvals))
               alphas<-rep(braw.env$alphaSig,length(newvals))
               rplusPs<-newvals
             },
             {
               rVals<-rep(rVal,length(newvals))
               nVals<-rep(design$sN,length(newvals))
               alphas<-rep(braw.env$alphaSig,length(newvals))
               rplusPs<-rep(hypothesis$effect$world$pRplus,length(newvals))
             }
      )
      basenpts<-51
      if (is.element(showType[si],c("rs","p","ws","wp","nw"))) {
        switch(showType[si],
               "rs"={
                 basevals<-seq(-1,1,length.out=basenpts)*braw.env$r_range
                 logScale<-FALSE
               },
               "p"={
                 logScale<-braw.env$pPlotScale=="log10"
                 if (logScale) {
                   basevals<-seq(0,ylim[1]-3,length.out=basenpts)
                   basevals<-10^basevals
                 } else {
                   basevals<-seq(1,0,length.out=basenpts)
                 }
               },
               "ws"={
                 basevals<-seq(braw.env$alphaSig*1.01,1/1.01,length.out=basenpts)
                 logScale<-FALSE
               },
               "wp"={
                 basevals<-seq(braw.env$alphaSig*1.01,1/1.01,length.out=basenpts)
                 logScale<-FALSE
               },
               "nw"={ 
                 logScale<-braw.env$nPlotScale=="log10"
                 if (logScale) {
                   basevals<-seq(log10(5),log10(braw.env$max_nw),length.out=basenpts)
                   basevals<-10^basevals
                 } else {
                   basevals<-5+seq(0,braw.env$max_nw,length.out=basenpts)
                 }
               }
        )
        if (!hypothesis$effect$world$On)
          hypothesis$effect$world<-list(On=TRUE,
                                        PDF="Single",
                                        RZ="r",
                                        PDFk=hypothesis$effect$rIV,
                                        pRplus=1)
        for (i in 1:length(newvals)) {
          hypothesis$effect$world$PDFk<-rVals[i]
          design$sN<-nVals[i]
          hypothesis$effect$world$pRplus<-rplusPs[i]
          r<-fullRSamplingDist(basevals,hypothesis$effect$world,design,
                               doStat=showType[si],logScale=logScale,quantiles=c(quants,0.5,1-quants))
          if (length(r)==1) theoryVals<-c(theoryVals,r)
          else {
            theoryLower<-c(theoryLower,r[1])
            theoryVals<-c(theoryVals,r[2])
            theoryUpper<-c(theoryUpper,r[3])
          }
        }
        if (logScale){
          theoryVals<-log10(theoryVals)
          theoryUpper<-log10(theoryUpper)
          theoryLower<-log10(theoryLower)
        }
      }
      if (showType[si]=="p(sig)") {
        for (i in 1:length(newvals)) {
          hypothesis$effect$world$PDFk<-rVals[i]
          design$sN<-nVals[i]
          hypothesis$effect$world$pRplus<-rplusPs[i]
          r<-fullPSig(hypothesis$effect$world,design,alpha=alphas[i])
          theoryVals<-c(theoryVals,r)
        }
      }
      if (is.element(showType[si],c("NHST","Inference","Source","Hits","Misses"))) {
        pRplus<-hypothesis$effect$world$pRplus
        hypothesis$effect$world$pRplus<-1
        theoryVals1<-c()
        for (i in 1:length(newvals)) {
          hypothesis$effect$world$PDFk<-rVals[i]
          design$sN<-nVals[i]
          # hypothesis$effect$world$pRplus<-rplusPs[i]
          r<-fullPSig(hypothesis$effect$world,design,alpha=alphas[i])
          theoryVals1<-c(theoryVals1,r)
        }
        switch(showType[si],
               "NHST"={
                 theoryVals1<-theoryVals1*(rplusPs)
                 theoryVals0<-theoryVals1*0+alphas*(1-rplusPs)
                 theoryVals2<-theoryVals0*0+(1-rplusPs)
               },
               "Source"={
                 theoryVals0<-theoryVals1*rplusPs/(theoryVals1*rplusPs+alphas*(1-rplusPs))
                 theoryVals2<-(1-theoryVals1)*rplusPs/((1-theoryVals1)*rplusPs+(1-alphas)*(1-rplusPs))
                 theoryVals1<-c()
               },
               "Inference"={
                 theoryVals0<-theoryVals1*rplusPs/(theoryVals1*rplusPs+alphas*(1-rplusPs))
                 theoryVals2<-(1-theoryVals1)*rplusPs/((1-theoryVals1)*rplusPs+(1-alphas)*(1-rplusPs))
                 theoryVals1<-c()
               },
               "Hits"={
                 theoryVals<-theoryVals1*rplusPs/(theoryVals1*rplusPs+alphas*(1-rplusPs))
                 theoryVals1<-c()
               },
               "Misses"={
                 theoryVals<-(1-theoryVals1)*rplusPs/((1-theoryVals1)*rplusPs+(1-alphas)*(1-rplusPs))
                 theoryVals1<-c()
               }
        )
        hypothesis$effect$world$pRplus<-pRplus
      }
      if (explore$xlog) newvals<-log10(newvals)
    }
    
    
    if (!is.null(exploreResult$result)) {
      result<-trimExploreResult(exploreResult$result,exploreResult$nullresult)
      if (is.null(hypothesis$IV2)){
        rVals<-result$rval
        pVals<-result$pval
      } else {
        rVals<-result$r[[effectType]][,,whichEffect]
        pVals<-result$p[[effectType]][,,whichEffect]
      }
      pVals[pVals==0]<-1e-20
      rpVals<-result$rpval
      nVals<-result$nval
      df1Vals<-result$df1
      
      showMax<-NULL
      showMin<-NULL
      switch (showType[si],
              "rs"={
                showVals<-rVals
              },
              "rp"={
                showVals<-rpVals
              },
              "re"={
                showVals<-rVals-rpVals
              },
              "p"={
                showVals<-pVals
                if (braw.env$pPlotScale=="log10"){
                  showVals<-log10(showVals)
                }
              },
              "ws"={
                showVals<-rn2w(rVals,result$nval)
                if (braw.env$wPlotScale=="log10"){
                  showVals<-log10(showVals)
                }
              },
              "wp"={
                showVals<-rn2w(rpVals,result$nval)
                if (braw.env$wPlotScale=="log10"){
                  showVals<-log10(showVals)
                }
              },
              "n"={
                showVals<-nVals
                if (braw.env$nPlotScale=="log10"){
                  showVals<-log10(showVals)
                }
              },
              "nw"={
                showVals<-rw2n(rVals,0.8,2)
                if (braw.env$nPlotScale=="log10"){
                  showVals<-log10(showVals)
                }
              },
              "likelihood"={
                showVals<-result$likes
              },
              "llknull"={
                showVals<-log10(exp(-0.5*(result$AIC-result$AICnull)))
              },
              "AIC"={
                showVals<-result$AIC-result$AICnull
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
                if (is.null(hypothesis$IV2)) ng<-2 else ng<-7
                nulls<-abs(rpVals)<=evidence$minRp
                semProps<-c()
                semPropsNull<-c()
                if (all(nulls) || all(!nulls)) {
                  for (ig in ng:1) semProps<-rbind(semProps,colMeans(result$sem==ig))
                  showVals<-semProps
                  showLabels<-showLabels[ng:1]
                  showSplit<-0
                } else {
                  for (ig in ng:1) semPropsNull<-rbind(semPropsNull,colSums((result$sem==ig)*nulls)/colSums(nulls | !nulls))
                  for (ig in 1:ng) semProps<-rbind(semProps,colSums((result$sem==ig)*(!nulls))/colSums(nulls | !nulls))
                  showVals<-rbind(semProps,semPropsNull)
                  showLabels<-c(paste0("H[+]('",showLabels[1:ng],"')"),paste0("H[0]('",showLabels[ng:1],"')"))
                  showSplit<-ng
                  ng<-ng*2
                }
                showCols<-rev(plotAxis("SEM",hypothesis,design)$cols[1:ng])
                showCols[rowSums(showVals)==0]<-NA
              },
              "log(lrs)"={
                ns<-result$nval
                df1<-result$df1
                showVals<-r2llr(rVals,ns,df1,"sLLR",evidence$llr,evidence$prior)
              },
              "log(lrd)"={
                ns<-result$nval
                df1<-result$df1
                showVals<-r2llr(rVals,ns,df1,"dLLR",evidence$llr,evidence$prior)
              },
              "n(sig)"={
                showVals<-NULL
                showMeans<-colMeans(result$nSig)
                if (any(!is.na(result$nFP))) {
                showMeans2<-colMeans(result$nFP)
                showMeans<-rbind(showMeans,showMeans2)
                }
                showMax<-apply(result$nSig,2,max)
                showMin<-apply(result$nSig,2,min)
              },
              "n(fd)"={
                showVals<-NULL
                showMeans<-colMeans(result$nFP)
                showMax<-apply(result$nFP,2,max)
                showMin<-apply(result$nFP,2,min)
              },
              "metaBias"={
                showVals<-result$sigOnly
              },
              "metaRiv"={
                showVals<-result$PDFk
              },
              "metaRsd"={
                showVals<-result$pRplus
              },
              "LambdaRn"={
                showVals<-result$pRplus
              },
              "PDFk"={
                showVals<-result$PDFk
              },
              "PDFshape"={
                showVals<-result$PDFshape
              },
              "mean(R+)"={
                showVals<-result$PDFk
              },
              "p(R+)"={
                showVals<-result$pRplus
              },
              "metaSmax"={
                showVals<-result$S
              },
              "p(w80)"={
                w<-rn2w(result$rpval,result$nval)
                showMeans<-colMeans(w>=0.8)
                showSE<-NULL
                showVals<-NULL
              },
              "p(sig)"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (explore$exploreType=="minRp") {
                  evidence$minRp<-exploreResult$vals
                }
                if (design$sBudgetOn) {
                  getStat<-function(x,n) {colMeans(x)*max(n)/colMeans(n)}
                } else {
                  getStat<-function(x,n) {colMeans(x)}
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                if (all(nulls) || all(!nulls)) {
                  showMeans<-getStat(abs(sigs),nVals)
                  sigs0<-colMeans(abs(sigs))
                  showSE<-sqrt(sigs0*(1-sigs0)/length(nulls))
                } else {
                  if (any(!nulls) || all(nulls)) {
                    showMeans<-getStat(abs(sigs & !nulls),nVals)
                    sigs0<-colMeans(abs(sigs & !nulls))
                    showSE<-sqrt(sigs0*(1-sigs0)/sum(!nulls))
                  } else {
                    showMeans<-0
                    showSE<-NULL
                  }
                }
                  
                if (any(hypothesis$effect$world$On)) {
                  showMeans2<-getStat(abs(sigs & nulls),nVals)
                  sigs0<-colMeans(abs(sigs & nulls))
                  showSE2<-sqrt(sigs0*(1-sigs0)/sum(nulls))
                  showMeans<-rbind(showMeans+showMeans2,showMeans2)
                showSE<-NULL
                showVals<-rbind(1-showMeans[1,],showMeans[1,]-showMeans[2,],showMeans[2,])
                showCols<-c(NA,col0,col5)
                showLabels<-c(NA,lb0,lb5)
                showSplit<-0
                } else {
                  showVals<-showMeans
                  showMeans<-rbind(showMeans)
                }
              },
              "tDR"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-1-colMeans(abs(sigs & nulls))/colMeans(abs(sigs))
                showSE<-NULL
              },
              "Inference"={
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (explore$exploreType=="minRp") {
                  evidence$minRp<-exploreResult$vals
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-colMeans(abs(sigs & !nulls))/colMeans(abs(sigs))
                showMeans<-rbind(showMeans,colMeans(abs(!sigs & nulls))/colMeans(abs(!sigs)))
              },
              "Source"={
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (explore$exploreType=="minRp") {
                  evidence$minRp<-exploreResult$vals
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-colMeans(abs(sigs & !nulls))/colMeans(abs(!nulls))
                showMeans<-rbind(showMeans,colMeans(abs(sigs & nulls))/colMeans(abs(nulls)))
              },
              "Hits"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (explore$exploreType=="minRp") {
                  evidence$minRp<-exploreResult$vals
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-colMeans(abs(sigs & !nulls))/colMeans(abs(sigs))
                showSE<-NULL
              },
              "Misses"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (explore$exploreType=="minRp") {
                  evidence$minRp<-exploreResult$vals
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-colMeans(abs(!sigs & !nulls))/colMeans(abs(!sigs))
                showSE<-NULL
              },
              "NHST"={
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (explore$exploreType=="minRp") {
                  evidence$minRp<-exploreResult$vals
                }
                nulls<-abs(result$rpval)<=matrix(evidence$minRp,nrow(result$rpval),ncol(result$rpval),byrow=TRUE)
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                if (braw.env$STMethod=="NHST") {
                  d<-sigs
                } else {
                  d<-r2llr(rVals,nVals,df1Vals,braw.env$STMethod,world=effect$world)
                }
                np<-sum(!is.na(pVals[,1]))
                if (fixNulls) {
                  adjustNulls<-(1-effect$world$pRplus)/(colSums(nulls)/np)
                  adjustNonNulls<-effect$world$pRplus/(colSums(!nulls)/np)
                } else {
                  adjustNulls<-1
                  adjustNonNulls<-1
                }
                sigNonNulls<- colSums( sigs & d>0 & !nulls,na.rm=TRUE)/np*adjustNonNulls
                nsigNonNulls<-colSums(!sigs &       !nulls,na.rm=TRUE)/np*adjustNonNulls
                isigNonNulls<-colSums( sigs & d<0 & !nulls,na.rm=TRUE)/np*adjustNonNulls
                isigNulls<-   colSums( sigs & d<0 & nulls,na.rm=TRUE)/np*adjustNulls
                sigNulls<-    colSums( sigs & d>0 & nulls,na.rm=TRUE)/np*adjustNulls
                nsigNulls<-   colSums(!sigs &       nulls,na.rm=TRUE)/np*adjustNulls
                showVals<-rbind()
                showVals<-rbind(isigNonNulls,nsigNonNulls,sigNonNulls,sigNulls,nsigNulls,isigNulls)
                rownames(showVals)<-c("isigNonNulls","nsigNonNulls","sigNonNulls","sigNulls","nsigNulls","isigNulls")
                showCols<-c(col1,col2,col0,col5,col3,col4)
                showLabels<-c(lb1,lb2,lb0,lb5,lb3,lb4)
                showSplit<-3
                lines<-c(0.05)
              },
              
              "PDF"={
                showVals<-NULL
                showMeans<-colMeans(result$dists==effect$world$PDF,na.rm=TRUE)
                showSE<-sqrt(showMeans*(1-showMeans)/nrow(showVals))
              },
              "iv.mn"={
                showVals<-result$iv$mn
              },
              "iv.sd"={
                showVals<-result$iv$sd
              },
              "iv.sk"={
                showVals<-result$iv$sk
              },
              "iv.kt"={
                showVals<-result$iv$kt
              },
              "dv.mn"={
                showVals<-result$dv$mn
              },
              "dv.sd"={
                showVals<-result$dv$sd
              },
              "dv.sk"={
                showVals<-result$dv$sk
              },
              "dv.kt"={
                showVals<-result$dv$kt
              },
              "er.mn"={
                showVals<-result$rs$mn
              },
              "er.sd"={
                showVals<-result$rs$sd
              },
              "er.sk"={
                showVals<-result$rs$sk
              },
              "er.kt"={
                showVals<-result$rs$kt
              }
      )
      if (is.element(showType[si],c("rs","rp","re","ro","metaRiv","metaRsd"))) {
        switch(braw.env$RZ,
               "r"={},
               "z"={showVals<-atanh(showVals)}
               )
      }
      if (!is.element(showType[si],c("NHST","SEM"))) {
        y50a<-NULL
        y75<-NULL
        y25<-NULL
        y62<-NULL
        y38<-NULL
        # draw the basic line and point data
        if (is.element(showType[si],c("n(sig)","n(fd)","p(sig)","p(w80)","Hits","Misses","Inference","Source"))) {
          if (nrow(showMeans)>1) {
            y50<-showMeans[1,]
            y50a<-showMeans[2,]
            if (all(y50a==0)) y50a<-y50+NA
            y75<-showMax
            y25<-showMin
          } else {
          y50<-showMeans
          }
        } else {
          y75<-apply( showVals , 2 , quantile , probs = 0.50+quants , na.rm = TRUE ,names=FALSE)
          y62<-apply( showVals , 2 , quantile , probs = 0.50+quants/2 , na.rm = TRUE ,names=FALSE)
          y50<-apply( showVals , 2 , quantile , probs = 0.50 , na.rm = TRUE ,names=FALSE)
          y38<-apply( showVals , 2 , quantile , probs = 0.50-quants/2 , na.rm = TRUE ,names=FALSE)
          y25<-apply( showVals , 2 , quantile , probs = 0.50-quants , na.rm = TRUE ,names=FALSE)
        }
      }
      
      if (!is.element(showType[si],c("NHST","p(sig)")))
      if (!fixedYlim) {
        if (showHist) {
          use_y<-c(showVals,theoryVals,theoryLower,theoryVals1,theoryVals0,theoryVals2)
        } else {
          use_y<-c(y25,y50a,y50,y75,theoryVals,theoryLower,theoryVals1,theoryVals0,theoryVals2)
        }
        use_y[is.infinite(use_y)]<-NA
        ylim<-c(
          min(use_y,na.rm=TRUE),
          max(use_y,na.rm=TRUE)
              )
        if (diff(ylim)==0) ylim<-ylim+c(-1,1)*ylim/10
        else               ylim<-ylim+c(-1,1)*diff(ylim)/10
      }
      # general start
      if (effectType==effectTypes[1]) {
      g<-startPlot(xlim,ylim,
                   xticks=makeTicks(breaks=xbreaks,labels=xnames,logScale=explore$xlog),
                   xlabel=makeLabel(label=exploreTypeShow),
                   xmax=TRUE,
                   yticks=makeTicks(logScale=yaxis$logScale),
                   ylabel=makeLabel(label=ylabel),
                   top=TRUE,g=g)
      if (nchar(useLabel)>0)    g<-addG(g,plotTitle(useLabel,size=1.5))
      else g<-addG(g,plotTitle(paste0("nsims=",exploreResult$count),size=0.5,position = "left"))
      }
      
      # theory plots
      if (!is.null(theoryVals)) {
        theory<-data.frame(x=newvals, y=theoryVals)
        g<-addG(g,dataLine(theory,colour=darken(col,1,-0.15),linewidth=1))
      }
      if (!is.null(theoryUpper)) {
        theory<-data.frame(x=newvals, y=theoryUpper)
        g<-addG(g,dataLine(theory,colour=darken(col,1,-0.15),linewidth=0.5))
      }
      if (!is.null(theoryLower)) {
        theory<-data.frame(x=newvals, y=theoryLower)
        g<-addG(g,dataLine(theory,colour=darken(col,1,-0.15),linewidth=0.5))
      }
      if (!is.null(theoryVals1)) {
        theory<-data.frame(x=c(newvals,rev(newvals)), y=1-c(theoryVals1,theoryVals1*0))
        g<-addG(g,dataPolygon(theory,colour=braw.env$plotColours$infer_sigNonNull,fill=braw.env$plotColours$infer_sigNonNull))
      }
      if (!is.null(theoryVals0)) {
        theory<-data.frame(x=c(newvals,rev(newvals)), y=c(theoryVals0,theoryVals0*0))
        g<-addG(g,dataPolygon(theory,colour=braw.env$plotColours$infer_sigNull,fill=braw.env$plotColours$infer_sigNull))
      }
      if (!is.null(theoryVals2)) {
        theory<-data.frame(x=newvals, y=theoryVals2)
        g<-addG(g,dataLine(theory,colour="#000000",linewidth=0.5))
      }
      
      # plot results
      if (!is.element(showType[si],c("NHST","SEM"))) {
        # draw the basic line and point data
        if (!isempty(y50)) {
          y50[y50>ylim[2]]<-ylim[2]
          y50[y50<ylim[1]]<-ylim[1]
          if (!is.null(y50a)) {
            y50a[y50a>ylim[2]]<-ylim[2]
            y50a[y50a<ylim[1]]<-ylim[1]
          }
          if (!is.null(y75)) {
            y75[y75>ylim[2]]<-ylim[2]
            y75[y75<ylim[1]]<-ylim[1]
          }
          if (!is.null(y62)) {
            y62[y62>ylim[2]]<-ylim[2]
            y62[y62<ylim[1]]<-ylim[1]
          }
          if (!is.null(y38)) {
            y38[y38>ylim[2]]<-ylim[2]
            y38[y38<ylim[1]]<-ylim[1]
          }
          if (!is.null(y25)) {
            y25[y25>ylim[2]]<-ylim[2]
            y25[y25<ylim[1]]<-ylim[1]
          }

        if (doLine) {
          if (!is.null(y75)) {
            pts1f<-data.frame(x=c(vals,rev(vals)),y=c(y25,rev(y75)))
            pts2f<-data.frame(x=c(vals,rev(vals)),y=c(y38,rev(y62)))
            g<-addG(g,dataPolygon(data=pts1f,fill=col,alpha=0.2,colour=NA))
            g<-addG(g,dataLine(data.frame(x=vals,y=y25),colour="white",alpha=0.3))
            g<-addG(g,dataLine(data.frame(x=vals,y=y75),colour="white",alpha=0.3))
            g<-addG(g,dataPolygon(data=pts2f,fill=col,alpha=0.4,colour=NA))
            g<-addG(g,dataLine(data.frame(x=vals,y=y38),colour="white",alpha=0.6))
            g<-addG(g,dataLine(data.frame(x=vals,y=y62),colour="white",alpha=0.6))
          }
          y50<-rbind(y50)
          for (i in 1:nrow(y50)) {
          pts0f<-data.frame(x=vals,y=y50[i,])
          g<-addG(g,dataLine(data=pts0f,linewidth=0.75))
          g<-addG(g,dataPoint(data=pts0f,fill=ycols[i],size=4))
          }
          if (!is.null(y50a)) {
            g<-addG(g,dataLine(data.frame(x=vals,y=y50a,linewidth=0.75)))
            g<-addG(g,dataPoint(data.frame(x=vals,y=y50a),fill=braw.env$plotColours$infer_sigNull,size=4))
            g<-addG(g,dataLegend(data.frame(names=c("total","false discovery"),colours=c(ycols[1],braw.env$plotColours$infer_sigNull))))
          }
        } else { # not doLine
          if (nrow(rVals)>0)
          sigVals<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,braw.env$alphaSig)
          else
          sigVals<-showVals*0+1
          if (!is.null(showVals)) {
            for (i in 1:length(vals)) {
              if (i==1) left=(vals[i+1]-vals[i])*0.35 else left=(vals[i]-vals[i-1])*0.35
              if (i==length(vals)) right=(vals[i]-vals[i-1])*0.35 else right=(vals[i+1]-vals[i])*0.35
              g<-simulations_plot(g,
                               data.frame(x=vals[i],y1=showVals[,i],sig=sigVals[,i],notNull=sigVals[,i]*0),
                               showType=showType[si],
                               simWorld=exploreResult$hypothesis$effect$world,
                               design=exploreResult$design,
                               ylim=ylim,
                               scale=3/(length(vals)+1),
                               orientation="horz",
                               width=c(left,right),
                               col=col,useSignificanceCols=FALSE,
                               histStyle="dense",
                               alpha=min(1,2.5/sqrt(length(showVals[,i]))),
                               npointsMax=500/length(vals)
              )
            }
          }
          pts0f<-data.frame(x=vals,y=y50)
          g<-addG(g,dataLine(data=pts0f,linewidth=0.75))
          if (length(vals)>=25) pts0f<-pts0f[seq(1,length(vals),round(length(vals)/13)),]
          g<-addG(g,dataPoint(data=pts0f,fill=col,size=4))
          if (!is.null(y75)) {
            g<-addG(g,dataLine(data.frame(x=vals,y=y25),colour="#000000",alpha=0.9))
            g<-addG(g,dataLine(data.frame(x=vals,y=y75),colour="#000000",alpha=0.9))
          }
          if (!is.null(y50a) && is.element(showType[si],c("n(sig)","n(fd)","p(sig)","p(w80)","Hits","Misses","Inference","Source"))) {
            g<-addG(g,dataLine(data=data.frame(x=vals,y=y50a),colour="#000000"))
            g<-addG(g,dataPoint(data=data.frame(x=vals,y=y50a),fill=braw.env$plotColours$infer_sigNull,size=4))
            g<-addG(g,dataLegend(data.frame(names=c("total","false discovery"),colours=c(col,braw.env$plotColours$infer_sigNull))))
          }
        } # end of line and point
        }
        if (showType[si]=="Inference") {
          data<-data.frame(colours=ycols,names=c("Hits","Misses"))
          g<-addG(g,dataLegend(data=data,title=showType))
        }
        if (showType[si]=="Source") {
          if (exploreResult$evidence$minRp!=0 || explore$exploreType=="minRp") 
            data<-data.frame(colours=ycols,names=c(braw.env$activeTitle,braw.env$inactiveTitle))
            else
              data<-data.frame(colours=ycols,names=c(braw.env$nonnullTitle,braw.env$nullTitle))
            g<-addG(g,dataLegend(data=data,title=showType))
        }
      } # end of !("NHST","SEM")
      else {
        # now the NHST filled areas
        ytop<-rep(1,ncol(showVals))
        # if (doLine) xoff<-0
        # else        xoff<-bwidth
        
        for (row in 1:nrow(showVals)) {
          colShow<-showCols[row]
          valsD<-showVals[row,]
          valsD[is.na(valsD)]<-0
          if (any(valsD!=0)) {
            ybottom<-ytop-valsD
            ybottom[ybottom<0]<-0
            if (showTheory) ptsShow<-data.frame(x=vals,y=ybottom)
            else            ptsShow<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
            ytop<-ybottom
          } else {
            ybottom<-ytop
            ptsShow<-NULL
          }
          if (!is.na(colShow) && !is.null(ptsShow)) {
            if (showTheory) {
              g<-addG(g,dataPoint(data=ptsShow,fill=colShow))
            } else {
              if (doLine) {
                g<-addG(g,dataPolygon(data=ptsShow,fill=colShow,colour="#000000",linewidth=0.1))
                if (is.element(row,showSplit)) g<-addG(g,dataLine(data.frame(x=vals,y=ybottom),linewidth=1))
              } else {
                  npts<-length(vals)
                  bwidth<-0.4*(ptsShow$x[2]-ptsShow$x[1])
                  for (i in 1:npts) {
                    g<-addG(g,drawNHSTBar(i,npts,ptsShow,bwidth,colShow))
                    if (is.element(row,showSplit)) g<-addG(g,drawNHSTLine(i,npts,ptsShow,bwidth*1.25,linewidth=1))
                  }
              }
            }
          }
        }
        g<-addG(g,dataLegend(data.frame(colours=showCols[!is.na(showCols)],names=showLabels[!is.na(showCols)]),title="",shape=22))
      }

      
      # find n80
      if (is.null(hypothesis$IV2) && showPower && 
          showType[si]=="p(sig)" && explore$exploreType=="n" && 
          (!effect$world$On || effect$world$PDF=="Single")){
          w<-y50
        n<-exploreResult$vals
        minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
        r_est<-r_est$minimum
        nvals<-seq(min(n),max(n),length.out=101)
        yvals<-rn2w(r_est,nvals)
        if (explore$xlog=="log10") ptsn<-data.frame(x=log10(nvals),y=yvals)
        else          ptsn<-data.frame(x=nvals,y=yvals)
        g<-addG(g,dataLine(data=ptsn,colour="#000000",linetype="dotted",linewidth=0.25))
        
        minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        n80<-optimize(minnw,c(min(n),max(n)),w=0.8,r=r_est)
        
        if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
          label<-paste("n80 =",format(round(n80$minimum),digits=2))
        } else {
          if (sum(n<n80$minimum)<2) label<-paste("Unsafe result")
          if (sum(n>n80$minimum)<2) label<-paste("Unsafe result")
        }
        if (braw.env$nPlotScale=="log10") 
             lpts<-data.frame(x=log10(min(n)),y=ylim[2]-diff(ylim)/10,label=label)
        else lpts<-data.frame(x=min(n),y=ylim[2]-diff(ylim)/10,label=label)
        g<-addG(g,dataLabel(data=lpts,label = label))
      }
      
      # find r80
      if (is.null(hypothesis$IV2) && showPowerR &&
          showType[si]=="p(sig)" && explore$exploreType=="rIV" &&
          (!effect$world$On || effect$world$PDF=="Single")){
        w<-y50
        r<-exploreResult$vals
        minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        maxN<-50
        n_est<-50
        while (n_est>(maxN/2)) {
          maxN<-maxN*2
          n_est<-optimize(minrw,c(0,maxN),w=w,r=r)
        n_est<-n_est$minimum
        }
        rvals<-seq(min(r),max(r),length.out=101)
        yvals<-rn2w(rvals,n_est)
        ptsn<-data.frame(x=rvals,y=yvals)
        g<-addG(g,dataLine(data=ptsn,colour="white",linetype="dotted",linewidth=0.25))
        
        minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        n80<-optimize(minnw,c(0,0.8),w=0.8,n=n_est)
        
        if (sum(r<n80$minimum)>=2 && sum(r>n80$minimum)>=2){
          label<-paste("r80 =",brawFormat(n80$minimum,digits=2))
        } else {
          if (sum(r<n80$minimum)<2) label<-paste("Unsafe result")
          if (sum(r>n80$minimum)<2) label<-paste("Unsafe result")
        }
        lpts<-data.frame(x=0,y=ylim[2]-diff(ylim)/10)
        g<-addG(g,dataLabel(data=lpts,label = label))
      }
    }
    
    lineCol<-"#000000"
    if (is.element(showType[si],c("p","e1p","e2p","e1d","e2d"))) lineCol<-"green"
    for (yl in ylines) {
      g<-addG(g,horzLine(yl,linetype="dotted",colour=lineCol))
    }
  }
    if (!is.null(hypothesis$IV2) && is.element(showType[si],c("rs","p"))) {
      if (effectType=="all") use<-1:3
      else use<-which(effectType==c("direct","unique","total"))
      g<-addG(g,dataLegend(data.frame(names=c("direct","unique","total")[use],
                                      colours=c(ycols[1],darken(desat(ycols[1],0.7),1.3),darken(desat(ycols[1],0.7),0.7))[use]
                                      ),
                           title="",fontsize=1)
      )
    }
  }
  }
  # if (exploreResult$count>0)
  # g<-addG(g,plotTitle(paste0("nsims=",brawFormat(exploreResult$count)),"right",size=1,fontface="plain"))
  if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
    print(g)
    return(invisible(g))
  }
  return(g)  
}

showExplore2D<-function(exploreResult=braw.res$explore,showType=c("rs","p"),showTheory=FALSE,
                 effectType="unique",whichEffect="All") {
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  design<-exploreResult$design
  
  result<-trimExploreResult(exploreResult$result,exploreResult$nullresult)
  if (is.null(hypothesis$IV2)){
    rVals<-result$rval
    pVals<-result$pval
  } else {
    rVals<-result$r[[effectType]][,,whichEffect]
    pVals<-result$p[[effectType]][,,whichEffect]
  }
  rpVals<-result$rpval
  nVals<-result$nval
  df1Vals<-result$df1
  
  for (si in 1:2) {
  switch (showType[si],
          "rs"={
            showVals<-rVals
          },
          "rp"={
            showVals<-rpVals
          },
          "re"={
            showVals<-rVals-rpVals
          },
          "p"={
            showVals<-pVals
            if (braw.env$pPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          },
          "ws"={
            showVals<-rn2w(rVals,result$nval)
            if (braw.env$wPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          },
          "wp"={
            showVals<-rn2w(rpVals,result$nval)
            if (braw.env$wPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          },
          "n"={
            showVals<-nVals
            if (braw.env$nPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          },
          "nw"={
            showVals<-rw2n(rVals,0.8,2)
            if (braw.env$nPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          }
  )
    if (is.element(showType[si],c("rs","rp","re","ro","metaRiv","metaRsd"))) {
      switch(braw.env$RZ,
             "r"={},
             "z"={showVals<-atanh(showVals)}
      )
    }
    
    switch (si,
            xVals<-apply(showVals,2,median),
            yVals<-apply(showVals,2,median)
    )
  }

  g<-NULL
  
  xaxis<-plotAxis(showType[1],hypothesis,design)
  xlim<-xaxis$lim
  xlabel<-xaxis$label
  xcols<-xaxis$cols
  xlines<-xaxis$lines
  xSecond<-NULL
  
  yaxis<-plotAxis(showType[2],hypothesis,design)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ycols<-yaxis$cols
  ylines<-yaxis$lines
  ySecond<-NULL

  if ((showType[2]=="rs") && (!is.null(IV2))) switch(whichEffect,ylabel<-"Main 1",ylabel<-"Main 2",ylabel<-"Interaction")
  if (showType[1]=="p" && braw.env$pPlotScale=="log10" && any(exploreResult$result$pval>0)) 
    while (mean(log10(exploreResult$result$pval)>xlim[1])<0.75) xlim[1]<-xlim[1]-1
  if (showType[2]=="p" && braw.env$pPlotScale=="log10" && any(exploreResult$result$pval>0)) 
    while (mean(log10(exploreResult$result$pval)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
  
  braw.env$plotArea<-c(0,0,1,1)*0.67+0.33
  g<-startPlot(xlim,ylim,
               xticks=makeTicks(logScale=xaxis$logScale),xlabel=makeLabel(xlabel),
               yticks=makeTicks(logScale=xaxis$logScale),ylabel=makeLabel(ylabel),
               box="both",top=TRUE,g=g)
  g<-addG(g,plotTitle(paste0("explore: ",explore$exploreType),size=1,fontface="plain",position="left"))

  lineCol<-"#000000"
  if (is.element(showType[1],c("p","e1p","e2p","e1d","e2d"))) lineCol<-"green"
  for (xl in xlines) {
    g<-addG(g,vertLine(xl,linetype="dotted",colour=lineCol,linewidth=0.5))
  }
  lineCol<-"#000000"
  if (is.element(showType[2],c("p","e1p","e2p","e1d","e2d"))) lineCol<-"green"
  for (yl in ylines) {
    g<-addG(g,horzLine(yl,linetype="dotted",colour=lineCol,linewidth=0.5))
  }
  
  g<-addG(g,dataLine(data.frame(x=xVals,y=yVals)))
  g<-addG(g,dataPoint(data.frame(x=xVals,y=yVals)))
  
  # if (exploreResult$count>0)
  #   g<-addG(g,plotTitle(paste0("nsims=",brawFormat(exploreResult$count)),"right",size=1))
  if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
    showHTML(g)
    return(invisible(NULL))
  }
  if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
    print(g)
    return(invisible(g))
  }
  return(g)  
}
