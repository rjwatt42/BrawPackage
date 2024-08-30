drawNHSTBar<-function(i,npts,pts1,bwidth,col1) {
  barx<-c(-1,-1,1,1)*bwidth
  bary<-c(i,npts*2-i+1,npts*2-i+1,i)
  
  y1<-pts1$y[bary]
  x1<-pts1$x[i]+barx
  pts<-data.frame(x=x1,y=y1)
  dataPolygon(data=pts,fill=col1)
}
drawNHSTLabel<-function(lb1,lb1xy,xoff,col1) {
  
  if (sum(col2rgb(col1))>128*3) col<-"black" else col<-"white"
  lb1xy$x<-lb1xy$x+xoff
  dataLabel(data=lb1xy,label=lb1,
            hjust=1,vjust=0.5,
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
#'                        showType="Basic",
#'                        effectType="unique",whichEffect="All")
#' @export
showExplore<-function(exploreResult=braw.res$explore,showType="Basic",dimension="1D",showTheory=FALSE,
                      effectType="unique",whichEffect="All"){

  if (is.null(exploreResult)) exploreResult=doExplore()

  showType<-strsplit(showType,";")[[1]]
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {showType<-c("rs","p")},
           "Power"=     {showType<-c("ws","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           {}
    )
  }

  if (!exploreResult$hypothesis$effect$world$worldOn && is.element(showType[1],c("NHST","Hits","Misses"))) {
    if (exploreResult$nullcount<exploreResult$count) {
      exploreResult<-doExplore(0,exploreResult,doingNull=TRUE)
    }
  }
  
  if (length(showType)>1 && dimension=="2D") {
    g<-showExplore2D(exploreResult=exploreResult,showType=showType,showTheory=showTheory,
                     effectType=effectType,whichEffect=whichEffect)
    return(g)
  }
  
  quants<-0.25
  showPower<-TRUE # show power calculations?
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  design<-exploreResult$design
  evidence<-exploreResult$evidence
  
  
  oldAlpha<-braw.env$alphaSig
  on.exit(braw.env$alphaSig<-oldAlpha)
  
  vals<-exploreResult$vals
  
  if (explore$exploreType=="rIV" && braw.env$RZ=="z") {
    vals<-atanh(vals)
  }
  
  if (!is.null(hypothesis$IV2) && whichEffect=="All") {
    plots<-matrix(c(0,0.33,0.65),nrow=1,byrow=TRUE)
    plotWidth<-0.35
    whichEffects<-1:3
  } else {
    switch (length(showType),
            {plots<-matrix(0)
            plotWidth<-1},
            {plots<-matrix(c(0,0.5),nrow=2)
            plotWidth<-0.5}
    )
    if (!is.null(hypothesis$IV2)) 
      switch (whichEffect,
              "Main 1"=whichEffects<-1,
              "Main 2"=whichEffects<-2,
              "rIVIV2DV"=whichEffects<-3
      )
    else whichEffects<-1
  }
  
  if (is.character(vals[1]) || length(vals)<10) {
    xlim<-c(0,length(vals)+1)
    vals<-1:length(vals)
    xbreaks<-vals
    xnames<-exploreResult$vals
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
  
  g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  
  for (si in 1:length(showType)) {
    
  yaxis<-plotAxis(showType[si],effect)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ycols<-yaxis$cols
  ylines<-yaxis$lines
  ySecond<-NULL
  
  if (showType[si]=="p" && braw.env$pPlotScale=="log10" && any(exploreResult$result$pval>0)) 
    while (mean(log10(exploreResult$result$pval)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
  
  col2<-braw.env$plotColours$infer_nsigNonNull
  col3<-braw.env$plotColours$infer_nsigNull
  col5<-braw.env$plotColours$infer_sigNull
  switch (braw.env$STMethod,
          "NHST"={
            col1<-NULL
            col0<-braw.env$plotColours$infer_sigNonNull
            col4<-NULL
          },
          "sLLR"={
            col1<-NULL
            col0<-braw.env$plotColours$infer_nsigNonNull
            col4<-NULL
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
  lb3<-braw.env$nullNS
  lb4<-braw.env$nullNegative
  lb5<-braw.env$nullPositive
  
  yn<-0.5
  lb0xy<-data.frame(x=max(xlim),y=1-yn/10)
  yn<-yn+1
  lb1xy<-data.frame(x=max(xlim),y=1-yn/10)
  yn<-yn+1
  lb2xy<-data.frame(x=max(xlim),y=1-yn/10)
  yn<-0.5
  lb5xy<-data.frame(x=max(xlim),y=0+yn/10)
  yn<-yn+1
  lb4xy<-data.frame(x=max(xlim),y=0+yn/10)
  yn<-yn+1
  lb3xy<-data.frame(x=max(xlim),y=0+yn/10)
  
  for (whichEffect in whichEffects) {
    braw.env$plotArea<-c(plots[si,whichEffect],0,plotWidth,1)
    g<-startPlot(xlim,ylim,box="Both",top=TRUE,tight=TRUE,g=g)
    g<-g+xAxisTicks(xbreaks,xnames,logScale=explore$xlog)+xAxisLabel(bquote(bold(.(explore$exploreType))))
    g<-g+yAxisTicks(logScale=yaxis$logScale)+yAxisLabel(ylabel)
    col<-ycols[1]
    
    theoryVals<-NULL
    theoryUpper<-NULL
    theoryLower<-NULL
    theoryVals0<-c()
    theoryVals1<-c()
    theoryVals2<-c()
    if (showTheory) {
      # if (!hypothesis$effect$world$worldOn)
      #   hypothesis$effect$world<-list(worldOn=TRUE,populationPDF="Single",populationRZ="r",populationPDFk=hypothesis$effect$rIV,populationNullp<-0)
      n75=qnorm(0.75)
      newvals<-seq(min(vals),max(vals),length.out=51)
      if (explore$xlog) newvals<-10^newvals
      alphas<-braw.env$alphaSig
      if (hypothesis$effect$world$worldOn)  rVal<-hypothesis$effect$world$populationPDFk
      else                                  rVal<-hypothesis$effect$rIV
      switch(explore$exploreType,
             "rIV"={
               rVals<-newvals
               nVals<-rep(design$sN,length(newvals))
               alphas<-rep(braw.env$alphaSig,length(newvals))
               nullPs<-rep(hypothesis$effect$world$populationNullp,length(newvals))
             },
             "n"={
               rVals<-rep(rVal,length(newvals))
               nVals<-newvals
               alphas<-rep(braw.env$alphaSig,length(newvals))
               nullPs<-rep(hypothesis$effect$world$populationNullp,length(newvals))
             },
             "Alpha"={
               rVals<-rep(rVal,length(newvals))
               nVals<-rep(design$sN,length(newvals))
               alphas<-newvals
               nullPs<-rep(hypothesis$effect$world$populationNullp,length(newvals))
             },
             "pNull"={
               rVals<-rep(rVal,length(newvals))
               nVals<-rep(design$sN,length(newvals))
               alphas<-rep(braw.env$alphaSig,length(newvals))
               nullPs<-newvals
             },
             {
               rVals<-rep(rVal,length(newvals))
               nVals<-rep(design$sN,length(newvals))
               alphas<-rep(braw.env$alphaSig,length(newvals))
               nullPs<-rep(hypothesis$effect$world$populationNullp,length(newvals))
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
        if (!hypothesis$effect$world$worldOn)
          hypothesis$effect$world<-list(worldOn=TRUE,
                                        populationPDF="Single",
                                        populationRZ="r",
                                        populationPDFk=hypothesis$effect$rIV,
                                        populationNullp=0)
        for (i in 1:length(newvals)) {
          hypothesis$effect$world$populationPDFk<-rVals[i]
          design$sN<-nVals[i]
          hypothesis$effect$world$populationNullp<-nullPs[i]
          r<-fullRSamplingDist(basevals,hypothesis$effect$world,design,
                               doStat=showType[si],logScale=logScale,quantiles=c(0.25,0.5,0.75))
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
          hypothesis$effect$world$populationPDFk<-rVals[i]
          design$sN<-nVals[i]
          hypothesis$effect$world$populationNullp<-nullPs[i]
          r<-fullPSig(hypothesis$effect$world,design,alpha=alphas[i])
          theoryVals<-c(theoryVals,r)
        }
      }
      if (is.element(showType[si],c("NHST","Hits","Misses"))) {
        Nullp<-hypothesis$effect$world$populationNullp
        hypothesis$effect$world$populationNullp<-0
        theoryVals1<-c()
        for (i in 1:length(newvals)) {
          hypothesis$effect$world$populationPDFk<-rVals[i]
          design$sN<-nVals[i]
          # hypothesis$effect$world$populationNullp<-nullPs[i]
          r<-fullPSig(hypothesis$effect$world,design,alpha=alphas[i])
          theoryVals1<-c(theoryVals1,r)
        }
        switch(showType[si],
               "NHST"={
                 theoryVals1<-theoryVals1*(1-nullPs)
                 theoryVals0<-theoryVals1*0+alphas*nullPs
                 theoryVals2<-theoryVals0*0+nullPs
               },
               "Hits"={
                 theoryVals<-theoryVals1*(1-nullPs)/(theoryVals1*(1-nullPs)+alphas*nullPs)
                 theoryVals1<-c()
               },
               "Misses"={
                 theoryVals<-(1-theoryVals1)*(1-nullPs)/((1-theoryVals1)*(1-nullPs)+(1-alphas)*nullPs)
                 theoryVals1<-c()
               }
        )
        hypothesis$effect$world$populationNullp<-Nullp
      }
      
      if (explore$xlog) newvals<-log10(newvals)
      if (!is.null(theoryVals)) {
        theory<-data.frame(x=newvals, y=theoryVals)
        g<-g+dataLine(theory,colour=darken(col,1,-0.15),linewidth=1)
      }
      if (!is.null(theoryUpper)) {
        theory<-data.frame(x=newvals, y=theoryUpper)
        g<-g+dataLine(theory,colour=darken(col,1,-0.15),linewidth=0.5)
      }
      if (!is.null(theoryLower)) {
        theory<-data.frame(x=newvals, y=theoryLower)
        g<-g+dataLine(theory,colour=darken(col,1,-0.15),linewidth=0.5)
      }
      
      if (!is.null(theoryVals1)) {
        theory<-data.frame(x=c(newvals,rev(newvals)), y=1-c(theoryVals1,theoryVals1*0))
        g<-g+dataPolygon(theory,colour=braw.env$plotColours$infer_sigNonNull,fill=braw.env$plotColours$infer_sigNonNull)
      }
      if (!is.null(theoryVals0)) {
        theory<-data.frame(x=c(newvals,rev(newvals)), y=c(theoryVals0,theoryVals0*0))
        g<-g+dataPolygon(theory,colour=braw.env$plotColours$infer_sigNull,fill=braw.env$plotColours$infer_sigNull)
      }
      if (!is.null(theoryVals2)) {
        theory<-data.frame(x=newvals, y=theoryVals2)
        g<-g+dataLine(theory,colour="black",linewidth=0.5)
      }
      
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
      rpVals<-result$rpval
      nVals<-result$nval
      df1Vals<-result$df1
      
      switch (showType[si],
              "rs"={
                showVals<-rVals
                if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
              },
              "rp"={
                showVals<-rpVals
                if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
              },
              "re"={
                showVals<-rVals-rpVals
                if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
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
              "Lambda"={
                showVals<-result$k
              },
              "pNull"={
                showVals<-result$pnull
              },
              "S"={
                showVals<-result$S
              },
              
              "p(sig)"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                if (design$sBudgetOn) {
                  getStat<-function(x,n) {colMeans(x)*max(n)/colMeans(n)}
                } else {
                  getStat<-function(x,n) {colMeans(x)}
                }
                nulls<-result$rpval==0
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                if (any(!nulls)) {
                  showMeans<-getStat(abs(sigs & !nulls),nVals)
                  sigs0<-colMeans(abs(sigs & !nulls))
                  showSE<-sqrt(sigs0*(1-sigs0)/sum(!nulls))
                } else {
                  showMeans<-0
                  showSE<-NULL
                }
                if (any(nulls)) {
                  showMeans2<-getStat(abs(sigs & nulls),nVals)
                  sigs0<-colMeans(abs(sigs & nulls))
                  showSE2<-sqrt(sigs0*(1-sigs0)/sum(nulls))
                } else showMeans2<-0
                showMeans<-showMeans+showMeans2
                showSE<-NULL
              },
              "tDR"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                nulls<-result$rpval==0
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-1-colMeans(abs(sigs & nulls))/colMeans(abs(sigs))
                showSE<-NULL
              },
              "Hits"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                nulls<-result$rpval==0
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-colMeans(abs(sigs & !nulls))/colMeans(abs(sigs))
                showSE<-NULL
              },
              "Misses"={
                showVals<-NULL
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                nulls<-result$rpval==0
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                showMeans<-colMeans(abs(!sigs & !nulls))/colMeans(abs(!sigs))
                showSE<-NULL
              },
              
              "NHST"={
                if (explore$exploreType=="Alpha") {
                  braw.env$alphaSig<-exploreResult$vals
                }
                sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
                nulls<-rpVals==0
                if (braw.env$STMethod=="NHST") {
                  d<-sigs
                } else {
                  d<-r2llr(rVals,nVals,df1Vals,braw.env$STMethod,world=effect$world)
                }
                np<-sum(!is.na(pVals[,1]))
                sigNonNulls<- colSums( sigs & d>0 & !nulls,na.rm=TRUE)/np 
                nsigNonNulls<-colSums(!sigs &       !nulls,na.rm=TRUE)/np
                isigNonNulls<-colSums( sigs & d<0 & !nulls,na.rm=TRUE)/np 
                isigNulls<-   colSums( sigs & d<0 & nulls,na.rm=TRUE)/np 
                sigNulls<-    colSums( sigs & d>0 & nulls,na.rm=TRUE)/np 
                nsigNulls<-   colSums(!sigs &       nulls,na.rm=TRUE)/np 
                
                lines<-c(0.05)
              },
              "PDF"={
                showVals<-NULL
                showMeans<-colMeans(result$dists==effect$world$populationPDF,na.rm=TRUE)
                showSE<-sqrt(showMeans*(1-showMeans)/nrow(showVals))
              }
      )
      
      if (!is.element(showType[si],c("NHST"))) {
        # draw the basic line and point data
        if (is.element(showType[si],c("p(sig)","Hits","Misses"))) {
          y50<-showMeans
          y75<-NULL
        } else {
          y75<-apply( showVals , 2 , quantile , probs = 0.50+quants , na.rm = TRUE ,names=FALSE)
          y62<-apply( showVals , 2 , quantile , probs = 0.50+quants/2 , na.rm = TRUE ,names=FALSE)
          y50<-apply( showVals , 2 , quantile , probs = 0.50 , na.rm = TRUE ,names=FALSE)
          y38<-apply( showVals , 2 , quantile , probs = 0.50-quants/2 , na.rm = TRUE ,names=FALSE)
          y25<-apply( showVals , 2 , quantile , probs = 0.50-quants , na.rm = TRUE ,names=FALSE)
        }

        if (!isempty(y50)) {
          y50[y50>ylim[2]]<-ylim[2]
          y50[y50<ylim[1]]<-ylim[1]
          if (!is.null(y75)) {
            y75[y75>ylim[2]]<-ylim[2]
            y62[y62>ylim[2]]<-ylim[2]
            y38[y38>ylim[2]]<-ylim[2]
            y25[y25>ylim[2]]<-ylim[2]
            
            y75[y75<ylim[1]]<-ylim[1]
            y62[y62<ylim[1]]<-ylim[1]
            y38[y38<ylim[1]]<-ylim[1]
            y25[y25<ylim[1]]<-ylim[1]
          }
          
        if (doLine) {
          if (!is.null(y75)) {
            pts1f<-data.frame(x=c(vals,rev(vals)),y=c(y25,rev(y75)))
            pts2f<-data.frame(x=c(vals,rev(vals)),y=c(y38,rev(y62)))
            g<-g+dataPolygon(data=pts1f,fill=col,alpha=0.2,colour=NA)
            g<-g+dataPolygon(data=pts2f,fill=col,alpha=0.4,colour=NA)
          }
          pts0f<-data.frame(x=vals,y=y50)
          g<-g+dataLine(data=pts0f)
          g<-g+dataPoint(data=pts0f,fill=col,size=4)
        } else {
          pts0f<-data.frame(x=vals,y=y50)
          g<-g+dataLine(data=pts0f,linewidth=0.25)
          sigVals<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,braw.env$alphaSig)
          if (!is.null(showVals)) {
            for (i in 1:length(vals))
              g<-expected_plot(g,
                               data.frame(x=vals[i],y1=showVals[,i],y2=sigVals[,i]),
                               showType=showType[si],ylim=ylim,scale=2.25/(length(vals)+1),col=col,
                               npointsMax=200/length(vals))
          }
          g<-g+dataPoint(data=pts0f,fill=col,size=4)
          if (!is.null(y75)) {
          pts1f<-data.frame(x=vals,ymin=y25,ymax=y75)
          g<-g+dataErrorBar(pts1f)
          }
        } # end of line and point
        }
      } else {
        # now the NHST filled areas
        ytop<-1-sigNonNulls*0
        showSequence<-c("isigNonNulls","nsigNonNulls","sigNonNulls","sigNulls","nsigNulls","isigNulls")
        splitSequence<-c("sigNonNulls")
        
        for (use in showSequence) {
          switch(use,
                 "isigNulls"={valsD<-isigNulls;colShow<-col4},
                 "nsigNulls"={valsD<-nsigNulls;colShow<-desat(col3,0.5)},
                 "sigNulls"={valsD<-sigNulls;colShow<-col5},
                 "sigNonNulls"={valsD<-sigNonNulls;colShow<-col0},
                 "nsigNonNulls"={valsD<-nsigNonNulls;colShow<-desat(col2,0.5)},
                 "isigNonNulls"={valsD<-isigNonNulls;colShow<-col1}
                 )
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
          if (!is.null(colShow) && !is.null(ptsShow)) {
            if (showTheory) {
              g<-g+dataPoint(data=ptsShow,fill=colShow)
            } else {
              if (doLine) {
                g<-g+dataPolygon(data=ptsShow,fill=colShow,colour=NA)
              } else {
                  npts<-length(vals)
                  bwidth<-0.4*(ptsShow$x[2]-pts1$x[1])
                  for (i in 1:npts) {
                    g<-g+drawNHSTBar(i,npts,ptsShow,bwidth,colShow)
                  }
              }
            }
          }
          if (is.element(use,splitSequence)) g<-g+dataLine(data.frame(x=vals,y=ybottom),linewidth=1)
        }
      }
      
      # find n80
      if (showType[si]=="p(sig)" && explore$exploreType=="n" && effect$world$populationPDF=="Single" && showPower){
        w<-y50
        n<-exploreResult$vals
        minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
        r_est<-r_est$minimum
        nvals<-seq(min(n),max(n),length.out=101)
        yvals<-rn2w(r_est,nvals)
        if (braw.env$nPlotScale=="log10") ptsn<-data.frame(x=log10(nvals),y=yvals)
        else          ptsn<-data.frame(x=nvals,y=yvals)
        g<-g+dataLine(data=ptsn,colour="black",linetype="dotted",linewidth=0.25)
        
        minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        n80<-optimize(minnw,c(min(n),max(n)),w=0.8,r=r_est)
        
        if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
          label<-paste("n80 =",format(round(n80$minimum),digits=2))
        } else {
          if (sum(n<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
          if (sum(n>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        }
        if (braw.env$nPlotScale=="log10") lpts<-data.frame(x=log10(min(n)),y=0.8,label=label)
        else lpts<-data.frame(x=min(n),y=0.8,label=label)
        g<-g+dataLabel(data=lpts,label = label)
      }
      
      # find r80
      if (showType[si]=="p(sig)" && explore$exploreType=="rIV" && showPower){
        w<-y50
        r<-exploreResult$vals
        minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        n_est<-optimize(minrw,c(0,100),w=w,r=r)
        n_est<-n_est$minimum
        rvals<-seq(min(r),max(r),length.out=101)
        yvals<-rn2w(rvals,n_est)
        ptsn<-data.frame(x=rvals,y=yvals)
        g<-g+dataLine(data=ptsn,colour="white",linetype="dotted",linewidth=0.25)
        
        minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
        n80<-optimize(minnw,c(0,0.8),w=0.8,n=n_est)
        
        if (sum(r<n80$minimum)>=2 && sum(r>n80$minimum)>=2){
          label<-paste("r80 =",format(n80$minimum,digits=2))
        } else {
          if (sum(r<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
          if (sum(r>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        }
        lpts<-data.frame(x=0,y=0.8)
        g<-g+dataLabel(data=lpts,label = label)
      }
    }
    
    if (showType[si]=="NHST") {
      if (doLine) xoff<-0
      else        xoff<-bwidth
      
      if (!is.null(col0)) g<-g+drawNHSTLabel(lb0,lb0xy,xoff,col0)
      if (!is.null(col1)) g<-g+drawNHSTLabel(lb1,lb1xy,xoff,col1)
      if (!is.null(col2)) g<-g+drawNHSTLabel(lb2,lb2xy,xoff,col2)
      if (!is.null(col3)) g<-g+drawNHSTLabel(lb3,lb3xy,xoff,col3)
      if (!is.null(col4)) g<-g+drawNHSTLabel(lb4,lb4xy,xoff,col4)
      if (!is.null(col5)) g<-g+drawNHSTLabel(lb5,lb5xy,xoff,col5)
    }

    lineCol<-"black"
    if (is.element(showType[si],c("p","e1","e2","e1d","e2d"))) lineCol<-"green"
    for (yl in ylines) {
      g<-g+horzLine(yl,linetype="dotted",colour=lineCol)
    }
  }
  }
  if (exploreResult$count>0)
  g<-g+plotTitle(paste0("Explore: ",brawFormat(exploreResult$count)),"right",size=0.85)
  g
}

showExplore2D<-function(exploreResult=braw.res$explore,showType=c("rs","p"),showTheory=FALSE,
                 effectType="unique",whichEffect="All") {
  
  explore<-exploreResult$explore
  hypothesis<-exploreResult$hypothesis
  effect<-hypothesis$effect
  
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
            if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
          },
          "rp"={
            showVals<-rpVals
            if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
          },
          "re"={
            showVals<-rVals-rpVals
            if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
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
    switch (si,
            xVals<-apply(showVals,2,median),
            yVals<-apply(showVals,2,median)
    )
  }

  g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  
  xaxis<-plotAxis(showType[1],effect)
  xlim<-xaxis$lim
  xlabel<-xaxis$label
  xcols<-xaxis$cols
  xlines<-xaxis$lines
  xSecond<-NULL
  
  yaxis<-plotAxis(showType[2],effect)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ycols<-yaxis$cols
  ylines<-yaxis$lines
  ySecond<-NULL
  
  if (showType[1]=="p" && braw.env$pPlotScale=="log10" && any(exploreResult$result$pval>0)) 
    while (mean(log10(exploreResult$result$pval)>xlim[1])<0.75) xlim[1]<-xlim[1]-1
  if (showType[2]=="p" && braw.env$pPlotScale=="log10" && any(exploreResult$result$pval>0)) 
    while (mean(log10(exploreResult$result$pval)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
  
  braw.env$plotArea<-c(0,0,1,1)
  g<-startPlot(xlim,ylim,box="both",top=TRUE,tight=TRUE,g=g)
  g<-g+plotTitle(bquote(bold("explore: " ~ .(explore$exploreType))))
  g<-g+xAxisTicks(logScale=xaxis$logScale)+xAxisLabel(xlabel)
  g<-g+yAxisTicks(logScale=yaxis$logScale)+yAxisLabel(ylabel)
  
  lineCol<-"black"
  if (is.element(showType[1],c("p","e1","e2","e1d","e2d"))) lineCol<-"green"
  for (xl in xlines) {
    g<-g+vertLine(xl,linetype="dotted",colour=lineCol,linewidth=0.5)
  }
  lineCol<-"black"
  if (is.element(showType[2],c("p","e1","e2","e1d","e2d"))) lineCol<-"green"
  for (yl in ylines) {
    g<-g+horzLine(yl,linetype="dotted",colour=lineCol,linewidth=0.5)
  }
  
  g<-g+dataLine(data.frame(x=xVals,y=yVals))
  g<-g+dataPoint(data.frame(x=xVals,y=yVals))
  
  if (exploreResult$count>0)
    g<-g+plotTitle(paste0("Explore: ",brawFormat(exploreResult$count)),"right",size=0.85)
  
}
