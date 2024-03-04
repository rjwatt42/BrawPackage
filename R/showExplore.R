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
            hjust=0,vjust=0.5,
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
#' @param showType        "r","p","n","w", "p(sig)" \cr
#' "NHST", "FDR","FMR"
#' @return ggplot2 object - and printed
#' @examples
#' showExplore(exploreResult=makeExplore(),
#'                        showType="r",
#'                        ylog=FALSE,
#'                        effectType="unique",whichEffect="All")
#' @export
showExplore<-function(exploreResult=makeExplore(autoShow=TRUE),showType="r",ylog=FALSE,
                      effectType="unique",whichEffect="All"){
  quants<-0.25
  
  explore<-exploreResult$explore
  hypothesis<-explore$hypothesis
  effect<-hypothesis$effect
  design<-explore$design
  evidence<-explore$evidence
  
  result<-trimExploreResult(exploreResult$result,exploreResult$nullresult)
  
  oldAlpha<-braw.env$alphaSig
  on.exit(braw.env$alphaSig<-oldAlpha)
  
  vals<-exploreResult$vals
  if (is.character(vals[1]) || is.element(explore$exploreType,c("IVcats","IVlevels","DVcats","DVlevels","Repeats","sig_only"))) {
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

  yaxis<-showAxis(showType,effect)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ybreaks<-NULL
  ycols<-yaxis$cols
  ylines<-yaxis$lines
  ySecond<-NULL

  if (!is.null(hypothesis$IV2) && whichEffect=="All") {
    plots<-c(0,0.33,0.65)
    plotWidth<-0.35
  } else {
    plots<-0
    plotWidth<-1
    if (!is.null(hypothesis$IV2)) 
      switch (whichEffect,
              "Main 1"=whichEffect<-1,
              "Main 2"=whichEffect<-2,
              "rIVIV2DV"=whichEffect<-3
      )
    else whichEffect<-1
  }
  
  g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0, 1))+braw.env$blankTheme
  for (whichEffect in 1:length(plots)) {
  braw.env$plotArea<-c(plots[whichEffect],0,plotWidth,1)
  g<-startPlot(xlim,ylim,top=TRUE,g=g)
  g<-g+xAxisLabel(bquote(bold(.(explore$exploreType))))+xAxisTicks(xbreaks,xnames,logScale=explore$xlog)
  g<-g+yAxisLabel(ylabel)+yAxisTicks(ybreaks,logScale=yaxis$logScale)
  
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
  
  col<-ycols[1]
  switch (showType,
          "r"={
            showVals<-rVals
            if (braw.env$RZ=="z") {showVals<-atanh(showVals)}
          },
          "p"={
            showVals<-pVals
            if (braw.env$pPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          },
          "n"={
            showVals<-result$nval
          },
          "w"={
            showVals<-rn2w(rVals,result$nval)
            if (braw.env$wPlotScale=="log10"){
              showVals<-log10(showVals)
            }
          },
          "nw"={
            showVals<-rw2n(rVals,0.8,2)
            if (braw.env$wPlotScale=="log10"){
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
          "k"={
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
            } else showMeans<-NULL
            if (any(nulls)) {
              showMeans2<-getStat(abs(sigs & nulls),nVals)
              sigs0<-colMeans(abs(sigs & nulls))
              showSE2<-sqrt(sigs0*(1-sigs0)/sum(nulls))
            } else showMeans2<-NULL
          },
          "FDR"={
            showVals<-NULL
            if (explore$exploreType=="Alpha") {
              braw.env$alphaSig<-exploreResult$vals
            }
            nulls<-result$rpval==0
            sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
            showMeans<-colMeans(abs(sigs & nulls))/colMeans(abs(sigs))
            showSE<-sqrt(showMeans*(1-showMeans)/sum(sigs))
          },
          "FMR"={
            showVals<-NULL
            if (explore$exploreType=="Alpha") {
              braw.env$alphaSig<-exploreResult$vals
            }
            nulls<-result$rpval==0
            sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
            showMeans<-colMeans(abs(!sigs & !nulls))/colMeans(abs(!sigs))
            showSE<-sqrt(showMeans*(1-showMeans)/sum(!sigs))
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
  
  if (explore$exploreType=="rIV") {
    if (braw.env$RZ=="z") {
      vals<-atanh(vals)
      xLabel<-braw.env$zpLabel
    } else {
      xLabel<-braw.env$rpLabel
    }
  }
  
  if (!is.element(showType,c("NHST"))) {
    # draw the basic line and point data
    if (!is.null(showVals)) {
      y75<-apply( showVals , 2 , quantile , probs = 0.50+quants , na.rm = TRUE ,names=FALSE)
      y62<-apply( showVals , 2 , quantile , probs = 0.50+quants/2 , na.rm = TRUE ,names=FALSE)
      y50<-apply( showVals , 2 , quantile , probs = 0.50 , na.rm = TRUE ,names=FALSE)
      y38<-apply( showVals , 2 , quantile , probs = 0.50-quants/2 , na.rm = TRUE ,names=FALSE)
      y25<-apply( showVals , 2 , quantile , probs = 0.50-quants , na.rm = TRUE ,names=FALSE)
    } else {
      y75<-showMeans+showSE*2
      y62<-showMeans+showSE
      y50<-showMeans
      y38<-showMeans-showSE
      y25<-showMeans-showSE*2
    }
    
    y75[y75>ylim[2]]<-ylim[2]
    y62[y62>ylim[2]]<-ylim[2]
    y38[y38>ylim[2]]<-ylim[2]
    y25[y25>ylim[2]]<-ylim[2]
    
    y75[y75<ylim[1]]<-ylim[1]
    y62[y62<ylim[1]]<-ylim[1]
    y38[y38<ylim[1]]<-ylim[1]
    y25[y25<ylim[1]]<-ylim[1]
    y50[y50<ylim[1]]<-NA
    
    pts1<-data.frame(vals=vals,y25=y25,y38=y38,y50=y50,y62=y62,y75=y75)
    
    if (doLine) {
      pts0f<-data.frame(x=vals,y=y50)
      pts1f<-data.frame(x=c(vals,rev(vals)),y=c(y25,rev(y75)))
      pts2f<-data.frame(x=c(vals,rev(vals)),y=c(y38,rev(y62)))
      g<-g+dataPolygon(data=pts1f,fill=col,alpha=0.2,colour=NA)
      g<-g+dataPolygon(data=pts2f,fill=col,alpha=0.4,colour=NA)
      g<-g+dataLine(data=pts0f)
      g<-g+dataPoint(data=pts0f,fill=col)
    } else {
      pts0f<-data.frame(x=vals,y=y50)
      pts1f<-data.frame(x=vals,ymin=y25,ymax=y75)
      g<-g+dataLine(data=pts0f,linewidth=0.25)
      sigVals<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,braw.env$alphaSig)
      for (i in 1:length(vals))
      g<-expected_plot(g,
                       data.frame(x=vals[i],y1=showVals[,i],y2=sigVals[,i]),
                       showType=showType,scale=2.25/(length(vals)+1),col=col)
      g<-g+dataPoint(data=pts0f,fill=col)
      g<-g+dataErrorBar(pts1f)
    } # end of line and point
  } else {
    # now the NHST filled areas
      ytop<-1-nsigNonNulls*0
      yn<-0.5
      
      col0<-braw.env$plotColours$infer_isigNonNull
      lb0<-braw.env$nonNullNegative
      switch (braw.env$STMethod,
              "NHST"={col1<-braw.env$plotColours$infer_nsNonNull},
              "sLLR"={col1<-braw.env$plotColours$infer_nsNonNull},
              "dLLR"={col1<-braw.env$plotColours$infer_nsdNonNull}
      )
      lb1<-braw.env$nonNullNS
      col2<-braw.env$plotColours$infer_sigNonNull
      lb2<-braw.env$nonNullPositive
      col3<-braw.env$plotColours$infer_isigNull
      lb3<-braw.env$nullNegative
      switch (braw.env$STMethod,
              "NHST"={col4<-braw.env$plotColours$infer_nsigNull},
              "sLLR"={col4<-braw.env$plotColours$infer_nsigNull},
              "dLLR"={col4<-braw.env$plotColours$infer_nsdNull})
      lb4<-braw.env$nullNS
      col5<-braw.env$plotColours$infer_sigNull
      lb5<-braw.env$nullPositive
      
      # error Z+
      if (any(isigNonNulls!=0)) {
        ybottom<-ytop-isigNonNulls
        ybottom[ybottom<0]<-0
        pts0<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
        lb0xy<-data.frame(x=max(vals),y=1-yn/10)
        yn<-yn+1
        ytop<-ybottom
      } else {pts0<-NULL}
      
      # false misses
      if (any(nsigNonNulls!=0)) {
        ybottom<-ytop-nsigNonNulls
        ybottom[ybottom<0]<-0
        pts1<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
        lb1xy<-data.frame(x=max(vals),y=1-yn/10)
        yn<-yn+1
        ytop<-ybottom
      } else {pts1<-NULL}
      
      # true hits
      if (any(sigNonNulls!=0)) {
        ybottom<-ytop-sigNonNulls
        ybottom[ybottom<0]<-0
        pts2<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
        lb2xy<-data.frame(x=max(vals),y=1-yn/10)
        yn<-yn+1
        ytop<-ybottom
      } else {pts2<-NULL}
      nonNullsLine<-data.frame(x=vals,y=ybottom)
      
      yn<-any(isigNulls!=0)+any(nsigNulls!=0)+any(sigNulls!=0)-0.5
      # false hits
      if (any(isigNulls!=0)) {
        ybottom<-ytop-isigNulls
        ybottom[ybottom<0]<-0
        pts3<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
        lb3xy<-data.frame(x=max(vals),y=0+yn/10)
        yn<-yn-1
        ytop<-ybottom
      } else {pts3<-NULL}
      
      # true misses
      if (any(nsigNulls!=0)) {
        ybottom<-ytop-nsigNulls
        ybottom[ybottom<0]<-0
        pts4<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
        lb4xy<-data.frame(x=max(vals),y=0+yn/10)
        yn<-yn-1
        ytop<-ybottom
      } else {pts4<-NULL}
      
      # error Z0
      if (any(sigNulls!=0)) {
        ybottom<-ytop-sigNulls
        ybottom[ybottom<0]<-0
        pts5<-data.frame(x=c(vals,rev(vals)),y=c(ybottom,rev(ytop)))
        lb5xy<-data.frame(x=max(vals),y=0+yn/10)
        yn<-yn-1
        ytop<-ybottom
      } else {pts5<-NULL}

    if (doLine) {
      #   errors
      if (!is.null(pts0)) g<-g+dataPolygon(data=pts0,fill=col0,colour=NA)
      if (!is.null(pts1)) g<-g+dataPolygon(data=pts1,fill=col1,colour=NA)
      if (!is.null(pts2)) g<-g+dataPolygon(data=pts2,fill=col2,colour=NA)
      if (!is.null(pts3)) g<-g+dataPolygon(data=pts3,fill=col3,colour=NA)
      if (!is.null(pts4)) g<-g+dataPolygon(data=pts4,fill=col4,colour=NA)
      if (!is.null(pts5)) g<-g+dataPolygon(data=pts5,fill=col5,colour=NA)
      if (!is.null(pts1)) {
        use<-1:(nrow(pts1)/2)
        g<-g+dataLine(data=pts1[use,],colour=col1)
      }
      if (!is.null(pts5)) {
        use<-1:(nrow(pts5)/2)
        g<-g+dataLine(data=pts5[nrow(pts5)/2+use,],colour=col5)
      }
    } else {
      npts<-length(vals)
      bwidth<-0.4*(pts1$x[2]-pts1$x[1])
      for (i in 1:npts) {
        if (!is.null(pts0)) g<-g+drawNHSTBar(i,npts,pts0,bwidth,col0)
        if (!is.null(pts1)) g<-g+drawNHSTBar(i,npts,pts1,bwidth,col1)
        if (!is.null(pts2)) g<-g+drawNHSTBar(i,npts,pts2,bwidth,col2)
        if (!is.null(pts3)) g<-g+drawNHSTBar(i,npts,pts3,bwidth,col3)
        if (!is.null(pts4)) g<-g+drawNHSTBar(i,npts,pts4,bwidth,col4)
        if (!is.null(pts5)) g<-g+drawNHSTBar(i,npts,pts5,bwidth,col5)
      }
    }
    
    if (showType=="NHST") {
      g<-g+dataLine(nonNullsLine)
      # g<-g+horzLine(intercept=1)
      # g<-g+horzLine(intercept=0)
      
      if (doLine) xoff<-0
      else        xoff<-bwidth
      
      if (!is.null(pts0)) g<-g+drawNHSTLabel(lb0,lb0xy,xoff,col0)
      if (!is.null(pts1)) g<-g+drawNHSTLabel(lb1,lb1xy,xoff,col1)
      if (!is.null(pts2)) g<-g+drawNHSTLabel(lb2,lb2xy,xoff,col2)
      if (!is.null(pts3)) g<-g+drawNHSTLabel(lb3,lb3xy,xoff,col3)
      if (!is.null(pts4)) g<-g+drawNHSTLabel(lb4,lb4xy,xoff,col4)
      if (!is.null(pts5)) g<-g+drawNHSTLabel(lb5,lb5xy,xoff,col5)
    }
  }
  
  # effect size vs effect size line
  if (is.element(showType,c("r","rIVIV2DV")) && is.element(explore$exploreType,c("rIV","rIV2","rIVIV2DV"))){
    pts3<-data.frame(x=c(-1,1),y=c(-1,1))
    g<-g+dataLine(data=pts3,colour="white", linetype="dotted")
  }
  
  # find n80
  if (showType=="p(sig)" && explore$exploreType=="n" && effect$world$populationPDF=="Single"){
    w<-y50
    n<-exploreResult$vals
    minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
    r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
    r_est<-r_est$minimum
    nvals<-seq(min(n),max(n),length.out=101)
    yvals<-rn2w(r_est,nvals)
    ptsn<-data.frame(x=nvals,y=yvals)
    g<-g+dataLine(data=ptsn,colour="white",linewidth=0.5)
    
    minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
    n80<-optimize(minnw,c(explore$min_n,explore$max_n),w=0.8,r=r_est)
    
    if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
      label<-paste("n80 =",format(round(n80$minimum),digits=2))
    } else {
      if (sum(n<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
      if (sum(n>n80$minimum)<2) label<-paste("Unsafe result - increase range")
    }
    lpts<-data.frame(x=min(n),y=0.8,label=label)
    g<-g+dataLabel(data=lpts,label = label)
  }
  
  # find r80
  if (showType=="p(sig)" && explore$exploreType=="rIV"){
    w<-y50
    r<-exploreResult$vals
    minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
    n_est<-optimize(minrw,c(0,100),w=w,r=r)
    n_est<-n_est$minimum
    rvals<-seq(min(r),max(r),length.out=101)
    yvals<-rn2w(rvals,n_est)
    ptsn<-data.frame(x=rvals,y=yvals)
    g<-g+dataLine(data=ptsn,colour="white")
    
    minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
    n80<-optimize(minnw,c(0,0.8),w=0.8,n=n_est)
    
    if (sum(r<n80$minimum)>=2 && sum(r>n80$minimum)>=2){
      label<-paste("n80 =",format(n80$minimum,digits=2))
    } else {
      if (sum(r<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
      if (sum(r>n80$minimum)<2) label<-paste("Unsafe result - increase range")
    }
    lpts<-data.frame(x=0,y=0.8+(ni_max2-1)/10)
    g<-g+dataLabel(data=lpts,label = label)
  }
  
  lineCol<-"black"
  if (is.element(showType,c("p","e1","e2","e1d","e2d"))) lineCol<-"green"
  for (yl in ylines) {
    g<-g+horzLine(yl,linetype="dotted",colour=lineCol)
  }
  }
  g<-g+plotTitle(paste0("Explore: ",format(exploreResult$count)),"right")
  g
}

