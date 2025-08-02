
trimanalysis<-function(analysis) {
  
  if (all(is.na(analysis$rIV))) return(analysis)
  use<-(!is.na(analysis$rIV))
  
  analysis$rIV=analysis$rIV[use]
  analysis$pIV=analysis$pIV[use]
  analysis$rpIV=analysis$rpIV[use]
  analysis$roIV=analysis$roIV[use]
  analysis$poIV=analysis$poIV[use]
  analysis$nval=analysis$nval[use]
  analysis$df1=analysis$df1[use]
  
  if (!is.null(analysis$hypothesis$IV2)) {
    analysis$rIV2=analysis$rIV2[use]
    analysis$pIV2=analysis$pIV2[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$r$direct=matrix(analysis$r$direct[use,],nrow=sum(use))
    analysis$r$unique=matrix(analysis$r$unique[use,],nrow=sum(use))
    analysis$r$total=matrix(analysis$r$total[use,],nrow=sum(use))
    analysis$p$direct=matrix(analysis$p$direct[use,],nrow=sum(use))
    analysis$p$unique=matrix(analysis$p$unique[use,],nrow=sum(use))
    analysis$p$total=matrix(analysis$p$total[use,],nrow=sum(use))
  }
  
  analysis
}

plotInference<-function(analysis,otheranalysis=NULL,disp="rs",orientation="vert",
                        whichEffect="Main 1",effectType="all",
                        showTheory=braw.env$showTheory,showData=TRUE,showLegend=FALSE,
                        g=NULL){
  if (length(disp)==2) {
    return(plot2Inference(analysis,disp[1],disp[2]))
  } 
  analysis<-trimanalysis(analysis)
  switch (disp,
          "rs"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "rp"={g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "re"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "ro"={g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "ci1"={g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "ci2"={g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},

          "p"= {g<-p_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "ps"= {g<-ps_plot(analysis,disp,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "po"= {g<-p_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "metaRiv"={g<-r_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "metaRsd"={g<-r_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "metaBias"={g<-r_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "metaS"={g<-r_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "llknull"={g<-r_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "AIC"={g<-aic_plot(analysis,disp,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "SEM"={g<-sem_plot(analysis,disp,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "sLLR"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "log(lrs)"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "log(lrd)"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "ws"= {g<-w_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "wp"={g<-w_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "nw"={g<-n_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "n"= {g<-n_plot(analysis,disp,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "rse"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,g=g)},
          "sig"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,g=g)},
          "ns"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,g=g)},
          "nonnulls"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,g=g)},
          "nulls"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,g=g)},
          "rss"= {g<-r_plot(analysis,disp,orientation=orientation,whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,g=g)},
          "e1r"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e2r"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e1+"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e2+"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e1-"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e2-"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "ps1"= {g<-ps_plot(analysis,disp,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e1p"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e2p"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e1a"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e2a"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e1b"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "e2b"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "iv.mn"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "iv.sd"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "iv.sk"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "iv.kt"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "dv.mn"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "dv.sd"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "dv.sk"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "dv.kt"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          
          "er.mn"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "er.sd"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "er.sk"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)},
          "er.kt"={g<-var_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,g=g)}
  )
  return(g)
}


plot2Inference<-function(analysis,disp1,disp2,metaPlot=FALSE){
    
  r<-analysis$hypothesis$rIV
  if (!is.null(analysis$hypothesis$IV2)){
    r<-c(r,analysis$hypothesis$rIV2,analysis$hypothesis$rIVIV2DV)
  }

  sequence<-analysis$sequence
  # if (analysis$design$Replication$On) {
  #   pvals<-analysis$ResultHistory$pIV
  #   rvals<-analysis$ResultHistory$rIV
  #   nvals<-analysis$ResultHistory$nval
  #   df1vals<-analysis$ResultHistory$df1
  #   if (!is.element(analysis$design$Replication$Keep,c("SmallP")))
  #     sequence<-TRUE
  #   else sequence<-FALSE
  # } else {
    pvals<-analysis$pIV
    rvals<-analysis$rIV
    nvals<-analysis$nval
    df1vals<-analysis$df1
  # }
  
  xaxis<-plotAxis(disp1,analysis$hypothesis,analysis$design,result=analysis)
  yaxis<-plotAxis(disp2,analysis$hypothesis,analysis$design,result=analysis)
  switch (disp1,
          "rs"={
            d1<-rvals
          },
          "p"={
            d1<-pvals
            if (braw.env$pPlotScale=="log10") d1<-log10(d1)
            },
          "rp"={
            d1<-analysis$rpIV
          },
          "re"={
            d1<-rvals-analysis$rpIV
          },
          "ro"={
            d1<-analysis$roIV
          },
          "po"={
            d1<-analysis$poIV
            if (braw.env$pPlotScale=="log10") d1<-log10(d1)
          },
          "n"={
            d1<-nvals
            if (braw.env$nPlotScale=="log10") d1<-log10(d1)
          },
          "no"={
            d1<-analysis$noval
            if (braw.env$nPlotScale=="log10") d1<-log10(d1)
          },
          "metaRiv"={
            d1<-analysis$best$param1
          },
          "metaRsd"={
            d1<-analysis$best$param2
          },
          "metaBias"={
            d1<-analysis$best$param3
          },
          "metaS"={
            d1<-analysis$best$S
          },
          "llknull"=d1<-(-0.5*(analysis$AIC-analysis$AICnull)),
          "sLLR"=d1<-res2llr(analysis,"sLLR"),
          "log(lrs)"=d1<-res2llr(analysis,"sLLR"),
          "log(lrd)"=d1<-res2llr(analysis,"dLLR"),
          "ws"={
            d1<-rn2w(analysis$rIV,analysis$nval)
            if (braw.env$wPlotScale=="log10") d1<-log10(d1)
          },
          "wp"={
            d1<-rn2w(analysis$rp,analysis$nval)
            if (braw.env$wPlotScale=="log10") d1<-log10(d1)
          },
          "nw"={
            d1<-rw2n(analysis$rIV,0.8,analysis$design$Replication$Tails)
            if (braw.env$wPlotScale=="log10") d1<-log10(d1)
          }
  )
  if (is.element(disp1,c("rs","rp","re","ro","metaRiv","metaRsd")))
    switch(braw.env$RZ,
           "r"={},
           "z"={d1<-atanh(d1)}
           )

  switch (disp2,
          "rs"={
            d2<-rvals
          },
          "p"={
            d2<-pvals
            if (braw.env$pPlotScale=="log10") d2<-log10(d2)
          },
          "rp"={
            d2<-analysis$rpIV
          },
          "re"={
            d2<-rvals-analysis$rpIV
          },
          "ro"={
            d2<-analysis$roIV
          },
          "po"={
            d2<-analysis$poIV
            if (braw.env$pPlotScale=="log10") d2<-log10(d2)
          },
          "n"={
            d2<-nvals
            if (braw.env$nPlotScale=="log10") d2<-log10(d2)
          },
          "no"={
            d2<-analysis$noval
            if (braw.env$nPlotScale=="log10") d2<-log10(d2)
          },
          "metaRiv"={
            d2<-analysis$best$param1
          },
          "metaRsd"={
            d2<-analysis$best$param2
          },
          "metaBias"={
            d2<-analysis$best$param3
          },
          "metaS"={
            d2<-analysis$best$S
          },
          "llknull"=d2<-(-0.5*(analysis$AIC-analysis$AICnull)),
          "sLLR"=d2<-res2llr(analysis,"sLLR"),
          "log(lrs)"=d2<-res2llr(analysis,"sLLR"),
          "log(lrd)"=d2<-res2llr(analysis,"dLLR"),
          "ws"={
            d2<-rn2w(analysis$rIV,analysis$nval)
            if (braw.env$wPlotScale=="log10") d2<-log10(d2)
          },
          "wp"={
            d2<-rn2w(analysis$rp,analysis$nval)
            if (braw.env$wPlotScale=="log10") d2<-log10(d2)
          },
          "nw"={
            d2<-rw2n(analysis$rIV,0.8,analysis$design$Replication$Tails)
            if (braw.env$wPlotScale=="log10") d2<-log10(d2)
          }
  )
  if (is.element(disp2,c("rs","rp","re","ro","metaRiv","metaRsd")))
    switch(braw.env$RZ,
           "r"={},
           "z"={d2<-atanh(d2)}
    )
  
  pts<-data.frame(x=d1,y=d2)
  labels<-1:length(d1)
  if (analysis$design$Replication$On) {
    labels<-c("Original",rep("Replication",length(d1)-1))
    if (length(d1)>2)
      labels<-c("0",rep("",length(d1)-2),"final")
    else 
      labels<-c("0","final")
    # if (analysis$design$Replication$Keep=="MetaAnalysis")
    #   labels[length(d1)]<-"Combined"
  }
  braw.env$plotArea<-c(0,0,1,1)
  g<-startPlot(xaxis$lim,yaxis$lim,
               xticks=makeTicks(logScale=xaxis$logScale),xlabel=makeLabel(xaxis$label),
               yticks=makeTicks(logScale=yaxis$logScale),ylabel=makeLabel(yaxis$label),
               top=FALSE,g=NULL)
  # g<-addG(g,xAxisTicks(logScale=xaxis$logScale),xAxisLabel(xaxis$label))
  # g<-addG(g,yAxisTicks(logScale=yaxis$logScale),yAxisLabel(yaxis$label))

  if (disp1=="rs" && disp2=="p") {
    rs<-seq(-braw.env$r_range,braw.env$r_range,length.out=51)
    ps<-r2p(rs,analysis$nval[1])
    if (braw.env$pPlotScale=="log10")  ps<-log10(ps)
    g<-addG(g,dataLine(data=data.frame(x=rs,y=ps),col="white"))
  }
 if (disp2=="p") {
   ps<-0.05
   if (braw.env$pPlotScale=="log10")  ps<-log10(ps)
   g<-addG(g,horzLine(ps,linetype="dotted",colour=braw.env$plotColours$infer_sigC,linewidth=1))
 }
  dotSize<-braw.env$dotSize

  if (!metaPlot && braw.env$useSignificanceCols){
    c1=braw.env$plotColours$infer_sigC
    c2=braw.env$plotColours$infer_nsigC
  } else {
    c1=braw.env$plotColours$descriptionC
    c2=braw.env$plotColours$descriptionC
  }
  
  shape<-braw.env$plotShapes$study
  # if (length(d1)<=200) gain<-0 else gain<-(length(d1)-200)/500
  use<-!isSignificant(braw.env$STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
  if (length(use)==0) { 
    use<-rep(FALSE,length(d1))
    shape<-braw.env$plotShapes$meta
  }
  np<-nrow(pts)
  if (np>1) dotSize<-dotSize*0.65
  alpha<-max(0.5,min(1,50/np))
  if (np>50) {b1<-c1;b2<-c2} else {b1<-b2<-"#000000"}
  last<-length(pts$x)
  if (!use[last]) colour<-c(b1,c1) else colour<-c(b2,c2)
  pts1<-pts
  g<-addG(g,dataPoint(data=pts1[last,],shape=shape, colour = colour[1], fill = colour[2], alpha=alpha, size = dotSize*1.2))
  pts1$x<-pts1$x+diff(xaxis$lim)*0.025
  if (sequence && nchar(labels[last])>0)
    g<-addG(g,dataLabel(data=pts1[last,],labels[last],vjust=0.5,size=0.75))
  if (last>1) {
    use<-use[1:(last-1)]
    pts1<-pts[1:(last-1),]
    labels<-labels[1:(last-1)]
    if (any(use)) {
      g<-addG(g,dataPoint(data=pts1[use,],shape=shape, colour = b2, fill = c2, alpha=alpha, size = dotSize))
      pts1$x<-pts1$x+diff(xaxis$lim)*0.025
      if (sequence)
        g<-addG(g,dataLabel(data=pts1[use,],labels[use],vjust=0.5,size=0.75))
      pts1$x<-pts1$x-diff(xaxis$lim)*0.025
    }
    if (any(!use)) {
      g<-addG(g,dataPoint(data=pts1[!use,],shape=shape, colour = b1, fill = c1, alpha=alpha, size = dotSize))
      pts1$x<-pts1$x+diff(xaxis$lim)*0.025
      if (sequence)
        g<-addG(g,dataLabel(data=pts1[!use,],labels[!use],vjust=0.5,size=0.75))
    }
  }
  if (sequence) {
      if (analysis$design$Replication$On && analysis$design$Replication$Keep=="MetaAnalysis") 
        pts<-pts[1:(last-1),]
    g<-addG(g,dataPath(data=pts,arrow=TRUE,linewidth=0.75,colour=braw.env$plotColours$sampleC))
  }
  return(g)
}
