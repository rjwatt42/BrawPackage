
makeMetaHist<-function(vals,use,xlim) {
  nbins<-10
  bins<-seq(xlim[1],xlim[2],length.out=nbins+1)
  dens<-hist(vals[use],bins,plot=FALSE)$counts/length(vals)
  h<-list(bins=bins,dens=dens)
}

worldLabel<-function(metaResult,whichMeta=NULL,modelPDF=NULL) {
  if (is.null(whichMeta)) whichMeta<-metaResult$best$dist
  if (whichMeta=="world") Dist<-modelPDF
  else Dist<-whichMeta
  Dist<-tolower(Dist)
  p1<-metaResult[[Dist]]$param1
  p2<-metaResult[[Dist]]$param2
  p3<-metaResult[[Dist]]$param3
  if (whichMeta!="world")
    switch(braw.env$RZ,
           "r"={},
           "z"={
             p1<-atanh(p1)
             p2<-atanh(p2)
           },
           "d"={
             p1<-2*p1/sqrt(1-p1^2)
             p2<-2*p2/sqrt(1-p2^2)
           }
    )
  
  if (is.element(Dist,c("random","fixed"))) label1<-paste0(braw.env$RZ,"[m]") 
  else                                      label1<-Dist
  lb<-paste0(label1,"=",brawFormat(mean(p1,na.rm=TRUE),digits=3))
  # if (length(p1)>1)
  #   lb<-paste0(lb,"\u00B1",brawFormat(std(p1),digits=2))
  if (!is.null(p2)) {
    label2<-braw.env$Pchar
    if (is.element(Dist,c("random","fixed"))) label2<-paste0(braw.env$RZ,"[sd]")
    lb<-paste0(lb,"\n",label2,"=",brawFormat(mean(p2,na.rm=TRUE),digits=3))
    # if (length(p2)>1)
    #   lb<-paste0(lb,"\u00B1",brawFormat(std(p2),digits=2))
  }
  if (is.element(Dist,c("random","fixed")))
    if (!is.null(p3)) {
      label3<-"bias[m]"
      lb<-paste0(lb,"\n",label3,"=",brawFormat(mean(p3,na.rm=TRUE),digits=3))
    }
  label4<-"S[max]"
  lb<-paste0(lb,"\n",label4,"=",brawFormat(metaResult[[Dist]]$Smax,digits=3))
  
  return(lb)
}

#' show a single meta-analysis 
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showMetaSingle(metaResult=doMetaAnalysis(),showType="n",showTheory=FALSE)
#' @export
showMetaSingle<-function(metaResult=braw.res$metaSingle,showType="n",
                         showTheory=TRUE,xRange="full",autoYlim=TRUE,
                         fill=NULL,alpha=NULL) {
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  oldminN<-braw.env$minN
  oldmaxN<-braw.env$maxN
  # on.exit({setBrawEnv("minN",oldminN);setBrawEnv("maxN",oldmaxN)})
  
  showSval<-FALSE
  showSig<-TRUE
  svalExponent<-1
  showLines<-FALSE # in jamovi the code for lines is very slow
  
  metaAnalysis<-metaResult$metaAnalysis
  hypothesis<-metaResult$hypothesis
  design<-metaResult$design
  evidence<-metaResult$evidence
  # setBrawEnv("RZ","z")
  
  d1<-metaResult$result$rIV
  switch(braw.env$RZ,
         "r"={},
         "z"={d1<-atanh(d1)},
         "d"={d1<-2*d1/sqrt(1-d1^2)}
  )
  d1n<-(abs(metaResult$result$rpIV)<=evidence$minRp & hypothesis$effect$world$On)
  x<-plotAxis("rs",hypothesis)
  xlim<-x$lim
  if (xRange!="full") {
    xlim[1]<-0
    x$ticks<-seq(0,1,0.1)
  }
  disp1<-x$label
  
  if (showType=="n") {
    d2<-metaResult$result$nval
    y<-plotAxis("n",hypothesis)
    disp2<-y$label
    if (autoYlim) {
      ylim<-c(min(d2),max(d2))+c(-1,1)
      braw.env$minN<-ylim[1]
      braw.env$maxN<-ylim[2]
    }
    else
      ylim<-c(braw.env$minN-1,braw.env$maxN+1)
    yticks<-y$ticks
    if (y$logScale) {
      d2<-log10(d2)
      # ylim<-log10(ylim)
      # if (autoYlim) {
      #   ylim<-ylim+c(-1,1)*(ylim[2]-ylim[1])*0.05
      #   braw.env$minN<-10^ylim[1]
      #   braw.env$maxN<-10^ylim[2]
      # }
      # yticks<-makeTicks(10^yticks,logScale=TRUE)
      ytick<-c(1,2,5,10,20,50,100,200,500,1000)
      yticks<-makeTicks(ytick[ytick>ylim[1] & ytick<ylim[2]],logScale=TRUE)
    }
  } else {
    disp2<-"1/se"
    ylim<-sqrt(c(braw.env$minN,braw.env$maxN))
    ytick<-seq(ceil(sqrt(braw.env$minN)),floor(sqrt(braw.env$maxN)),1)
    yticks<-makeTicks(yticks)
    d2<-sqrt(metaResult$result$nval)
  }
  useAll<-(d2>ylim[1]) & (d2<ylim[2])
  ptsAll<-data.frame(x=d1[useAll],y=d2[useAll])
  useNull<-(d2>ylim[1]) & (d2<ylim[2] & d1n)
  ptsNull<-data.frame(x=d1[useNull],y=d2[useNull])
  
  assign("plotArea",c(0,0,1,1),braw.env)
  g<-startPlot(xlim,log10(ylim),
               xticks=makeTicks(x$ticks),xlabel=makeLabel(disp1),
               yticks=yticks,
               ylabel=makeLabel(disp2),
               top=1,g=NULL)
  if (showTheory) 
    g<-addG(g,plotTitle(paste0("Method=",metaResult$metaAnalysis$method),size=0.75))
  
  if (showTheory)
  g<-drawWorld(hypothesis,design,metaResult,showType,g,
               braw.env$plotColours$metaAnalysisTheory,
               # sigOnly=metaAnalysis$analyseBias,
               showTheory=showTheory,svalExponent=svalExponent,showLines=showLines)
  if (showSig && metaAnalysis$analyseBias) {
    nv<-10^seq(log10(braw.env$minN),log10(braw.env$maxN),length.out=101)
    rv<-p2r(0.05,nv,1)
    switch(braw.env$RZ,
           "r"={},
           "z"={rv<-atanh(rv)},
           "d"={rv<-2*rv/sqrt(1-rv^2)}
    )
    if (showType!="n") nv<-sqrt(nv)
    else {if (braw.env$nPlotScale=="log10") {nv<-log10(nv)}}
    use<-(nv<ylim[2] & nv>ylim[1])
    g<-addG(g,dataLine(data.frame(x=rv[use],y=nv[use]),
                       colour=darken(braw.env$plotColours$infer_nsigC,off=-0.1),
                       linewidth=1))
    g<-addG(g,dataLine(data.frame(x=-rv[use],y=nv[use]),
                       colour=darken(braw.env$plotColours$infer_nsigC,off=-0.1),
                       linewidth=1))
  }
  
  # show individual studies
  if (length(d1)<1200) {
  colgain<-1-min(1,sqrt(max(0,(length(d1)-50))/200))
  alphaUse<-1/(max(1,sqrt(length(d1)/100)))
  dotSize<-min(4,braw.env$dotSize*alphaUse*4)
  fill1<-rep(braw.env$plotColours$metaAnalysis,length(ptsAll$x))
  fill2<-braw.env$plotColours$infer_nsigC
  if (showSval) {
    b<-getLogLikelihood(atanh(metaResult$result$rIV),metaResult$result$nval,rep(1,length(metaResult$result$nval)),
                        distribution=metaResult$best$dist,
                        location=metaResult$best$param1,spread=metaResult$best$param2,
                        bias=metaResult$metaAnalysis$analyseBias,returnVals = TRUE)
    fill1<-hsv(0.9*round((b-min(b))/(max(b)-min(b))*4)/4)
    fill1<-hsv(0.9*round((b/max(b))^svalExponent*10)/10)
  }
  col1<-hsv(1,0,1-alphaUse)
  col2<-fill2
  if (!is.null(alpha)) alphaUse<-alpha
  if (!is.null(fill)) fill1<-fill
  g<-addG(g,dataPoint(data=ptsAll, shape=braw.env$plotShapes$study, colour = col1, fill = fill1, alpha=alphaUse, size = dotSize))
  if (nrow(ptsNull)>0)
    g<-addG(g,dataPoint(data=ptsNull,shape=braw.env$plotShapes$study, colour = col2, fill = fill2, alpha=alphaUse, size = dotSize))
  } else {
    rBins<-seq(0,1,length.out=31)
    nBins<-seq(log10(5),log10(500),length.out=21)
    z<-matrix(0,length(nBins)-1,length(rBins)-1)
    for (ir in 1:(length(rBins)-1)) {
      for (inv in 1:(length(nBins)-1)) {
        z[inv,ir]<-sum(metaResult$result$nval>10^nBins[inv] & metaResult$result$nval<=10^nBins[inv+1] &
                         metaResult$result$rIV>rBins[ir] & metaResult$result$rIV<=rBins[ir+1] ,
                       na.rm=TRUE)
      }
    }
    if (is.null(fill)) fill<-braw.env$plotColours$metaAnalysis
    g<-addG(g,dataContour(data=list(x=rBins[1:(length(rBins)-1)],
                                          y=nBins[1:(length(nBins)-1)],
                                          z=z),
                          fill=fill))
  }
  
  if (showTheory) {
  if (metaAnalysis$modelPDF=="All") metaAnalysis$modelPDF<-metaResult$best$dist
  lb<-worldLabel(metaResult,metaAnalysis$analysisType,metaAnalysis$modelPDF)
  names=strsplit(lb,"\n")[[1]]
  if (length(names)==1) colours=braw.env$plotColours$metaAnalysis else colours=c(braw.env$plotColours$metaAnalysis,rep(NA,length(names)-1))
  g<-addG(g,dataLegend(data.frame(names=names,colours=colours),title="",shape=22))
  # g<-addG(g,plotTitle(lb,"left",size=1))
  }
  
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

#' show a multiple meta-analyses
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showMetaMultiple<-function(metaResult=doMetaAnalysis(),showType=NULL,dimension="2D")
#' @export
showMetaMultiple<-function(metaResult=braw.res$metaMultiple,showType=NULL,dimension="1D") {
  if (is.null(metaResult)) metaResult<-doMetaMultiple()
  
  if (is.null(showType)) {
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={
             showType<-"metaRiv;metaS"
             if (metaResult$metaAnalysis$analyseBias) showType<-"metaRiv;metaBias"
           },
           "random"={
             showType<-"metaRiv;metaRsd"
           },
           "world"={
             if (metaResult$metaAnalysis$modelPDF=="All") showType<-"All"
             else showType="metaK;null"
           })
  }
  
  if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
    autoPrintOld<-braw.env$autoPrint
    on.exit(setBrawEnv("autoPrint",autoPrintOld))
    setBrawEnv("autoPrint",FALSE)
    g<-showMultiple(metaResult,showType=showType,dimension=dimension)
  } else {
    switch(showType,
           "metaS;metaS"={
             braw.env$plotArea<-c(0,0,1,1)
             g<-drawMeta(metaResult=metaResult,showType=showType,g=NULL)
           },
           "metaK;null"={
             braw.env$plotArea<-c(0,0,1,1)
             g<-drawMeta(metaResult=metaResult,whichMeta=metaResult$metaAnalysis$modelPDF,showType=showType,g=NULL)
           },
           {
             g<-nullPlot()
             if (braw.env$includeSingle) {
               braw.env$plotArea<-c(0,0,0.39,1)
               if (!all(is.na(metaResult$single$Smax)))
                 g<-drawMeta(metaResult=metaResult,whichMeta="Single",showType="metaK;null",g)
               xoff<-0.41
               xsize<-0.27
               xgap<-0
             } else {
               xoff<-0
               xsize<-0.42
               xgap<-0.1
             }
             braw.env$plotArea<-c(xoff,0,xsize+xgap,1)
             if (!all(is.na(metaResult$gauss$Smax)))
               g<-drawMeta(metaResult=metaResult,whichMeta="Gauss",showType="metaK;null",g)
             braw.env$plotArea<-c(xoff+xsize+xgap+0.02,0,xsize,1)
             if (!all(is.na(metaResult$exp$Smax)))
               g<-drawMeta(metaResult=metaResult,whichMeta="Exp",showType="metaK;null",g)
           }
    )
  }
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
  
  drawMeta<-function(metaResult=doMetaMultiple(),whichMeta="Single",showType="metaK;null",g=NULL) {
    
    metaAnalysis<-metaResult$metaAnalysis
    
    xlim<-c(-1,1)
    xticks<-seq(-1,1,0.5)
    
    if (is.element(whichMeta,c("Single","Gauss","Exp","Gamma","GenExp"))) {
      n1<-sum(metaResult$best$dist=="Single")
      n2<-sum(metaResult$best$dist=="Gauss")
      n3<-sum(metaResult$best$dist=="Exp")
      n4<-sum(metaResult$best$dist=="Gamma")
      n5<-sum(metaResult$best$dist=="GenExp")
      sAll<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax,metaResult$gamma$Smax,metaResult$genexp$Smax)
      
      use<-order(c(n1,n2,n3,n4,n5))
      use1<-c("Single","Gauss","Exp","Gamma","GenExp")[use[4]]
      use2<-c("Single","Gauss","Exp","Gamma","GenExp")[use[5]]
      metaX<-metaResult[[tolower(use1)]]
      metaY<-metaResult[[tolower(use2)]]
      if (showType=="metaS;metaS") {
        x<-metaX$Smax
        yS<-metaY$Smax
        y1<-yS
        xticks<-c()
      } else {
        switch (whichMeta,
                "Single"={
                  x<-metaResult$single$param1
                  yS<-metaResult$single$Smax
                  y1<-metaResult$single$param2
                },
                "Gauss"={
                  x<-metaResult$gauss$param1
                  yS<-metaResult$gauss$Smax
                  y1<-metaResult$gauss$param2
                },
                "Exp"={
                  x<-metaResult$exp$param1
                  yS<-metaResult$exp$Smax
                  y1<-metaResult$exp$param2
                },
                "Gamma"={
                  x<-metaResult$gamma$param1
                  yS<-metaResult$gamma$Smax
                  y1<-metaResult$gamma$param2
                },
                "GenExp"={
                  x<-metaResult$genexp$param1
                  yS<-metaResult$genexp$Smax
                  y1<-metaResult$genexp$param2
                }
        )
      }
      keep<- !is.na(x) & !is.na(yS)
      best<-metaResult$best$S[keep]
      yS<-yS[keep]
      y1<-y1[keep]
      x<-x[keep]
      useBest<-yS==best
      
      if (isempty(x)) {return(nullPlot())}
    }
    
    if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
      switch(metaResult$metaAnalysis$analysisType,
             "fixed"={result<-metaResult$fixed},
             "random"={result<-metaResult$random})
    }
    yticks<-c()
    switch (showType,
            "metaRiv;metaRsd"={
              x<-result$param1
              y<-result$param2
              y1<-0
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylabel<-"r[sd]"
              xlabel<-"r[m]"
              useBest<-1:length(x)
            },
            "metaRiv;metaBias"={
              x<-result$param1
              y<-result$param3
              y1<-0
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylim<-c(0,1)
              ylabel<-"bias[m]"
              xlabel<-"r[m]"
              useBest<-1:length(x)
            },
            "metaRiv;metaS"={
              x<-result$param1
              y<-result$Smax
              y1<-0
              sAll<-result$Smax
              ylim<-c(min(sAll,na.rm=TRUE),max(sAll,na.rm=TRUE))+c(-1,1)*(max(sAll,na.rm=TRUE)-min(sAll,na.rm=TRUE))/4
              ylabel<-"log(lk)"
              xlabel<-"r[m]"
              useBest<-1:length(x)
            },
            "metaK;null"={
              y<-y1
              ylim<-c(-0.02,1.1)
              ylabel<-"p[null]"
              xlabel<-braw.env$Llabel
            },
            "metaK;metaS"={
              xlabel<-braw.env$Llabel
              y<-yS
              ylim<-c(min(sAll,na.rm=TRUE),max(sAll,na.rm=TRUE))
              ylabel<-paste0("log(lk ",use2,")")
              useBest<- (metaResult$hypothesis$effect$world$PDF==use2) | (metaResult$hypothesis$effect$world$PDF==use1)
            },
            "metaS;metaS"={
              y<-yS
              xlim<-c(min(sAll,na.rm=TRUE),max(sAll,na.rm=TRUE))
              if (length(x)==1) xlim<-xlim+c(-1,1)
              else xlim=xlim+c(-1,1)*(max(sAll,na.rm=TRUE)-min(sAll,na.rm=TRUE))/4
              xlabel<-paste0("log(lk ",use1,")")
              ylim<-xlim
              ylabel<-paste0("log(lk ",use2,")")
              useBest<- (y>x & metaResult$hypothesis$effect$world$PDF==use2) | (y<x & metaResult$hypothesis$effect$world$PDF==use1)
            }
    )
    pts<-data.frame(x=x,y=y)
    
    if (braw.env$plotArea[1]==0)  
      g<-startPlot(xlim,ylim,
                   xticks=makeTicks(xticks),xlabel=makeLabel(xlabel),
                   yticks=makeTicks(yticks),ylabel=makeLabel(ylabel),
                   top=TRUE,g=g)
    else  
      g<-startPlot(xlim,ylim,
                   xticks=makeTicks(xticks),xlabel=makeLabel(xlabel),
                   top=TRUE,g=g)
    
    dotSize=16*min(0.25,16/length(x))
    
    g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$meta, 
                        colour="#000000", fill="grey", alpha=min(1,2.5/sqrt(length(x))), 
                        size = dotSize))
    pts<-data.frame(x=x[useBest],y=y[useBest])
    g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$meta,
                        colour="#000000", fill=braw.env$plotColours$metaMultiple, alpha=min(1,2.5/sqrt(length(x))), 
                        size = dotSize))
    
    if (showType=="metaS;metaS") {
      g<-addG(g,dataPath(data=data.frame(x=xlim,y=ylim),colour="red"))
    }
    
    if (mean(y1)>0.5) {
      yp<-ylim[1]+diff(ylim)/10
      vj<-0
    } else {
      yp<-ylim[2]-diff(ylim)/10
      vj<-1
    }
    if (showType=="metaS;metaS") {
      fullText<-paste0(use2,"(",format(mean(metaY$param1),digits=3))
      if (length(metaY$param1)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$param1),digits=2),")")
      else fullText<-paste0(fullText,")")
      if (metaAnalysis$modelNulls) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaY$param2),digits=3))
        if (length(metaY$param2)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$param2),digits=2),")")
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaY$Smax),digits=2))
      if (length(metaY$Smax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$Smax),digits=2),")")
      fullText<-paste0(fullText," (",format(sum(y>x)),"/",length(metaResult$best$dist),")")
      
      if (mean(y>x)) colM=braw.env$plotColours$metaMultiple  else colM="grey"
      names<-strsplit(fullText,"\n")[[1]]
      g<-addG(g,dataLegend(data.frame(names=names,colours=c(colM,rep(NA,length(names)-1))),title="",shape=braw.env$plotShapes$meta))
      
      fullText<-paste0(use1,"(",format(mean(metaX$param1),digits=3))
      if (length(metaX$param1)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$param1),digits=2),")")
      else fullText<-paste0(fullText,")")
      if (metaAnalysis$modelNulls) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaX$param2),digits=3))
        if (length(metaX$param2)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$param2),digits=2),")")
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaX$Smax),digits=2))
      if (length(metaX$Smax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$Smax),digits=2),")")
      fullText<-paste0(fullText," (",format(sum(x>y)),"/",length(metaResult$best$dist),")")
      
      if (mean(y>x)) colM="grey"  else colM=braw.env$plotColours$metaMultiple
      names<-strsplit(fullText,"\n")[[1]]
      g<-addG(g,dataLegend(data.frame(names=names,colours=c(colM,rep(NA,length(names)-1))),title="",shape=braw.env$plotShapes$meta))
    } else {
      
      if (is.element(showType,c("metaRiv;metaS","metaRiv;metaBias","metaRiv;metaRsd"))) {
        colM=braw.env$plotColours$metaMultiple
        lb<-worldLabel(metaResult,whichMeta)
        names<-strsplit(lb,"\n")[[1]]
        
        colours<-c(colM,rep(NA,length(names)-1))
        # if (whichMeta=="fixed") {names<-names[1]; colours<-colM;}
        
        g<-addG(g,dataLegend(data.frame(names=names,colours=colours),title="",
                             shape=braw.env$plotShapes$meta))
      } else {
        use<-which.max(c(n1,n2,n3))
        bestD<-c("Single","Gauss","Exp")[use]
        if (whichMeta==bestD)  colM=braw.env$plotColours$metaMultiple else colM="grey"
        lb<-worldLabel(metaResult,whichMeta)
        lb<-strsplit(lb,"\n")[[1]]
        g<-addG(g,dataLegend(data.frame(names=lb,colours=c(colM,rep(NA,length(lb)-1))),title="",shape=braw.env$plotShapes$meta))
      }     
    }
    return(g)
    
  }
  
  makeWorldDist<-function(metaResult,design,world,z,n,sigOnly=0,doTheory=FALSE) {
    if (doTheory) {
      lambda<-world$PDFk
      offset<-0
      shape<-0
      pRPlus<-world$pRPlus
      if (metaResult$metaAnalysis$analysisType=="random") {
        lambda<-metaResult$hypothesis$effect$rSD
        offset<-metaResult$hypothesis$effect$rIV
        pRPlus<-1
        world$PDF<-"Gauss"
      }
      if (metaResult$metaAnalysis$analysisType=="fixed") {
        lambda<-metaResult$hypothesis$effect$rIV
        pRPlus<-1
        world$PDF<-"Single"
      }
    } else {
      lambda<-metaResult$best$param1
      pRPlus<-metaResult$best$param2
      offset<-0
      shape<-0
      if (metaResult$metaAnalysis$analysisType=="random") {
        lambda<-metaResult$random$param1
        shape<-metaResult$random$param2
        pRPlus<-1
        world$PDF<-"Single"
      }
      if (metaResult$metaAnalysis$analysisType=="fixed") {
        lambda<-metaResult$fixed$param1
        pRPlus<-1
        world$PDF<-"Single"
      }
    }
    sigma<-1/sqrt(n-3)
    gain<-nDistrDens(n,design)
    nGain<-gain*n  # *n for the log scale
    
    zdens<-c()
    switch (world$PDF,
            "Single"={
              for (i in 1:length(n)) {
                zrow<-SingleSamplingPDF(z,lambda,sigma[i],shape)$pdf*pRPlus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRPlus)
                if (metaResult$metaAnalysis$analyseBias || sigOnly>0) {
                  zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                  zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
                }
                densGain<-1/sum(zrow)
                # densGain<-gain[i]
                zdens<-rbind(zdens,zrow*densGain*nGain[i])
              }
            },
            "Gauss"={
              for (i in 1:length(n)) {
                zrow<-GaussSamplingPDF(z,lambda,sigma[i],offset)$pdf*pRPlus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRPlus)
                if (metaResult$metaAnalysis$analyseBias || sigOnly>0) {
                  zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                  zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
                }
                densGain<-1/sum(zrow)
                # densGain<-gain[i]
                zdens<-rbind(zdens,zrow*densGain*nGain[i])
              }
            },
            "Exp"={
              for (i in 1:length(n)) {
                zrow<-ExpSamplingPDF(z,lambda,sigma[i])$pdf*pRPlus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRPlus)
                densGain<-1/sum(zrow)
                
                if (metaResult$metaAnalysis$analyseBias || sigOnly>0) {
                  zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                  zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
                }
                # densGain<-gain[i]
                zdens<-rbind(zdens,zrow*densGain*nGain[i])
              }
            },
            "Gamma"={
              for (i in 1:length(n)) {
                zrow<-GammaSamplingPDF(z,lambda,sigma[i])$pdf*pRPlus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRPlus)
                if (metaResult$metaAnalysis$analyseBias || sigOnly>0) {
                  zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                  zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
                }
                densGain<-1/sum(zrow)
                # densGain<-gain[i]
                zdens<-rbind(zdens,zrow*densGain*nGain[i])
              }
            },
            "GenExp"={
              for (i in 1:length(n)) {
                zrow<-GenExpSamplingPDF(z,lambda,sigma[i])$pdf*pRPlus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRPlus)
                if (metaResult$metaAnalysis$analyseBias || sigOnly>0) {
                  zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                  zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
                }
                densGain<-1/sum(zrow)
                # densGain<-gain[i]
                zdens<-rbind(zdens,zrow*densGain*nGain[i])
              }
            }
    )
    # zdens[1,]<-0
    # zdens[,1]<-0
    # zdens[nrow(zdens),]<-0
    # zdens[,ncol(zdens)]<-0
    return(zdens)
  }
  
  drawWorld<-function(hypothesis,design,metaResult,showType="n",g,colour="white",
                      sigOnly=0,
                      showTheory=FALSE,svalExponent=1,showLines=FALSE) {
    world<-hypothesis$effect$world
    if (!world$On) {
      world<-makeWorld(On=TRUE,PDF="Single",RZ="r",
                       PDFk=hypothesis$effect$rIV,pRPlus=1)
    }
    switch(braw.env$RZ,
           "r"={
             r<-seq(-1,1,length.out=501)*braw.env$r_range
             z<-atanh(r)
           },
           "z"={
             z<-seq(-1,1,length.out=501)*braw.env$z_range
           },
           "d"={
             d<-seq(-1,1,length.out=501)*braw.env$d_range
             r<-d/sqrt(d^2+4)
             z<-atanh(r)
           }
    )
    if (showType=="n") {
      if (braw.env$nPlotScale=="log10") 
        n<-10^seq(log10(braw.env$minN),log10(braw.env$maxN),length.out=101)
      else 
        n<-seq(braw.env$minN,braw.env$maN,length.out=101)
    } else
      n<-seq(sqrt(braw.env$minN),sqrt(braw.env$maxN),length.out=101)^2
    
    if (showTheory) {
      za<-makeWorldDist(metaResult,design,world,z,n,sigOnly=sigOnly,doTheory=TRUE)
      switch(braw.env$RZ,
             "r"={
               for (i in 1:nrow(za)) za[i,]<-zdens2rdens(za[i,],r)
             },
             "z"={
             },
             "d"={
               for (i in 1:nrow(za)) za[i,]<-rdens2ddens(zdens2rdens(za[i,],r),d)
             }
      )
      za<-za/max(za,na.rm=TRUE)
    }
    
    if (!is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
      world$PDF<-metaResult$best$dist
      world$PDFk<-metaResult$best$param1
      world$pRPlus<-metaResult$best$param2
    }
    zb<-makeWorldDist(metaResult,design,world,z,n,sigOnly=1,doTheory=FALSE)
    switch(braw.env$RZ,
           "r"={
             for (i in 1:nrow(zb)) zb[i,]<-zdens2rdens(zb[i,],r)
           },
           "z"={
           },
           "d"={
             for (i in 1:nrow(zb)) zb[i,]<-rdens2ddens(zdens2rdens(zb[i,],r),d)
           }
    ) 
    # zb<-zb-min(zb,na.rm=TRUE)
    zb<-zb/max(zb,na.rm=TRUE)
    
    if (showType=="n") {
      if (braw.env$nPlotScale=="log10") {n<-log10(n)}
    }   else n<-sqrt(n)
    switch(braw.env$RZ,"r"={z<-r},"z"={},"d"={z<-d})
    
    # black is the actual world
    # filled is the best fit world
    if (showTheory) {
      ptsa<-list(x=z,y=n,z=za)
      g<-addG(g,dataContour(data=ptsa,fill=NA,colour="#000000",linewidth=0.5,linetype="dotted"))
    }
    
    if (showLines) {
      quants<-seq(0.1,0.9,0.2)
      res<-matrix(NA,length(n),length(quants)*2)
      for (ni in 1:length(n)) {
        use<-zb[ni,]^svalExponent
        localRes<-c()
        for (qi in 1:length(quants)) {
          ascends<-which(use[1:(length(use)-1)]>0 & use[1:(length(use)-1)]<quants[qi] & use[2:length(use)]>quants[qi])
          if (!isempty(ascends))
            localRes<-c(localRes,
                        approx(use[ascends:(ascends+1)],z[ascends:(ascends+1)],quants[qi])$y)
          else localRes<-c(localRes,NA)
          descends<-which(use[2:length(use)]>0 & use[1:(length(use)-1)]>quants[qi] & use[2:length(use)]<quants[qi])
          if (!isempty(descends))
            localRes<-c(localRes,
                        approx(use[descends:(descends+1)],z[descends:(descends+1)],quants[qi])$y)
          else localRes<-c(localRes,NA)
        }
        res[ni,]<-localRes
      }
      for (qi in 1:ncol(res)){
        thisline<-res[,qi]
        thisn<-n
        while (length(thisline)>0) {
          u1<-which(!is.na(thisline))
          if (!isempty(u1)) {
            u1<-min(u1)
            thisn<-thisn[u1:length(thisline)]
            thisline<-thisline[u1:length(thisline)]
            u2<-which(is.na(thisline))
            if (isempty(u2)) u2<-length(thisline)
            else u2<-min(u2)-1
            g<-addG(g,dataPath(data.frame(x=thisline[1:u2],y=thisn[1:u2]),colour=colour,linewidth=0.5))
            if (u2<length(thisline)) {
              thisn<-thisn[(u2+1):length(thisline)]  
              thisline<-thisline[(u2+1):length(thisline)]  
            }
            else thisline<-c()
          }
        }
      }
    }
    
    # g<-addG(g,dataContour(data=ptsb,colour=colour,fill=NA,linewidth=0.5))
    ptsb<-list(x=z,y=n,z=zb)
    g<-addG(g,dataContour(data=ptsb,breaks=seq(0.1,0.9,0.2),colour="black",fill=colour,linewidth=0.1))
    return(g)
  }
  
