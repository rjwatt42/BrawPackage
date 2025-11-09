
makeMetaHist<-function(vals,use,xlim) {
  nbins<-10
  bins<-seq(xlim[1],xlim[2],length.out=nbins+1)
  dens<-hist(vals[use],bins,plot=FALSE)$counts/length(vals)
  h<-list(bins=bins,dens=dens)
}

worldLabel<-function(metaResult,whichMeta=NULL,modelPDF=NULL) {
  if (is.null(whichMeta)) whichMeta<-metaResult$best$PDF
  if (whichMeta=="world") Dist<-modelPDF
  else Dist<-whichMeta
  Dist<-tolower(Dist)
  p1<-metaResult[[Dist]]$PDFk
  p2<-metaResult[[Dist]]$pRplus
  p3<-metaResult[[Dist]]$sigOnly
  p4<-metaResult[[Dist]]$PDFspread
  p5<-metaResult[[Dist]]$PDFshape
  if (whichMeta!="world")
    switch(braw.env$RZ,
           "r"={},
           "z"={
             p1<-atanh(p1)
             p4<-atanh(p4)
           },
           "d"={
             p1<-2*p1/sqrt(1-p1^2)
             p4<-2*p4/sqrt(1-p4^2)
           }
    )
  
  if (is.element(Dist,c("random","fixed"))) {
    label1<-paste0(braw.env$RZ,"[m]") 
    lb<-paste0(label1,"=",brawFormat(mean(p1,na.rm=TRUE),digits=3))
    if (is.element(Dist,c("random"))) {
      label2<-paste0(braw.env$RZ,"[sd]")
      lb<-paste0(lb,"\n",label2,"=",brawFormat(mean(p4,na.rm=TRUE),digits=3))
    }
  } else {
    lb<-paste0(Dist,"(","z","/",brawFormat(mean(p1,na.rm=TRUE),digits=3))
    if (is.element(Dist,c("genexp","gamma"))) lb<-paste0(lb,",",brawFormat(mean(p5,na.rm=TRUE),digits=3))
    lb<-paste0(lb,")")
  }
  if (metaResult$metaAnalysis$analyseNulls) {
    label2<-braw.env$Plabel
    lb<-paste0(lb,"\n",braw.env$Plabel,"=",brawFormat(mean(p2,na.rm=TRUE),digits=3))
  }
  if (metaResult$metaAnalysis$analyseBias) {
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
                         showData=TRUE,showTheory=TRUE,limitNumber=0,
                         xRange="full",fixedYlim=braw.env$fixedYlim,
                         fill=NULL,alpha=NULL) {
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  oldminN<-braw.env$minN
  oldmaxN<-braw.env$maxN
  # on.exit({setBrawEnv("minN",oldminN);setBrawEnv("maxN",oldmaxN)})
  
  showSval<-FALSE
  showSig<-TRUE
  svalExponent<-2
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
    if (!fixedYlim) {
      ylim<-c(min(d2),max(d2))
      if (y$logScale) ylim<-ylim*c(0.75,1.25)
      else ylim<-ylim+c(-1,1)*(max(d2)-min(d2))*0.25
      braw.env$minN<-ylim[1]
      braw.env$maxN<-ylim[2]
    }
    else
      ylim<-c(min(min(d2),braw.env$minN)-1,braw.env$maxN+1)
    yticks<-y$ticks
    if (y$logScale) {
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

  if (limitNumber>0) {
  if (length(d1)>limitNumber) d1<-d1[1:limitNumber]
  if (length(d2)>limitNumber) d2<-d2[1:limitNumber]
  }
  
  useAll<-(d2>ylim[1]) & (d2<ylim[2])
  ptsAll<-data.frame(x=d1[useAll],y=d2[useAll])
  useNull<-(d2>ylim[1]) & (d2<ylim[2]) & d1n
  ptsNull<-data.frame(x=d1[useNull],y=d2[useNull])
  
  if (y$logScale) {
    ptsAll$y<-log10(ptsAll$y)
    ptsNull$y<-log10(ptsNull$y)
  }
  assign("plotArea",c(0,0,1,1),braw.env)
  g<-startPlot(xlim,log10(ylim),
               xticks=makeTicks(x$ticks),xlabel=makeLabel(disp1),
               yticks=yticks,
               ylabel=makeLabel(disp2),
               top=1.5,g=NULL)
  if (showTheory) 
    g<-addG(g,plotTitle(paste0("Method=",metaResult$metaAnalysis$method),size=0.75))
  
  if (showTheory)
  g<-drawWorld(hypothesis,design,metaResult,showType,g,
               braw.env$plotColours$metaAnalysisTheory,
               # sigOnly=metaAnalysis$analyseBias,
               showTheory=FALSE,svalExponent=svalExponent,showLines=showLines)
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
  if (showData) {
  if (length(d1)<=10000) {
  colgain<-1-min(1,sqrt(max(0,(length(d1)-50))/200))
  alphaUse<-max(0.5,1/(max(1,sqrt(length(d1)/100))))
  dotSize<-braw.env$dotSize*min(1,alphaUse)
  fill1<-rep(braw.env$plotColours$metaAnalysis,length(ptsAll$x))
  fill2<-braw.env$plotColours$infer_nsigC
  if (showSval) {
    b<-getLogLikelihood(atanh(metaResult$result$rIV),metaResult$result$nval,rep(1,length(metaResult$result$nval)),
                        distribution=metaResult$best$PDF,
                        location=metaResult$best$PDFk,spread=metaResult$best$pRplus,
                        bias=metaResult$metaAnalysis$analyseBias,returnVals = TRUE)
    b<-((b-min(b))/(max(b)-min(b)))[1:length(d1)]
    fill1<-hsv(0.9*floor(b*10)/10)
  }
  col1<-hsv(1,0,1-alphaUse)
  col2<-fill2
  if (!is.null(alpha)) alphaUse<-alpha
  if (!is.null(fill)) fill1<-fill
  g<-addG(g,dataPoint(data=ptsAll, shape=braw.env$plotShapes$study, colour = col1, fill = fill1, alpha=alphaUse, size = dotSize))
  if (nrow(ptsNull)>0)
    g<-addG(g,dataPoint(data=ptsNull,shape=braw.env$plotShapes$study, colour = col2, fill = fill2, alpha=alphaUse, size = dotSize))
  } else {
    rBins<-seq(-1,1,length.out=101)
    nBins<-seq(log10(5),log10(500),length.out=101)
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
  }
  
  if (showTheory) {
  if (metaAnalysis$modelPDF=="All") metaAnalysis$modelPDF<-metaResult$best$PDF
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
#' showMetaMultiple<-function(metaResult=doMetaAnalysis(),showType=NULL,dimension="1D",orientation="vert")
#' @export
showMetaMultiple<-function(metaResult=braw.res$metaMultiple,showType=NULL,dimension="1D",orientation="vert") {
  if (is.null(metaResult)) metaResult<-doMetaMultiple()
  
  if (is.element(showType,c("Riv","Spread","Smax","K","PRplus","Bias")))
    showType<-paste0("meta",showType)
  
  if (is.null(showType)) {
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={
             showType<-"metaRiv"
             if (metaResult$metaAnalysis$analyseBias) showType<-"metaRiv;metaBias"
           },
           "random"={
             showType<-"metaRiv;metaSpread"
           },
           "world"={
             showType<-"metaRiv"
             if (metaResult$metaAnalysis$analyseBias) showType<-"metaRiv;metaBias"
           })
  }
  
  if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
    autoPrintOld<-braw.env$autoPrint
    on.exit(setBrawEnv("autoPrint",autoPrintOld))
    setBrawEnv("autoPrint",FALSE)
    g<-showMultiple(metaResult,showType=showType,dimension=dimension,orientation=orientation)
  } else {
    switch(showType,
           "metaSmax;metaSmax"={
             braw.env$plotArea<-c(0,0,1,1)
             g<-drawMeta(metaResult=metaResult,showType=showType,g=NULL)
           },
           "metaK;metaPRplus"={
             braw.env$plotArea<-c(0,0,1,1)
             g<-drawMeta(metaResult=metaResult,whichMeta=metaResult$metaAnalysis$modelPDF,showType=showType,g=NULL)
           },
           {
             g<-nullPlot()
             nplots<-sum(!is.na(c(metaResult$single$Smax[1],metaResult$gauss$Smax[1],metaResult$exp$Smax[1],metaResult$genexp$Smax[1],metaResult$gamma$Smax[1])))
             xgap<-0.1
             xsize<-1/nplots-xgap
             xoff<-0
             for (i in 1:5) {
               switch(i,
                      if (!all(is.na(metaResult$single$Smax))) {
                        braw.env$plotArea<-c(xoff,0,xsize,1)
                        g<-drawMeta(metaResult=metaResult,whichMeta="Single",showType=showType,g)
                        xoff<-xoff+xsize+xgap
                      },
                      if (!all(is.na(metaResult$gauss$Smax))) {
                        braw.env$plotArea<-c(xoff,0,xsize,1)
                        g<-drawMeta(metaResult=metaResult,whichMeta="Gauss",showType=showType,g)
                        xoff<-xoff+xsize+xgap
                      },
                      if (!all(is.na(metaResult$exp$Smax))) {
                        braw.env$plotArea<-c(xoff,0,xsize,1)
                        g<-drawMeta(metaResult=metaResult,whichMeta="Exp",showType=showType,g)
                        xoff<-xoff+xsize+xgap
                      },
                      if (!all(is.na(metaResult$genexp$Smax))) {
                        braw.env$plotArea<-c(xoff,0,xsize,1)
                        g<-drawMeta(metaResult=metaResult,whichMeta="GenExp",showType=showType,g)
                        xoff<-xoff+xsize+xgap
                      },
                      if (!all(is.na(metaResult$gamma$Smax))) {
                        braw.env$plotArea<-c(xoff,0,xsize,1)
                        g<-drawMeta(metaResult=metaResult,whichMeta="Gamma",showType=showType,g)
                        xoff<-xoff+xsize+xgap
                      }
               )
             }
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
    
    if (is.element(whichMeta,c("Single","Gauss","Exp","Gamma","GenExp"))) {
      n1<-sum(metaResult$best$PDF=="Single")
      n2<-sum(metaResult$best$PDF=="Gauss")
      n3<-sum(metaResult$best$PDF=="Exp")
      n4<-sum(metaResult$best$PDF=="Gamma")
      n5<-sum(metaResult$best$PDF=="GenExp")
      sAll<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax,metaResult$gamma$Smax,metaResult$genexp$Smax)
      
      use<-order(c(n1,n2,n3,n4,n5))
      use1<-c("Single","Gauss","Exp","Gamma","GenExp")[use[4]]
      use2<-c("Single","Gauss","Exp","Gamma","GenExp")[use[5]]
      metaX<-metaResult[[tolower(use1)]]
      metaY<-metaResult[[tolower(use2)]]
      if (showType=="metaS;metaSmax") {
        x<-metaX$Smax
        yS<-metaY$Smax
        y1<-yS
        xticks<-c()
      } else {
        switch (whichMeta,
                "Single"={
                  x<-metaResult$single$PDFk
                  yS<-metaResult$single$Smax
                  y1<-metaResult$single$pRplus
                },
                "Gauss"={
                  x<-metaResult$gauss$PDFk
                  yS<-metaResult$gauss$Smax
                  y1<-metaResult$gauss$pRplus
                },
                "Exp"={
                  x<-metaResult$exp$PDFk
                  yS<-metaResult$exp$Smax
                  y1<-metaResult$exp$pRplus
                },
                "Gamma"={
                  x<-metaResult$gamma$PDFk
                  yS<-metaResult$gamma$Smax
                  y1<-metaResult$gamma$pRplus
                },
                "GenExp"={
                  x<-metaResult$genexp$PDFk
                  yS<-metaResult$genexp$Smax
                  y1<-metaResult$genexp$pRplus
                }
        )
      }
      keep<- !is.na(x) & !is.na(yS)
      best<-metaResult$best$Smax[keep]
      yS<-yS[keep]
      y1<-y1[keep]
      x<-x[keep]
      useBest<-yS==best
      
      if (isempty(x)) {return(nullPlot())}
    }
    
    useBest<-1:length(x)
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={result<-metaResult$fixed},
           "random"={result<-metaResult$random},
           "world"={
             useBest<-(metaResult$best$PDF==whichMeta)
             switch(whichMeta,
               "Single"={result<-metaResult$single},
               "Gauss"={result<-metaResult$gauss},
               "Exp"={result<-metaResult$exp},
               "Gamma"={result<-metaResult$gamma},
               "GenExp"={result<-metaResult$genexp}
             )
           }
    )
    
    xticks<-c()
    yticks<-c()
    showTypes<-strsplit(showType,";")[[1]]
    switch(showTypes[1],
           "metaRiv"={
             x<-result$PDFk
             xlim<-c(-1,1)
             xlabel<-"r[m]"
           },
           "metaK"={
             x<-result$PDFk
             xlim<-c(0,1)+c(-1,1)*(1-0)*0.1
             xlabel<-braw.env$Llabel
           },
           "metaSpread"={
             x<-result$PDFspread
             xlim<-c(min(x),max(x))+c(-1,1)*(max(x)-min(x))*0.2
             xlabel<-"r[sd]"
           },
           "metaShape"={
             x<-result$PDFshape
             xlim<-c(min(x),max(x))+c(-1,1)*(max(x)-min(x))*0.2
             xlabel<-"r[sh]"
           },
           "metaBias"={
             x<-result$sigOnly
             xlim<-c(0,1)+c(-1,1)*(1-0)*0.1
             xlabel<-"bias[m]"
           },
           "metaSmax"={
             x<-result$Smax
             xlim<-c(min(x),max(x))+c(-1,1)*(max(x)-min(x))*0.2
             xlabel<-"log(lk)"
           },
           "metapRplus"={
             x<-result$pRplus
             xlim<-c(0,1)+c(-1,1)*(1-0)*0.1
             xlabel<-braw.env$Plabel
           },
           "null"={
             x<-result$pRplus
             xlim<-c(0,1)+c(-1,1)*(1-0)*0.1
             xlabel<-braw.env$Plabel
           }
    )
    switch (showTypes[2],
            "metaRiv"={
              y<-result$PDFk
              ylim<-c(-1,1)
              ylabel<-"r[m]"
            },
            "metaK"={
              y<-result$PDFk
              ylim<-c(-1,1)
              ylabel<-braw.env$Llabel
            },
            "metaSpread"={
              y<-result$PDFspread
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylabel<-"r[sd]"
            },
            "metaShape"={
              y<-result$PDFshape
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylabel<-"r[sh]"
            },
            "metaBias"={
              y<-result$sigOnly
              ylim<-c(0,1)
              ylabel<-"bias[m]"
            },
            "metaSmax"={
              y<-result$Smax
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylabel<-"log(lk)"
            },
            "metaPrplus"={
              y<-result$pRplus
              ylim<-c(-0.02,1.1)
              ylabel<-braw.env$Plabel
            },
            "null"={
              y<-result$pRplus
              ylim<-c(-0.02,1.1)
              ylabel<-braw.env$Plabel
            }
    )
    pts<-data.frame(x=x,y=y)
    
    if (braw.env$plotArea[1]==0)  
      g<-startPlot(xlim,ylim,
                   xticks=makeTicks(xticks),xlabel=makeLabel(xlabel),
                   yticks=makeTicks(yticks),ylabel=makeLabel(ylabel),
                   top=1.5,g=g)
    else  
      g<-startPlot(xlim,ylim,
                   xticks=makeTicks(xticks),xlabel=makeLabel(xlabel),
                   # yticks=makeTicks(yticks),ylabel=makeLabel(ylabel),
                   top=1.5,g=g)
    
    dotSize=16*min(0.25,16/length(x))
    
    f1<-darken(braw.env$plotColours$metaMultiple,off=-0.2)
    f2<-braw.env$plotColours$metaMultiple
    if (any(!useBest))
      g<-addG(g,dataPoint(data=pts[!useBest,],shape=braw.env$plotShapes$meta, 
                        colour=f1, 
                        fill=f1, alpha=min(1,2.5/sqrt(length(x))), 
                        size = dotSize))
    if (any(useBest))
    g<-addG(g,dataPoint(data=pts[useBest,],shape=braw.env$plotShapes$meta,
                        colour=darken(f2,off=0.2), 
                        fill=f2, alpha=min(1,2.5/sqrt(length(x))), 
                        size = dotSize,strokewidth=1))
    
    use<-which.max(c(n1,n2,n3))
    bestD<-c("Single","Gauss","Exp")[use]
    if (whichMeta==bestD)  
         colM=f2 
    else colM=f1
    lb<-worldLabel(metaResult,whichMeta)
    lb<-strsplit(lb,"\n")[[1]]
    g<-addG(g,dataLegend(data.frame(names=lb,colours=c(colM,rep(NA,length(lb)-1))),title="",shape=braw.env$plotShapes$meta))
    
    return(g)
    
  }
  
  makeWorldDist<-function(metaResult,design,world,z,n,sigOnly=0,doTheory=TRUE) {
    if (doTheory) {
      lambda<-world$PDFk
      offset<-0
      shape<-0
      pRplus<-world$pRplus
      if (metaResult$metaAnalysis$analysisType=="random") {
        lambda<-metaResult$hypothesis$effect$rSD
        offset<-metaResult$hypothesis$effect$rIV
        pRplus<-1
        world$PDF<-"Gauss"
      }
      if (metaResult$metaAnalysis$analysisType=="fixed") {
        lambda<-metaResult$hypothesis$effect$rIV
        pRplus<-1
        world$PDF<-"Single"
      }
    } else {
      lambda<-metaResult$best$PDFk
      pRplus<-metaResult$best$pRplus
      offset<-0
      shape<-0
      if (metaResult$metaAnalysis$analysisType=="random") {
        lambda<-metaResult$random$PDFk
        shape<-metaResult$random$PDFshape
        pRplus<-metaResult$best$pRplus
        world$PDF<-"Single"
      }
      if (metaResult$metaAnalysis$analysisType=="fixed") {
        lambda<-metaResult$fixed$PDFk
        pRplus<-metaResult$best$pRplus
        world$PDF<-"Single"
      }
    }
    sigma<-1/sqrt(n-3)
    # 
    # q<-fitdistrplus::fitdist(metaResult$result$nval, distr = "gamma", method = "mle")
    design<-getDesign("Psych")
    gain<-nDistrDens(n,design)
    nGain<-gain*n  # *n for the log scale
    # h<-hist(metaResult$result$nval,breaks=c(0,n,1000),plot=FALSE)
    # nGain<-h$density

    zdens<-c()
    switch (world$PDF,
            "Single"={
              for (i in 1:length(n)) {
                zrow<-SingleSamplingPDF(z,lambda,sigma[i])$pdf*pRplus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRplus)
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
                zrow<-GaussSamplingPDF(z,lambda,sigma[i],offset)$pdf*pRplus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRplus)
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
                zrow<-ExpSamplingPDF(z,lambda,sigma[i])$pdf*pRplus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRplus)
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
                zrow<-GammaSamplingPDF(z,lambda,sigma[i])$pdf*pRplus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRplus)
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
                zrow<-GenExpSamplingPDF(z,lambda,sigma[i])$pdf*pRplus+
                  SingleSamplingPDF(z,0,sigma[i])$pdf*(1-pRplus)
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
                       PDFk=hypothesis$effect$rIV,pRplus=1)
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
      za<-makeWorldDist(metaResult,design,world,z,n,sigOnly=sigOnly)
      switch(braw.env$RZ,
             "r"={ for (i in 1:nrow(za)) za[i,]<-zdens2rdens(za[i,],r) },
             "z"={ },
             "d"={ for (i in 1:nrow(za)) za[i,]<-rdens2ddens(zdens2rdens(za[i,],r),d) }
      )
      za<-za/max(za,na.rm=TRUE)
    }

      zb<-makeWorldDist(metaResult,design,metaResult$best,z,n,sigOnly=sigOnly)
      zb[is.na(zb)]<-0
      switch(braw.env$RZ,
             "r"={ for (i in 1:nrow(zb)) zb[i,]<-zdens2rdens(zb[i,],r) },
             "z"={ },
             "d"={ for (i in 1:nrow(zb)) zb[i,]<-rdens2ddens(zdens2rdens(zb[i,],r),d) }
      ) 
      zb<-zb/max(zb,na.rm=TRUE)

    if (showType=="n") {
      if (braw.env$nPlotScale=="log10") {n<-log10(n)}
    }   else n<-sqrt(n)
    switch(braw.env$RZ,"r"={z<-r},"z"={},"d"={z<-d})
    
    # black is the actual world
    # filled is the best fit world
    if (showTheory) {
      ptsa<-list(x=z,y=n,z=za)
      g<-addG(g,dataContour(data=ptsa,breaks=seq(0,1,0.1)^svalExponent,fill=NA,colour="#000000",linewidth=0.5,linetype="dotted"))
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
      
    ptsb<-list(x=z,y=n,z=zb)
    g<-addG(g,dataContour(data=ptsb,breaks=seq(0,1,0.2)^svalExponent,colour="black",fill=colour,linewidth=0.1))
    return(g)
  }
  
