
makeMetaHist<-function(vals,use,xlim) {
  nbins<-10
  bins<-seq(xlim[1],xlim[2],length.out=nbins+1)
  dens<-hist(vals[use],bins,plot=FALSE)$counts/length(vals)
  h<-list(bins=bins,dens=dens)
}

worldLabel<-function(metaResult,whichMeta=NULL,modelPDF=NULL) {
  if (is.null(whichMeta)) whichMeta<-metaResult$bestDist
  if (whichMeta=="world") Dist<-modelPDF
  else Dist<-whichMeta
  Dist<-tolower(Dist)
    p1<-metaResult[[Dist]]$param1Max
    p2<-metaResult[[Dist]]$param2Max
    p3<-metaResult[[Dist]]$param3Max
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
      if (is.element(Dist,c("random","fixed"))) label2<-paste0("sd(",braw.env$RZ,")[m]")
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
showMetaSingle<-function(metaResult=braw.res$metaSingle,showType="n",showTheory=FALSE) {
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  showSval<-FALSE
  showSig<-TRUE
  SvalExp<-1
  showLines<-FALSE # in jamovi the code for lines is very slow
  
  metaAnalysis<-metaResult$metaAnalysis
  hypothesis<-metaResult$hypothesis
  design<-metaResult$design
  # setBrawEnv("RZ","z")
  
  d1<-metaResult$result$rIV
  switch(braw.env$RZ,
         "r"={},
         "z"={d1<-atanh(d1)},
         "d"={d1<-2*d1/sqrt(1-d1^2)}
         )
  d1n<-(metaResult$result$rpIV==0 & hypothesis$effect$world$worldOn)
  x<-plotAxis("rs",hypothesis)
  xlim<-x$lim
  disp1<-x$label

  if (showType=="n") {
    d2<-metaResult$result$nval
    y<-plotAxis("n",hypothesis)
    disp2<-y$label
    ylim<-c(braw.env$minN-1,braw.env$maxN+1)
    yticks<-y$ticks
    if (braw.env$nPlotScale=="log10") {
      d2<-log10(d2)
      ylim<-log10(ylim)
      yticks<-makeTicks(10^yticks,logScale=TRUE)
    }
  } else {
    disp2<-"1/se"
    ylim<-sqrt(c(braw.env$minN,braw.env$maxN))
    yticks<-seq(ceil(sqrt(braw.env$minN)),floor(sqrt(braw.env$maxN)),1)
    d2<-sqrt(metaResult$result$nval)
  }
  useAll<-(d2>ylim[1]) & (d2<ylim[2])
  ptsAll<-data.frame(x=d1[useAll],y=d2[useAll])
  useNull<-(d2>ylim[1]) & (d2<ylim[2] & d1n)
  ptsNull<-data.frame(x=d1[useNull],y=d2[useNull])
  
  assign("plotArea",c(0,0,1,1),braw.env)
  g<-startPlot(xlim,ylim,
               xticks=makeTicks(x$ticks),xlabel=makeLabel(disp1),
               yticks=yticks,
               ylabel=makeLabel(disp2),
               top=1,g=NULL)
  g<-addG(g,plotTitle(paste0("Method=",metaResult$metaAnalysis$method),size=0.75))
  
  g<-drawWorld(hypothesis,design,metaResult,showType,g,
               braw.env$plotColours$metaAnalysisTheory,
               # sigOnly=metaAnalysis$analyseBias,
               showTheory=showTheory,SvalExp=SvalExp,showLines=showLines)
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
  # if (length(d1)<1200) {
    colgain<-1-min(1,sqrt(max(0,(length(d1)-50))/200))
    alpha<-1/(max(1,sqrt(length(d1)/100)))
    dotSize<-braw.env$dotSize*alpha*0.66
    fill1<-rep(braw.env$plotColours$metaAnalysis,length(ptsAll$x))
    fill2<-braw.env$plotColours$infer_nsigC
    if (showSval) {
      b<-getLogLikelihood(atanh(metaResult$result$rIV),metaResult$result$nval,rep(1,length(metaResult$result$nval)),
                          distribution=metaResult$bestDist,metaResult$bestParam1,metaResult$bestParam2,
                          bias=metaResult$metaAnalysis$analyseBias,returnVals = TRUE)
      fill1<-hsv(0.9*round((b-min(b))/(max(b)-min(b))*4)/4)
      fill1<-hsv(0.9*round((b/max(b))^SvalExp*10)/10)
    }
    col1<-hsv(1,0,1-alpha)
    col2<-fill2
    g<-addG(g,dataPoint(data=ptsAll, shape=braw.env$plotShapes$study, colour = col1, fill = fill1, alpha=alpha, size = dotSize))
    if (nrow(ptsNull)>0)
    g<-addG(g,dataPoint(data=ptsNull,shape=braw.env$plotShapes$study, colour = col2, fill = fill2, alpha=alpha, size = dotSize))
  # }
  
  lb<-worldLabel(metaResult,metaAnalysis$analysisType,metaAnalysis$modelPDF)
  names=strsplit(lb,"\n")[[1]]
  if (length(names)==1) colours=braw.env$plotColours$metaAnalysis else colours=c(braw.env$plotColours$metaAnalysis,rep(NA,length(names)-1))
  g<-addG(g,dataLegend(data.frame(names=names,colours=colours),title="",shape=22))
  # g<-addG(g,plotTitle(lb,"left",size=1))
  
  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  

}

#' show a multiple meta-analyses
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showMetaMultiple<-function(metaResult=doMetaAnalysis(),showType=NULL,dimension="2D")
#' @export
showMetaMultiple<-function(metaResult=braw.res$metaMultiple,showType=NULL,dimension="2D") {
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
             showType="metaS;metaS"
           })
  }
  
  if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
    g<-showMultiple(metaResult,showType=showType,dimension=dimension)
  } else {
    if (showType=="metaS;metaS") {
      braw.env$plotArea<-c(0,0,1,1)
      g<-drawMeta(metaResult=metaResult,showType=showType,g=NULL)
    } else {
      g<-nullPlot()
      if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
        braw.env$plotArea<-c(0,0,1,1)
        g<-drawMeta(metaResult=metaResult,showType=showType,whichMeta=metaResult$metaAnalysis$analysisType,g=g)
      }  else {
        braw.env$plotArea<-c(0,0,0.4,1)
        if (!all(is.na(metaResult$single$Smax)))
          g<-drawMeta(metaResult=metaResult,whichMeta="Single",showType=showType,g)
        braw.env$plotArea<-c(0.4,0,0.3,1)
        if (!all(is.na(metaResult$gauss$Smax)))
          g<-drawMeta(metaResult=metaResult,whichMeta="Gauss",showType=showType,g)
        braw.env$plotArea<-c(0.7,0,0.3,1)
        if (!all(is.na(metaResult$exp$Smax)))
          g<-drawMeta(metaResult=metaResult,whichMeta="Exp",showType=showType,g)
      }
    }
  }
  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}

drawMeta<-function(metaResult=doMetaMultiple(),whichMeta="Single",showType="metaK;null",g=NULL) {
  
  metaAnalysis<-metaResult$metaAnalysis

  xlim<-c(-1,1)
  xticks<-seq(-1,1,0.5)
  
  if (is.element(whichMeta,c("Single","Gauss","Exp"))) {
  n1<-sum(metaResult$bestDist=="Single")
  n2<-sum(metaResult$bestDist=="Gauss")
  n3<-sum(metaResult$bestDist=="Exp")
  sAll<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax)
  
  if (showType=="metaS;metaS") {
    use<-order(c(n1,n2,n3))
    use1<-c("Single","Gauss","Exp")[use[2]]
    use2<-c("Single","Gauss","Exp")[use[3]]
    metaX<-metaResult[[tolower(use1)]]
    metaY<-metaResult[[tolower(use2)]]
    x<-metaX$Smax
    yS<-metaY$Smax
    y1<-yS
    xticks<-c()
  } else {
    switch (whichMeta,
            "Single"={
              x<-metaResult$single$param1Max
              yS<-metaResult$single$Smax
              y1<-metaResult$single$param2Max
            },
            "Gauss"={
              x<-metaResult$gauss$param1Max
              yS<-metaResult$gauss$Smax
              y1<-metaResult$gauss$param2Max
            },
            "Exp"={
              x<-metaResult$exp$param1Max
              yS<-metaResult$exp$Smax
              y1<-metaResult$exp$param2Max
            }
    )
  }
    keep<- !is.na(x) & !is.na(yS)
    best<-metaResult$bestS[keep]
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
              x<-result$param1Max
              y<-result$param2Max
              y1<-0
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylabel<-"sd(r)[m]"
              xlabel<-"r[m]"
              useBest<-1:length(x)
            },
            "metaRiv;metaBias"={
              x<-result$param1Max
              y<-result$param3Max
              y1<-0
              ylim<-c(min(y),max(y))+c(-1,1)*(max(y)-min(y))*0.2
              ylim<-c(0,1)
              ylabel<-"bias[m]"
              xlabel<-"r[m]"
              useBest<-1:length(x)
            },
            "metaRiv;metaS"={
              x<-result$param1Max
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
            "metaS;metaS"={
              y<-yS
              xlim<-c(min(sAll,na.rm=TRUE),max(sAll,na.rm=TRUE))
              if (length(x)==1) xlim<-xlim+c(-1,1)
              else xlim=xlim+c(-1,1)*(max(sAll,na.rm=TRUE)-min(sAll,na.rm=TRUE))/4
              xlabel<-paste0("log(lk ",use1,")")
              ylim<-xlim
              ylabel<-paste0("log(lk ",use2,")")
              useBest<- (y>x & metaResult$hypothesis$effect$world$populationPDF==use2) | (y<x & metaResult$hypothesis$effect$world$populationPDF==use1)
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

      dotSize=16*min(0.25,2.5/sqrt(length(x)))
      
      g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$meta, 
                          colour="black", fill="grey", alpha=min(1,2.5/sqrt(length(x))), 
                          size = dotSize))
      pts<-data.frame(x=x[useBest],y=y[useBest])
      g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$meta,
                          colour="black", fill=braw.env$plotColours$metaMultiple, alpha=min(1,2.5/sqrt(length(x))), 
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
      fullText<-paste0(use2,"(",format(mean(metaY$param1Max),digits=3))
      if (length(metaY$param1Max)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$param1Max),digits=2),")")
      else fullText<-paste0(fullText,")")
      if (metaAnalysis$includeNulls) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaY$param2Max),digits=3))
        if (length(metaY$param2Max)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$param2Max),digits=2),")")
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaY$Smax),digits=2))
      if (length(metaY$Smax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$Smax),digits=2),")")
      fullText<-paste0(fullText," (",format(sum(y>x)),"/",length(metaResult$bestDist),")")
      
      if (mean(y>x)) colM=braw.env$plotColours$metaMultiple  else colM="grey"
      names<-strsplit(fullText,"\n")[[1]]
      g<-addG(g,dataLegend(data.frame(names=names,colours=c(colM,rep(NA,length(names)-1))),title="",shape=braw.env$plotShapes$meta))
      
      fullText<-paste0(use1,"(",format(mean(metaX$param1Max),digits=3))
      if (length(metaX$param1Max)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$param1Max),digits=2),")")
      else fullText<-paste0(fullText,")")
      if (metaAnalysis$includeNulls) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaX$param2Max),digits=3))
        if (length(metaX$param2Max)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$param2Max),digits=2),")")
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaX$Smax),digits=2))
      if (length(metaX$Smax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$Smax),digits=2),")")
      fullText<-paste0(fullText," (",format(sum(x>y)),"/",length(metaResult$bestDist),")")
      
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
        g<-addG(g,dataLegend(data.frame(names=strsplit(lb,"\n")[[1]],colours=c(colM,NA)),title="",shape=braw.env$plotShapes$meta))
      }     
    }
    return(g)

}

makeWorldDist<-function(metaResult,design,world,z,n,sigOnly=FALSE,doTheory=FALSE) {
  if (doTheory) {
    lambda<-world$populationPDFk
    nullP<-world$populationNullp
    offset<-0
    shape<-0
    nullP<-world$populationNullp
    if (metaResult$metaAnalysis$analysisType=="random") {
      lambda<-metaResult$hypothesis$effect$rSD
      offset<-metaResult$hypothesis$effect$rIV
      nullP<-0
      world$populationPDF<-"Gauss"
    }
    if (metaResult$metaAnalysis$analysisType=="fixed") {
      lambda<-metaResult$hypothesis$effect$rIV
      nullP<-0
      world$populationPDF<-"Single"
    }
  } else {
    lambda<-metaResult$bestParam1
    nullP<-metaResult$bestParam2
    offset<-0
    shape<-0
    if (metaResult$metaAnalysis$analysisType=="random") {
      lambda<-metaResult$random$param1Max
      shape<-metaResult$random$param2Max
      nullP<-0
      world$populationPDF<-"Single"
    }
    if (metaResult$metaAnalysis$analysisType=="fixed") {
      lambda<-metaResult$fixed$param1Max
      nullP<-0
      world$populationPDF<-"Single"
    }
  }
  sigma<-1/sqrt(n-3)
  gain<-nDistrDens(n,design)
  gain<-gain*n  # *n for the log scale
  
  zdens<-c()
  switch (world$populationPDF,
          "Single"={
            for (i in 1:length(n)) {
              zrow<-SingleSamplingPDF(z,lambda,sigma[i],shape)$pdf*(1-nullP)+
                SingleSamplingPDF(z,0,sigma[i])$pdf*nullP
              if (metaResult$metaAnalysis$analyseBias & sigOnly>0) {
                zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
              }
              densGain<-1/sum(zrow)
              # densGain<-gain[i]
              zdens<-rbind(zdens,zrow*densGain)
            }
          },
          "Gauss"={
            for (i in 1:length(n)) {
              zrow<-GaussSamplingPDF(z,lambda,sigma[i],offset)$pdf*(1-nullP)+
                SingleSamplingPDF(z,0,sigma[i])$pdf*nullP
              if (metaResult$metaAnalysis$analyseBias & sigOnly>0) {
                zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
              }
              densGain<-1/max(zrow)
              # densGain<-gain[i]
              zdens<-rbind(zdens,zrow*densGain)
            }
          },
          "Exp"={
            for (i in 1:length(n)) {
              zrow<-ExpSamplingPDF(z,lambda,sigma[i])$pdf*(1-nullP)+
                SingleSamplingPDF(z,0,sigma[i])$pdf*nullP
              if (metaResult$metaAnalysis$analyseBias & sigOnly>0) {
                zcrit<-atanh(p2r(braw.env$alphaSig,n[i]))
                zrow[abs(z)<zcrit]<-zrow[abs(z)<zcrit]*(1-sigOnly)
              }
              densGain<-1/max(zrow)
              # densGain<-gain[i]
              zdens<-rbind(zdens,zrow*densGain)
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
                    sigOnly=FALSE,
                    showTheory=FALSE,SvalExp=1,showLines=FALSE) {
  world<-hypothesis$effect$world
  if (!world$worldOn) {
    world<-makeWorld(worldOn=TRUE,populationPDF="Single",populationRZ="r",
                     populationPDFk=hypothesis$effect$rIV,populationNullp=0)
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
    world$populationPDF<-metaResult$bestDist
    world$populationPDFk<-metaResult$bestParam1
    world$populationNullp<-metaResult$bestParam2
  }
  zb<-makeWorldDist(metaResult,design,world,z,n,sigOnly=sigOnly,doTheory=FALSE)
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
    g<-addG(g,dataContour(data=ptsa,colour="black",linewidth=0.5,linetype="dotted"))
  }
  
  if (showLines) {
  quants<-seq(0.1,0.9,0.2)
  res<-matrix(NA,length(n),length(quants)*2)
  for (ni in 1:length(n)) {
    use<-zb[ni,]^SvalExp
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
  ptsb<-list(x=z,y=n,z=zb^SvalExp)
  g<-addG(g,dataContour(data=ptsb,colour=NA,fill=colour,linewidth=0.5))
  return(g)
}
