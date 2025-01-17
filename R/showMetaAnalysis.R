
makeMetaHist<-function(vals,use,xlim) {
  nbins<-10
  bins<-seq(xlim[1],xlim[2],length.out=nbins+1)
  dens<-hist(vals[use],bins,plot=FALSE)$counts/length(vals)
  h<-list(bins=bins,dens=dens)
}

worldLabel<-function(metaResult,whichMeta=NULL) {
  if (is.null(whichMeta)) whichMeta<-metaResult$bestDist
    Dist<-tolower(whichMeta)
    K<-metaResult[[Dist]]$Kmax
    pNull<-metaResult[[Dist]]$Nullmax
  
  lb<-paste0(Dist,"(",brawFormat(mean(K,na.rm=TRUE),digits=2))
  if (length(K)>1)
    lb<-paste0(lb,"\u00B1",brawFormat(std(K),digits=2))
  lb<-paste0(lb,")")
  if (!is.null(pNull)) {
    lb<-paste0(lb,"\np(null)=",brawFormat(mean(pNull,na.rm=TRUE),digits=2))
    if (length(pNull)>1)
      lb<-paste0(lb,"\u00B1",brawFormat(std(pNull),digits=2))
  }
  return(lb)
}

#' show a single meta-analysis 
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showSingleMeta(metaResult=doMetaAnalysis(1),showTheory=FALSE)
#' @export
showMetaSingle<-function(metaResult=braw.res$metaResult,showTheory=FALSE) {
  if (is.null(metaResult)) metaResult<-doMetaAnalysis(1)
  
  metaAnalysis<-metaResult$metaAnalysis
  hypothesis<-metaResult$hypothesis
  design<-metaResult$design
  
  d1<-metaResult$result$rIV
  d1n<-(metaResult$result$rpIV==0)
  x<-plotAxis("rs",hypothesis)
  xlim<-x$lim
  disp1<-x$label
  # if (all(d1>=0)) xlim[1]<-0
  
  d2<-metaResult$result$nval
  y<-plotAxis("n",hypothesis)
  disp2<-y$label
  ylim<-y$lim
  
  if (y$logScale) d2<-log10(d2)
  useAll<-(d2>ylim[1]) & (d2<ylim[2])
  ptsAll<-data.frame(x=d1[useAll],y=d2[useAll])
  useNull<-(d2>ylim[1]) & (d2<ylim[2] & d1n)
  ptsNull<-data.frame(x=d1[useNull],y=d2[useNull])
  
  assign("plotArea",c(0,0,1,1),braw.env)
  g<-startPlot(xlim,ylim,
               xticks=makeTicks(x$ticks),xlabel=makeLabel(disp1),
               yticks=makeTicks(y$ticks,10^y$ticks),ylabel=makeLabel(disp2),
               top=1,g=NULL)

  if (length(d1)>=1200) {
    nbins<-diff(ylim)/(2*IQR(d2[use])*length(d2[use])^(-0.33))*2
    nbins<-min(nbins,101)
    g<-addG(g,stat_bin2d(data=pts,aes(x=x,y=y),bins=nbins)+scale_fill_gradientn(colours=c(braw.env$plotColours$graphBack,braw.env$plotColours$descriptionC)))
  }
  
  g<-drawWorld(hypothesis,design,metaResult,g,
               darken(braw.env$plotColours$descriptionC,off=0.1),showTheory=showTheory)
  if (metaAnalysis$includeBias) {
    rv<-seq(min(xlim),max(xlim),length.out=101)
    nv<-rw2n(rv,0.5)
    if (braw.env$nPlotScale=="log10") {nv<-log10(nv)}
    nv[nv>ylim[2] | nv<ylim[1]]<-NA
    g<-addG(g,dataLine(data.frame(x=rv,y=nv),
                       colour=darken(braw.env$plotColours$infer_sigC,off=0.1),linewidth=0.5))
  }
    
  dotSize<-braw.env$dotSize/2
  
  # show individual studies
  if (length(d1)<1200) {
    colgain<-1-min(1,sqrt(max(0,(length(d1)-50))/200))
    dotSize<-dotSize/(ceil(length(d1)/400))
    cl<-"black"
    col1<-braw.env$plotColours$descriptionC
    col2<-braw.env$plotColours$infer_nsigC
    g<-addG(g,dataPoint(data=ptsAll, shape=braw.env$plotShapes$study, colour = darken(col1,off=-colgain), fill = col1, size = dotSize))
    g<-addG(g,dataPoint(data=ptsNull,shape=braw.env$plotShapes$study, colour = darken(col2,off=-colgain), fill = col2,  size = dotSize))
  }
  
  lb<-worldLabel(metaResult)
  g<-addG(g,dataLegend(data.frame(names=strsplit(lb,"\n")[[1]],colours=c(braw.env$plotColours$descriptionC,NA)),title="",shape=22))
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
#' showMultipleMeta<-function(metaResult=doMetaAnalysis(),showType="n-k")
#' @export
showMetaMultiple<-function(metaResult=braw.res$metaResult,showType="n-k") {
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()

  if (metaResult$metaAnalysis$analysisType=="fixed") showType<-"S-k"
  if (showType=="S-S") {
    braw.env$plotArea<-c(0,0,1,1)
    g<-drawMeta(metaResult=metaResult,showType=showType,g=NULL)
  } else {
    g<-nullPlot()
    if (metaResult$metaAnalysis$analysisType=="fixed") braw.env$plotArea<-c(0,0,1,1)
    else braw.env$plotArea<-c(0,0,0.4,1)
    if (!all(is.na(metaResult$single$Smax)))
      g<-drawMeta(metaResult=metaResult,whichMeta="Single",showType=showType,g)
    braw.env$plotArea<-c(0.4,0,0.3,1)
    if (!all(is.na(metaResult$gauss$Smax)))
      g<-drawMeta(metaResult=metaResult,whichMeta="Gauss",showType=showType,g)
    braw.env$plotArea<-c(0.7,0,0.3,1)
    if (!all(is.na(metaResult$exp$Smax)))
      g<-drawMeta(metaResult=metaResult,whichMeta="Exp",showType=showType,g)
  }
  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}

drawMeta<-function(metaResult=doMetaAnalysis(),whichMeta="Single",showType="n-k",g=NULL) {
  
  metaAnalysis<-metaResult$metaAnalysis

  n1<-sum(metaResult$bestDist=="Single")
  n2<-sum(metaResult$bestDist=="Gauss")
  n3<-sum(metaResult$bestDist=="Exp")
  sAll<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax)
  
  xlim<-c(-1,1)
  xticks<-seq(-1,1,0.5)
  if (showType=="S-S") {
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
              x<-metaResult$single$Kmax
              yS<-metaResult$single$Smax
              y1<-metaResult$single$Nullmax
            },
            "Gauss"={
              x<-metaResult$gauss$Kmax
              yS<-metaResult$gauss$Smax
              y1<-metaResult$gauss$Nullmax
            },
            "Exp"={
              x<-metaResult$exp$Kmax
              yS<-metaResult$exp$Smax
              y1<-metaResult$exp$Nullmax
            }
    )
  }
    keep<- !is.na(x) & !is.na(yS)
    best<-metaResult$bestS[keep]
    yS<-yS[keep]
    y1<-y1[keep]
    x<-x[keep]
    
    if (isempty(x)) {return(nullPlot())}
    
    yticks<-c()
    useBest<-yS==best
    switch (showType,
            "S-k"={
              y<-yS
              ylim<-c(min(sAll,na.rm=TRUE),max(sAll,na.rm=TRUE))+c(-1,1)*(max(sAll,na.rm=TRUE)-min(sAll,na.rm=TRUE))/4
              ylabel<-"log(lk)"
              xlabel<-braw.env$Llabel
            },
            "n-k"={
              y<-y1
              ylim<-c(-0.02,1.1)
              ylabel<-"p[null]"
              xlabel<-braw.env$Llabel
            },
            "S-S"={
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

      dotSize=min(4,max(4,sqrt(50/length(x))))
      
      g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$meta, colour = "black", fill = "grey", size = dotSize))
      pts<-data.frame(x=x[useBest],y=y[useBest])
      g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$meta, colour = "black", fill = "yellow", size = dotSize))
      
      if (showType=="S-S") {
        g<-addG(g,dataPath(data=data.frame(x=xlim,y=ylim),colour="red"))
      }

    if (mean(y1)>0.5) {
      yp<-ylim[1]+diff(ylim)/10
      vj<-0
    } else {
        yp<-ylim[2]-diff(ylim)/10
        vj<-1
        }
    if (showType=="S-S") {
      fullText<-paste0(use2,"(",format(mean(metaY$Kmax),digits=3))
      if (length(metaY$Kmax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$Kmax),digits=2),")")
      else fullText<-paste0(fullText,")")
      if (metaAnalysis$includeNulls) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaY$Nullmax),digits=3))
        if (length(metaY$Nullmax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$Nullmax),digits=2),")")
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaY$Smax),digits=2))
      if (length(metaY$Smax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaY$Smax),digits=2),")")
      fullText<-paste0(fullText," (",format(sum(y>x)),"/",length(metaResult$bestDist),")")
      
      if (mean(y>x)) colM="yellow"  else colM="grey"
      names<-strsplit(fullText,"\n")[[1]]
      g<-addG(g,dataLegend(data.frame(names=names,colours=c(colM,rep(NA,length(names)-1))),title="",shape=braw.env$plotShapes$meta))
      
      fullText<-paste0(use1,"(",format(mean(metaX$Kmax),digits=3))
      if (length(metaX$Kmax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$Kmax),digits=2),")")
      else fullText<-paste0(fullText,")")
      if (metaAnalysis$includeNulls) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaX$Nullmax),digits=3))
        if (length(metaX$Nullmax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$Nullmax),digits=2),")")
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaX$Smax),digits=2))
      if (length(metaX$Smax)>1) fullText<-paste0(fullText,"\u00B1",format(std(metaX$Smax),digits=2),")")
      fullText<-paste0(fullText," (",format(sum(x>y)),"/",length(metaResult$bestDist),")")
      
      if (mean(y>x)) colM="grey"  else colM="yellow"
      names<-strsplit(fullText,"\n")[[1]]
      g<-addG(g,dataLegend(data.frame(names=names,colours=c(colM,rep(NA,length(names)-1))),title="",shape=braw.env$plotShapes$meta))
    } else {
      

      use<-which.max(c(n1,n2,n3))
      bestD<-c("Single","Gauss","Exp")[use]
      if (whichMeta==bestD)  colM="yellow" else colM="grey"
      lb<-worldLabel(metaResult,whichMeta)
      g<-addG(g,dataLegend(data.frame(names=strsplit(lb,"\n")[[1]],colours=c(colM,NA)),title="",shape=braw.env$plotShapes$meta))
    }
    return(g)

}

makeWorldDist<-function(metaResult,design,world,x,y,sigOnly=FALSE) {
  if (metaResult$metaAnalysis$analysisType=="random") {
    lambda<-metaResult$bestK
    shape<-metaResult$bestNull
    nullP<-0
  } else {
    lambda<-world$populationPDFk
    shape<-0
    nullP<-world$populationNullp
  }
  sigma<-1/sqrt(y-3)
  gain<-dgamma(y-braw.env$minN,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
  
  z<-c()
  switch (world$populationPDF,
          "Single"={
            for (i in 1:length(y)) {
              z1<-SingleSamplingPDF(atanh(x),lambda,sigma[i],shape)$pdf*(1-nullP)+
                SingleSamplingPDF(atanh(x),0,sigma[i])$pdf*nullP
              if (sigOnly) {
                zcrit<-qnorm(1-braw.env$alphaSig/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          },
          "Gauss"={
            for (i in 1:length(y)) {
              z1<-GaussSamplingPDF(atanh(x),lambda,sigma[i])$pdf*(1-nullP)+
                SingleSamplingPDF(atanh(x),0,sigma[i])$pdf*nullP
              if (sigOnly) {
                zcrit<-qnorm(1-braw.env$alphaSig/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          },
          "Exp"={
            for (i in 1:length(y)) {
              z1<-ExpSamplingPDF(atanh(x),lambda,sigma[i])$pdf*(1-nullP)+
                SingleSamplingPDF(atanh(x),0,sigma[i])$pdf*nullP
              if (sigOnly) {
                zcrit<-qnorm(1-braw.env$alphaSig/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          }
  )
  return(z)
}

drawWorld<-function(hypothesis,design,metaResult,g,colour="white",showTheory=FALSE) {
  world<-hypothesis$effect$world
  if (!world$worldOn) {
    world<-makeWorld(worldOn=TRUE,populationPDF="Single",populationRZ="r",
                     populationPDFk=hypothesis$effect$rIV,populationNullp=0)
  }
  
  x<-seq(-1,1,length.out=101)*braw.env$r_range
  y<-seq(5,braw.env$maxN,length.out=101)

  za<-makeWorldDist(metaResult,design,world,x,y)

  world$populationPDF<-metaResult$bestDist
  world$populationPDFk<-metaResult$bestK
  world$populationNullp<-metaResult$bestNull
  zb<-makeWorldDist(metaResult,design,world,x,y)
  
  if (braw.env$nPlotScale=="log10") {y<-log10(y)}
  
  x1<-as.vector(matrix(rep(x,101),101,byrow=TRUE))
  y1<-as.vector(matrix(rep(y,101),101,byrow=FALSE))
  za<-as.vector(za)
  zb<-as.vector(zb)

  use<-is.finite(za) & y1<braw.env$maxRandN*design$sN
  za<-za/max(za,na.rm=TRUE)
  ptsa<-data.frame(x=x1[use],y=y1[use],z=za[use])
  
  use<-is.finite(zb) & y1<braw.env$maxRandN*design$sN
  zb<-zb/max(zb,na.rm=TRUE)
  ptsb<-data.frame(x=x1[use],y=y1[use],z=zb[use])

  # white is the actual world
  # black is the best fit world
  if (showTheory) {
    g<-addG(g,dataContour(data=ptsa,colour="white",linetype="dotted"))
  }
  g<-addG(g,dataContour(data=ptsb,colour=colour,linewidth=0.5))
  return(g)
}
