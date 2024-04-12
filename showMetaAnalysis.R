
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

drawSingleMeta<-function(metaResult=doMetaAnalysis(),showTheory=FALSE) {

  metaAnalysis<-metaResult$metaAnalysis
  hypothesis<-metaResult$hypothesis
  design<-metaResult$design
  
  d1<-metaResult$result$rIV
  d1n<-(metaResult$result$rpIV==0)
  x<-showAxis("r",hypothesis$effect)
  xlim<-x$lim
  disp1<-x$label
  if (all(d1>=0)) xlim[1]<-0
  
  d2<-metaResult$result$nval
  y<-showAxis("n",hypothesis$effect)
  disp2<-y$label
  ylim<-y$lim
  
  if (y$logScale) d2<-log10(d2)
  useAll<-(d2>ylim[1]) & (d2<ylim[2])
  ptsAll<-data.frame(x=d1[useAll],y=d2[useAll])
  useNull<-(d2>ylim[1]) & (d2<ylim[2] & d1n)
  ptsNull<-data.frame(x=d1[useNull],y=d2[useNull])
  
  g<-ggplot()+braw.env$plotRect
  g<-startPlot(xlim,ylim,box="both",top=FALSE,g=g)
  
  if (length(d1)>=1200) {
    nbins<-diff(ylim)/(2*IQR(d2[use])*length(d2[use])^(-0.33))*2
    nbins<-min(nbins,101)
    g<-g+stat_bin2d(data=pts,aes(x=x,y=y),bins=nbins)+scale_fill_gradientn(colours=c(braw.env$plotColours$graphBack,braw.env$plotColours$descriptionC))
  }
  
  g<-drawWorld(hypothesis,design,metaResult,g,braw.env$plotColours$descriptionC,showTheory=showTheory)
  
  dotSize<-braw.env$dotSize/2
  
  # show individual studies
  if (length(d1)<1200) {
    dotSize<-dotSize/(ceil(length(d1)/400))
    cl<-"black"
    g<-g+dataPoint(data=ptsAll,shape=braw.env$plotShapes$study, colour = cl, fill = braw.env$plotColours$descriptionC, size = dotSize)
    g<-g+dataPoint(data=ptsNull,shape=braw.env$plotShapes$study, colour = cl, fill = braw.env$plotColours$infer_nsigC, size = dotSize)
  }
  g<-g+xAxisLabel(disp1)+xAxisTicks(x$ticks)+yAxisLabel(disp2)+yAxisTicks(y$ticks)
  
  lb<-worldLabel(metaResult)
  pts_lb<-data.frame(x=mean(xlim),y=ylim[2]-diff(ylim)*0.1)
  g<-g+dataLabel(data=pts_lb,label=lb, hjust=0.5,colour="black",fill=braw.env$plotColours$descriptionC)
  
  return(g)
  
}

showMetaAnalysis<-function(metaResult=doMetaAnalysis(),showType="n-k") {
  g<-ggplot()+braw.env$plotRect
  braw.env$plotArea<-c(0,0,0.48,1)
  g<-drawMeta(metaResult=doMetaAnalysis(),whichMeta="Single",showType=showType,g)
  braw.env$plotArea<-c(0.39,0,0.36,1)
  g<-drawMeta(metaResult=doMetaAnalysis(),whichMeta="Gauss",showType=showType,g)
  assign("plotArea",c(0.66,0,0.36,1),braw.env)
  g<-drawMeta(metaResult=doMetaAnalysis(),whichMeta="Exp",showType=showType,g)
  print(g)
}

drawMeta<-function(metaResult=doMetaAnalysis(),whichMeta="Single",showType="n-k",g=ggplot()+braw.env$plotRect) {
  
  metaAnalysis<-metaResult$metaAnalysis

  n1<-sum(metaResult$bestDist=="Single")
  n2<-sum(metaResult$bestDist=="Gauss")
  n3<-sum(metaResult$bestDist=="Exp")
  sAll<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax)
  
  xlim<-c(-1,1)
  xticks<-seq(-1,1,0.5)
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
            },
            "S-S"={
              x<-metaResult$gauss$Smax
              yS<-metaResult$exp$Smax
              y1<-yS
              xlim<-c(min(x),max(x))
              xlim<-xlim+c(-1,1)*diff(xlim)/10
              xticks<-c()
            }
    )
    keep<- !is.na(x) & !is.na(yS)
    best<-metaResult$bestS[keep]
    yS<-yS[keep]
    y1<-y1[keep]
    x<-x[keep]
    
    if (isempty(x)) {return(ggplot()+plotBlankTheme)}
    
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
              ylabel<-bquote(bold(p['null']))
              xlabel<-braw.env$Llabel
            },
            "S-S"={
              y<-yS
              xlim<-c(min(sAll,na.rm=TRUE),max(sAll,na.rm=TRUE))+c(-1,1)*(max(sAll,na.rm=TRUE)-min(sAll,na.rm=TRUE))/4
              xlabel<-"log(lk Gauss)"
              ylim<-xlim
              ylabel<-"log(lk Exp)"
              useBest<- (y>x & metaResult$effect$world$populationPDF=="Exp") | (y<x & metaResult$effect$world$populationPDF=="Gauss")
            }
    )
    pts<-data.frame(x=x,y=y)
      
      if (braw.env$plotArea[1]==0)  
            g<-startPlot(xlim,ylim,box="both",top=TRUE,g=g)+yAxisLabel(ylabel)+yAxisTicks(yticks)
      else  g<-startPlot(xlim,ylim,box="x",top=TRUE,g=g)
      g<-g+xAxisLabel(xlabel)+xAxisTicks(xticks)
      
      dotSize=min(4,max(4,sqrt(50/length(x))))
      
      g<-g+dataPoint(data=pts,shape=braw.env$plotShapes$meta, colour = "black", fill = "grey", size = dotSize)
      pts<-data.frame(x=x[useBest],y=y[useBest])
      g<-g+dataPoint(data=pts,shape=braw.env$plotShapes$meta, colour = "black", fill = "yellow", size = dotSize)
      
      if (showType=="S-S") {
        g<-g+dataPath(data=data.frame(x=xlim,y=ylim),colour="red")
      }

    if (mean(y1)>0.5) {
      yp<-ylim[1]+diff(ylim)/10
      vj<-0
    } else {
        yp<-ylim[2]-diff(ylim)/10
        vj<-1
        }
    if (showType=="S-S") {
      fullText<-paste0("Exp","(",format(mean(metaResult$exp$Kmax),digits=3),"\u00B1",format(std(metaResult$exp$Kmax),digits=2),")")
      if (metaAnalysis$meta_nullAnal) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaResult$exp$Nullmax),digits=3),"\u00B1",format(std(metaResult$exp$Nullmax),digits=2))
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaResult$exp$Smax),digits=2),"\u00B1",format(std(metaResult$exp$Smax),digits=2)," (",format(sum(y>x)),"/",length(metaResult$bestDist),")")
      pts_lb<-data.frame(x=xlim[1], y=ylim[2], lb=fullText)
      if (mean(y>x)) {
      g<-g+geom_label(data=pts_lb,aes(x=x,y=y,label=lb),hjust=0,vjust=1,size=labelSize,fill="yellow")
      } else {
        g<-g+geom_label(data=pts_lb,aes(x=x,y=y,label=lb),hjust=0,vjust=1,size=labelSize,fill="grey")
      }
      fullText<-paste0("Gauss","(",format(metaResult$gauss$Kmax,digits=3),"\u00B1",format(std(metaResult$gauss$Kmax),digits=2),")")
      if (metaAnalysis$meta_nullAnal) {
        fullText<-paste0(fullText,"\nnull=",format(mean(metaResult$gauss$Nullmax),digits=3),"\u00B1",format(std(metaResult$gauss$Nullmax),digits=2))
      }
      fullText<-paste0(fullText,"\nS= ",format(mean(metaResult$gauss$Smax),digits=2),"\u00B1",format(std(metaResult$gauss$Smax),digits=2)," (",format(sum(x>y)),"/",length(metaResult$bestDist),")")
      pts_lb<-data.frame(x=xlim[2], y=ylim[1], lb=fullText)
      if (mean(y>x)) {
        g<-g+geom_label(data=pts_lb,aes(x=x,y=y,label=lb),hjust=1,vjust=0,size=labelSize,fill="grey")
      } else {
        g<-g+geom_label(data=pts_lb,aes(x=x,y=y,label=lb),hjust=1,vjust=0,size=labelSize,fill="yellow")
      }
    } else {
      
      lb<-worldLabel(metaResult,whichMeta)
      pts_lb<-data.frame(x=mean(x), y=yp)
      
      use<-which.max(c(n1,n2,n3))
      bestD<-c("Single","Gauss","Exp")[use]
      g<-g+plotTitle(lb,"left",size=1)
      # if (whichMeta==bestD) {
        # g<-g+dataLabel(data=pts_lb,lb,hjust=0.5,vjust=vj,fill="yellow")
      # } else {
      #   g<-g+dataLabel(data=pts_lb,lb,hjust=0.5,vjust=vj,fill="grey")
      # }
    }
    return(g)

}

makeWorldDist<-function(world,x,y,sigOnly=FALSE) {
  lambda<-world$populationPDFk
  nullP<-world$populationNullp
  sigma<-1/sqrt(y-3)
  gain<-dgamma(y-braw.env$minN,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
  
  z<-c()
  switch (world$populationPDF,
          "Single"={
            for (i in 1:length(y)) {
              z1<-SingleSamplingPDF(atanh(x),lambda,sigma[i])$pdf*(1-nullP)+
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
  
  x<-seq(-1,1,length.out=101)*braw.env$r_range
  y<-seq(5,braw.env$maxN,length.out=101)

  za<-makeWorldDist(world,x,y)

  world$populationPDF<-metaResult$bestDist
  world$populationPDFk<-metaResult$bestK
  world$populationNullp<-metaResult$bestNull
  zb<-makeWorldDist(world,x,y)
  
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
    g<-g+dataContour(data=ptsa,colour="white")
  }
  g<-g+dataContour(data=ptsb)
  return(g)
}
