

showLikelihood<-function(result=braw.res$result,showType="mean(R+)",
                         prior=NULL,norm=FALSE,
                         fontsize=1.2,markRs=NULL,
                         plotArea=c(0,0,1,1),g=NULL,new=TRUE) {
  world<-result$hypothesis$effect$world
  design<-result$design
  evidence<-result$evidence

  rs<-result$rIV
  n<-result$nval
  if (is.null(n)) n<-design$sN
  if (length(n)<length(rs)) n<-rep(n,length(rs))
  
  switch(showType,
         "rp"={
           if (braw.env$RZ=="z") range<-seq(-1,1,length.out=1001)*1.5
           else range<-atanh(seq(-0.9,0.9,length.out=201))
           xlabel<-"r[p]"
           if (braw.env$RZ=="z") xlabel<-sub("r","z",xlabel)
           dens<-getLogLikelihood(atanh(rs),n,1,"Single",location=range,bias=0)
           if (!is.null(prior)) 
             dens<-dens+log(zPopulationDist(range,prior))
           if (braw.env$RZ=="r") {
             range<-tanh(range)
           }
         },
    "mean(R+)"={
      if (world$PDF=="Single") {
        range<-seq(-0.9,0.9,length.out=201)
      }
      else {
        range<-seq(0,0.6,length.out=201)
      }
      xlabel<-sub("[+]","[+]",showType)
      if (braw.env$RZ=="z") xlabel<-sub("R","Z",xlabel)
      dens<-getLogLikelihood(atanh(rs),n,1,world$PDF,location=range,bias=evidence$sigOnly)
    }
  )
  dens[is.infinite(dens)]<-NA
  if (norm) dens<-dens-max(dens,na.rm=TRUE)
  dens<-exp(dens)
  dens[is.na(dens)]<-0
  use<-which.max(dens)
  
  xlim<-c(min(range),max(range))
  ylim<-c(min(dens),max(dens))+c(-1,1)*(max(dens)-min(dens))*0.2
  ylim[1]<-0
  
  braw.env$plotArea<-plotArea
  if (plotArea[3]>0.45) ylabel<-"S" else ylabel=""
  if (new)
  g<-startPlot(xlim=xlim,ylim=ylim,top=TRUE,
               xlabel=makeLabel(xlabel),ylabel=makeLabel(ylabel),
               xticks=makeTicks(),yticks=NULL,
               fontScale=fontsize,g=g
               )
  
  n<-length(range)
  col<-darken(braw.env$plotColours$sampleC,off=0)
  g<-addG(g,dataPath(data.frame(x=range,y=dens),colour=col,linewidth=1.25))
  g<-addG(g,dataPath(data.frame(x=c(0,0)+range[use],y=c(0,dens[use])),linetype="dotted"))

  if (!is.null(markRs)) {
      h<-approx(range,dens,markRs)$y
      g<-addG(g,dataPoint(data.frame(x=markRs,y=h),fill="#CCCCCC"))
  }
  
  g<-addG(g,dataPoint(data.frame(x=range[use],y=dens[use])))
  g<-addG(g,dataText(data.frame(x=range[use],y=dens[use]+diff(ylim)/30),
                     paste0("MLE = ",brawFormat(range[use])),
                     fontface="bold",size=0.65))
  
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