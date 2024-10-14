plotSample<-function(IV,DV,effect,ivplot,dvplot,g=NULL) {

  # the population
  g<-plotPopulation(IV,DV,effect,alpha=1,g)
  
  # the scattered points
  dotSize<-braw.env$dotSize
  if (length(ivplot)>100) {
    dotSize<-dotSize*sqrt(100/length(ivplot))
  }
  
  x<-ivplot
  y<-dvplot
  pts<-data.frame(x=x,y=y)
  g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour = "black", fill = braw.env$plotColours$sampleC, size = dotSize))
  if (braw.env$showMedians) {
    if (sample$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
    g<-addG(g,horizLine(intercept=yuse,col="red"))
    if (sample$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
    g<-addG(g,vertLine(intercept=xuse,col="red"))
  }
  g
  
}

#' show a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showSample(sample=doSample())
#' @export
showSample<-function(sample=braw.res$result,marginals=braw.env$newSampleDisplay){
  if (is.null(sample)) sample<-doResult(autoShow=FALSE)
  
  if (marginals) {
    g<-showMarginals(sample)
    return(g)
  }
  
  IV<-sample$hypothesis$IV
  IV2<-sample$hypothesis$IV2
  DV<-sample$hypothesis$DV
  effect<-sample$hypothesis$effect
  if (!is.null(sample$rIV)) effect$rIV<-sample$rIV
  
  if (is.null(IV2)) {
    braw.env$plotArea<-c(0,0,1,1)
    g<-plotSample(IV,DV,effect,sample$ivplot,sample$dvplot)
  } else {
    braw.env$plotArea<-c(0,0,0.55,0.55)
    g<-plotSample(IV,IV2,effect,sample$ivplot,sample$iv2plot)
    braw.env$plotArea<-c(0.45,0,0.55,0.55)
    g<-plotSample(IV,DV,effect,sample$ivplot,sample$dvplot,g)
    braw.env$plotArea<-c(0.2,0.5,0.55,0.55)
    g<-plotSample(IV2,DV,effect,sample$iv2plot,sample$dvplot,g)
  }
  braw.env$plotArea<-c(0,0,1,1)
  
  return(g)
}
