plotSample<-function(IV,DV,effect,ivplot,dvplot,g=NULL) {
  if (is.null(g)) 
    g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0, 1))+braw.env$blankTheme

  # the population
  g<-plotPopulation(IV,DV,effect,alpha=1,theme=braw.env$plotTheme,g)
  
  # the scattered points
  dotSize<-(braw.env$plotTheme$axis.title$size)/3
  if (length(ivplot)>100) {
    dotSize<-dotSize*sqrt(100/length(ivplot))
  }
  
  x<-ivplot
  y<-dvplot
  pts<-data.frame(x=x,y=y)
  g<-g+dataPoint(data=pts,shape=braw.env$plotShapes$data, colour = "black", fill = braw.env$plotColours$sampleC, size = dotSize)
  if (braw.env$showMedians) {
    if (sample$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
    g<-g+horizLine(intercept=yuse,col="red")
    if (sample$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
    g<-g+vertLine(intercept=xuse,col="red")
  }
  # g<-g+labs(x=IV$name,y=DV$name)+braw.env$plotTheme
  g
  
}

#' show a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showSample(sample=makeSample())
#' @export
showSample<-function(sample=makeSample()){
  IV<-sample$hypothesis$IV
  IV2<-sample$hypothesis$IV2
  DV<-sample$hypothesis$DV
  effect<-sample$hypothesis$effect
  
  if (is.null(IV2)) {
    braw.env$plotArea<-c(0,0,1,1)
    g<-plotSample(IV,DV,effect,sample$ivplot,sample$dvplot)
  } else {
    braw.env$plotArea<-c(0,0,0.45,0.45)
    g<-plotSample(IV,IV2,effect,sample$ivplot,sample$iv2plot)
    braw.env$plotArea<-c(0.55,0,0.45,0.45)
    g<-plotSample(IV,DV,effect,sample$ivplot,sample$dvplot,g)
    braw.env$plotArea<-c(0.55/2,0.55,0.45,0.45)
    g<-plotSample(IV2,DV,effect,sample$iv2plot,sample$dvplot,g)
  }
  braw.env$plotArea<-c(0,0,1,1)
  
  return(g)
}
