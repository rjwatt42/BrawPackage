plotSample<-function(IV,DV,effect,ivplot,dvplot) {
  # the population
  g<-plotPopulation(IV,DV,effect,alpha=0.75,theme=braw.env$plotTheme)
  
  # the scattered points
  dotSize<-(braw.env$plotTheme$axis.title$size)/3
  if (length(ivplot)>100) {
    dotSize<-dotSize*sqrt(100/length(iv))
  }
  
  x<-ivplot
  y<-dvplot
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=braw.env$plotShapes$data, colour = "black", fill = braw.env$plotColours$sampleC, size = dotSize)
  if (braw.env$showMedians) {
    if (sample$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
    g<-g+geom_hline(yintercept=yuse,col="red")
    if (sample$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
    g<-g+geom_vline(xintercept=xuse,col="red")
  }
  g<-g+labs(x=IV$name,y=DV$name)+braw.env$plotTheme
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
    g<-plotSample(IV,DV,effect,sample$ivplot,sample$dvplot)
  } else {
    g1<-plotSample(IV,IV2,effect,sample$ivplot,sample$iv2plot)
    g2<-plotSample(IV,DV,effect,sample$ivplot,sample$dvplot)
    g3<-plotSample(IV2,DV,effect,sample$iv2plot,sample$dvplot)
    g<-joinPlots(g1,g2,g3)
  }
  
  
  return(g)
}
