drawSample<-function(result){
  IV<-result$IV
  IV2<-result$IV2
  DV<-result$DV
  effect<-result$effect
  
  # the population
  g<-plotPopulation(IV,DV,effect,alpha=0.75,theme=plotTheme)
  
  # the scattered points
  dotSize<-(plotTheme$axis.title$size)/3
  if (result$nval>100) {
    dotSize<-dotSize*sqrt(100/result$nval)
  }
  x<-result$ivplot
  y<-result$dvplot
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour = "black", fill = plotcolours$sampleC, size = dotSize)
  if (showMedians) {
    if (resultDV$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
      g<-g+geom_hline(yintercept=yuse,col="red")
      if (resultIV$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
      g<-g+geom_vline(xintercept=xuse,col="red")
  }
  g<-g+labs(x=result$IV$name,y=result$DV$name)+plotTheme
  g
  
}
