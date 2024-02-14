showSample<-function(sample=makeSample()){
  IV<-sample$IV
  IV2<-sample$IV2
  DV<-sample$DV
  effect<-sample$effect
  
  # the population
  g<-plotPopulation(IV,DV,effect,alpha=0.75,theme=plotTheme)
  
  # the scattered points
  dotSize<-(plotTheme$axis.title$size)/3
  if (result$nval>100) {
    dotSize<-dotSize*sqrt(100/result$nval)
  }
  x<-sample$ivplot
  y<-sample$dvplot
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour = "black", fill = plotcolours$sampleC, size = dotSize)
  if (showMedians) {
    if (sample$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
      g<-g+geom_hline(yintercept=yuse,col="red")
      if (sample$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
      g<-g+geom_vline(xintercept=xuse,col="red")
  }
  g<-g+labs(x=sample$IV$name,y=sample$DV$name)+plotTheme
  g
  
}
