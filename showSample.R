showSample<-function(sample=makeSample()){
  IV<-sample$hypothesis$IV
  IV2<-sample$hypothesis$IV2
  DV<-sample$hypothesis$DV
  effect<-sample$hypothesis$effect
  
  # the population
  g<-plotPopulation(IV,DV,effect,alpha=0.75,theme=BrawOpts$plotTheme)
  
  # the scattered points
  dotSize<-(BrawOpts$plotTheme$axis.title$size)/3
  if (sample$nval>100) {
    dotSize<-dotSize*sqrt(100/sample$nval)
  }
  x<-sample$ivplot
  y<-sample$dvplot
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=BrawOpts$plotShapes$data, colour = "black", fill = BrawOpts$plotColours$sampleC, size = dotSize)
  if (showMedians) {
    if (sample$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
      g<-g+geom_hline(yintercept=yuse,col="red")
      if (sample$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
      g<-g+geom_vline(xintercept=xuse,col="red")
  }
  g<-g+labs(x=IV$name,y=DV$name)+BrawOpts$plotTheme
  g
  
}
