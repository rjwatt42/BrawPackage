reRange<-function(r,plotRange,rRange=NULL) {
  if (is.null(rRange)) rRange<-c(min(r),max(r))
  r_range<-diff(rRange)
  r<-(r-rRange[1]+r_range/20)/(r_range*1.1)
  r<-r*(plotRange[2])+plotRange[1]
  
}

drawVar<-function(pts,ticks,var,plotArea=c(0,0,1,1),g){

  pts$r<-reRange(pts$r,plotArea[c(1,3)])
  pts$dens<-reRange(pts$dens*0.8,plotArea[c(2,4)]+plotArea[4]*c(1,-1)*0.2,c(0,1))

  ticks$breaks<-reRange(ticks$breaks,plotArea[c(1,3)])
  
  back<-data.frame(x=plotArea[1]+plotArea[3]*c(0,0,1,1),
                   y=reRange(c(0,1,1,0),plotArea[c(2,4)]+plotArea[4]*c(1,-1)*0.2,c(0,1)))
  axis<-data.frame(x=plotArea[1]+plotArea[3]*c(0,1),
                   y=reRange(c(0,1),plotArea[c(2,4)]+plotArea[4]*c(1,-1)*0.2,c(0,1)))

  g<-g+geom_polygon(data=back,aes(x=x,y=y),fill=braw.env$plotColours$graphBack, col=braw.env$plotColours$graphBack)
  g<-g+geom_polygon(data=pts,aes(x=r,y=dens),fill=braw.env$plotColours$sampleC, col="black")
  
    g+geom_line(data=axis,aes(x=x,y=y[1]),color="black",lwd=0.5)+
      geom_text(data=axis,aes(x=mean(x),y=y[1]),label=var$name,vjust=2.2,fontface="bold")+
      geom_text(data=ticks,aes(x=breaks,y=axis$y[1],label=labels),vjust=1.4,size=3)      
    }

shrinkString<-function(s,n) {return(substr(s,1,n))}

drawCategorical<-function(var,plotArea=c(0,0,1,1),g){
  ng<-var$ncats
  pp<-CatProportions(var)
  b<-(1:ng)*2-(ng+1)
  bt<-b
  
  r1<-c(-1, -1, 1, 1)*0.6
  d1<-c(0,1,1,0)
  
  r<-c()
  dens<-c()
  for (i in 1:length(b)){
    r<-c(r,r1+b[i])
    dens<-c(dens,d1*pp[i])
  }

  lt<-var$cases[1:ng]
  if (sum(sapply(lt,nchar))>12) {
    lt<-sapply(lt,shrinkString,ceil(12/ng))
  }

  xlim<-c(-ng,ng)+c(-1,1)*ng/10
  r<-c(xlim[1],r,xlim[2])
  dens<-c(0,dens,0)
  pts=data.frame(r=r,dens=dens)
  ticks<-data.frame(breaks=bt,labels=lt)
  
  g<-drawVar(pts,ticks,var,plotArea,g)
  
  if (var$deploy=="Within") {
    pts<-data.frame(x=b,y=pp*0.75)
    g<-g+geom_line(data=pts,aes(x=x,y=y),colour="gray",lwd=1)+
      geom_point(data=pts,aes(x=x,y=y),colour="black")
  }

  return(g)
}

drawOrdinal<-function(var,plotArea=c(0,0,1,1),g){
  r1<-c(-1, -1, 1, 1)*0.5
  d1<-c(0,1,1,0)
  
    ng<-var$nlevs
    pp<-OrdProportions(var)
    b<-(1:ng)
    bt<-b
    lt=1:ng

    
  r<-r1[1:3]+b[1]
  dens<-d1[1:3]*pp[1]
  for (i in 2:length(b)){
    r<-c(r,r1[2:3]+b[i])
    dens<-c(dens,d1[2:3]*pp[i])
  }
  r<-c(r,r[length(r)])
  dens<-c(dens,0)

  xlim<-c(min(r),max(r))+c(-1,1)*ng/10
  r<-c(xlim[1],r,xlim[2])
  dens<-c(0,dens,0)
  pts=data.frame(r=r,dens=dens)
  ticks<-data.frame(breaks=bt,labels=lt)
  drawVar(pts,ticks,var,plotArea,g)
  
}

drawInterval<-function(var,plotArea=c(0,0,1,1),g){
  r<-seq(-braw.env$fullRange,braw.env$fullRange,length.out=braw.env$varNPoints)*var$sd+var$mu
  if (var$skew!=0 || var$kurtosis!=0) {
    a<-f_johnson_M(var$mu,var$sd,var$skew,var$kurtosis)
    dens<-f_Johnson_pdf(r,a$coef,a$type)
    dens[is.na(dens)]<-0
  } else {
    dens<-dnorm(r,var$mu,var$sd) # exp(-0.5*((r-var$mu)/var$sd)^2)
  }
  # dens<-dJohnson(r,list())
  dens[1]=0; dens[length(dens)]=0
  
  pts=data.frame(r=r,dens=dens/max(dens))
  bt<-c(-2,-1,0,1,2)*var$sd+var$mu
  lt<-bt
  ticks<-data.frame(breaks=bt,labels=lt)
  
  drawVar(pts,ticks,var,plotArea,g)
}

#' show a variable object
#' 
#' @param variable a variable object
#' @returns a ggplot2 object
#' @examples
#' variable<-showVariable(variable=makeVariable())
#' @export
showVariable<-function(variable=makeVariable(),plotArea=c(0,0,1,1),g=NULL){
  if (is.null(g)) 
    g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0, 1))+braw.env$blankTheme
  switch(variable$type,
         "Interval"={g<-drawInterval(variable,plotArea,g)},
         "Ordinal"={g<-drawOrdinal(variable,plotArea,g)},
         "Categorical"={g<-drawCategorical(variable,plotArea,g)},
         "empty"={g<-g+drawVar(NULL,variable)}
  )
  return(g)
      
  
}
