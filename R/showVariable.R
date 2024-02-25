
drawVar<-function(pts,var){
  if (is.null(pts)) {
    pts<-data.frame(x=0,y=0.5,t=var$name)
    ggplot()+geom_label(data=pts,aes(x=x,y=y,label=t),hjust=0.5, vjust=0.5, size=11, fontface="bold",label.size = NA)+
      labs(x="",y="")+
      braw.env$diagramTheme+
      theme(plot.margin=margin(1.15,1.8,1,1.25,"cm"))+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()
      )+
      theme(plot.margin=margin(0.0,-0.2,0,-1,"cm"))+
    coord_cartesian(xlim = c(-1,1), ylim = c(0, 1))
  } else {
  ggplot(pts,aes(x=r,y=dens))+geom_polygon(fill=braw.env$plotColours$sampleC)+
    geom_line(lwd=0.25,color="black")+
      # geom_line(aes(x=r,y=dens*0),color="black",lwd=0.5)+
      labs(x=var$name,y="")+
      braw.env$diagramTheme+
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(plot.margin=margin(0.0,-0.2,0,-1,"cm"))
    
}
}

shrinkString<-function(s,n) {return(substr(s,1,n))}

drawCategorical<-function(var){
  ng<-var$ncats
  pp<-CatProportions(var)
  b<-(1:ng)*2-(ng+1)
  
  r1<-c(-1, -1, 1, 1)*0.6
  d1<-c(0,1,1,0)
  
  r<-c()
  dens<-c()
  for (i in 1:length(b)){
    r<-c(r,r1+b[i])
    dens<-c(dens,d1*pp[i])
  }

  l<-var$cases[1:ng]
  if (sum(sapply(l,nchar))>12) {
    l<-sapply(l,shrinkString,ceil(12/ng))
  }

  xlim<-c(-ng,ng)+c(-1,1)*ng/10
  r<-c(xlim[1],r,xlim[2])
  dens<-c(0,dens,0)
  pts=data.frame(r=r,dens=dens)
  
  g<-drawVar(pts,var)
  if (var$deploy=="Within") {
    pts<-data.frame(x=b,y=pp*0.75)
    g<-g+geom_line(data=pts,aes(x=x,y=y),colour="gray",lwd=1)+
      geom_point(data=pts,aes(x=x,y=y),colour="black")
  }
    g+scale_x_continuous(breaks=b,labels=l)+
    coord_cartesian(xlim = xlim, ylim = c(0, 1.2))
  
}

drawOrdinal<-function(var){
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
  
  drawVar(pts,var)+
    scale_x_continuous(breaks=bt,labels=lt)+
    coord_cartesian(xlim = xlim, ylim = c(0, 1.2))
  
}

drawInterval<-function(var){
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
  xlim<-c(-1,1)*3*var$sd+var$mu
  drawVar(pts,var)+
    coord_cartesian(xlim = xlim, ylim = c(0, 1.2))
}

#' show a variable object
#' 
#' @param variable a variable object
#' @returns a ggplot2 object
#' @examples
#' variable<-showVariable(variable=makeVariable())
#' @export
showVariable<-function(variable=makeVariable(),single=FALSE){
  switch(variable$type,
         "Interval"={g<-drawInterval(variable)},
         "Ordinal"={g<-drawOrdinal(variable)},
         "Categorical"={g<-drawCategorical(variable)},
         "empty"={g<-drawVar(NULL,variable)}
  )
  if (single)
  g+theme(plot.margin=margin(0.5,0.5,0.0,0.0,"cm"))
  else g
}
