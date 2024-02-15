
mv2dens<-function(x,rho,break1,break2){
  mu=c(0,0)
  sigma=matrix(c(1,rho,rho,1),ncol=2,byrow=TRUE)
  xd=x[2]-x[1]
  xi=c(x[1]-xd/2,x+xd/2)
  xd<-matrix(c(xi,rep(break1[1],length(xi))),ncol=2,byrow=FALSE)
  z1=diff(pmnorm(xd,mu,sigma))
  xd<-matrix(c(xi,rep(break2[1],length(xi))),ncol=2,byrow=FALSE)
  z2=diff(pmnorm(xd,mu,sigma))
  z2-z1
}


erf<-function(z){
  2*pnorm(sqrt(2)*z) - 1
}

plotCatPositions<-function(ncats){
  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-exp(-qnorm(pbreaks)^2/2)
  -1/sqrt(2*pi)*diff(ebreaks)/diff(pbreaks)
}

plotRibbon<-function(x,y,yoff) {
  np<-length(x)
  xshape<-(c(0,0,1,1)-0.5)*(x[2]-x[1])
  yshape<-(c(0,1,1,0)-0.5)
  ids<-1:np
  xd<-rep(xshape,np)+rep(x,each=4)
  yd<-rep(yshape,np)
  yo<-rep(yoff,np*4)
  id<-rep(ids,each=4)+(yoff-1)*np
  vd<-rep(y,each=4)
  pts<-data.frame(x=xd,y=yd,yoff=yo,value=vd,ids=id)
  return(pts)
}

plotParParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  theta=seq(0,2*pi,length.out=varNPoints)
  d<-acos(rho)
  x=cos(theta+d/2)
  y=cos(theta-d/2)
  y<-(y-x*rho)*(1+x/3*Heteroscedasticity)+x*rho
  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  radius<-qnorm(seq(0.55,0.95,0.1))*1.5
  for (ir in 1:length(radius)) {
    pts<-data.frame(x=x*radius[ir]*IV$sd+IV$mu,y=y*radius[ir]*DV$sd+DV$mu)
    g<-g+geom_polygon(data=pts,aes(x=x,y=y), fill = BrawOpts$plotColours$sampleC, color=NA, alpha=alpha/(length(radius)-2))
  }
  return(g+scale_alpha_continuous(range = c(0, 1)))
}

plotOrdParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ng<-IV$nlevs
  pp<-OrdProportions(IV)
  pp<-pp/max(pp)
  l=paste(1:ng,sep="")
  b<-1:ng
  
  pbreaks<-seq(0,1,1/(ng))
  ebreaks<-(pbreaks-0.5)*2
  
  y<-seq(-1,1,length.out=varNPoints)*fullRange
  np<-length(y)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ng) {
    x<-mv2dens(y,rho,ebreaks[id],ebreaks[id+1])*pp[id]
    pts<-rbind(pts,plotRibbon(y,x,id))
  }
  pts$value<-pts$value-min(pts$value)
  pts$value<-pts$value/max(pts$value)
  # pts$y<-pts$y*pts$value+pts$yoff
  pts$y<-pts$y+pts$yoff
  
  g<-ggplot(pts,aes(x=y,y=x))
  g<-g+geom_polygon(data=pts,aes(x=y,y=x*DV$sd+DV$mu,group=ids,alpha=alpha*value),fill=BrawOpts$plotColours$sampleC,color=NA,show.legend=FALSE)
  g+scale_x_continuous(breaks=b,labels=l)+scale_alpha_continuous(range = c(0, 1))
  
}

plotCatParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats<-IV$ncats
  l<-IV$cases
  if (sum(sapply(l,nchar))>12) {
    l<-sapply(l,shrinkString,ceil(12/length(l)))
  }
  pp<-CatProportions(IV)
  b<-(1:ncats)-1

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  y<-seq(-1,1,length.out=varNPoints)*fullRange
  yshape<-c(y,rev(y))
  if (length(IV$vals)>0){
    # dealing with a sample
    muv<-array(0,ncats)
    sdv<-array(0,ncats)
    for (id in 1:ncats) {
      muv[id]<-mean(DV$vals[IV$vals==IV$cases[id]],na.rm=TRUE)
      sdv[id]<-sd(DV$vals[IV$vals==IV$cases[id]],na.rm=TRUE)
    }
    mu_order<-order(muv)
  } else {
    muv<-array(DV$mu,ncats)
    sdv<-array(DV$sd,ncats)
    mu_order<-1:ncats
    if (rho<0) {mu_order<-rev(mu_order)}
  }
  hsy<-1+seq(-1,1,length.out=ncats)*Heteroscedasticity

  np<-length(y)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ncats) {
    use<-mu_order[id]
    x<-mv2dens(y,abs(rho),ebreaks[use],ebreaks[use+1])*pp[use]
    pts<-rbind(pts,plotRibbon(y,x,b[id]))
  }
  pts$value<-pts$value/max(pts$value)
  pts$y<-pts$y*pts$value*0.9+pts$yoff

  g<-ggplot(pts,aes(x=y,y=x))
  g<-g+geom_polygon(data=pts,aes(x=y,y=x*DV$sd+DV$mu,group=ids,alpha=alpha*value),fill=BrawOpts$plotColours$sampleC,color=NA,show.legend=FALSE)
  g+scale_x_continuous(breaks=b,labels=l)+scale_alpha_continuous(range = c(0, 1))
  
  #   
  # pts=data.frame(x=y*0,y=y)
  # g<-ggplot(pts,aes(x=x,y=y))
  # for (id in 1:ncats) {
  #   use<-mu_order[id]
  #       x<-mv2dens(y,abs(rho),ebreaks[use],ebreaks[use+1])
  #   x<-x/max(x,na.rm=TRUE)*(b[2]-b[1])/2.2
  #   xshape<-c(-x,rev(x))*pp[id]
  #   pts<-data.frame(x=xshape+b[id],y=yshape*hsy[id]*sdv[id]+muv[id])
  #   g<-g+
  #     geom_polygon(data=pts,aes(x=x,y=y),fill = BrawOpts$plotColours$sampleC,color=NA,alpha=alpha)
  # }
  # g+scale_x_continuous(breaks=b,labels=l)
  # 
}

plotParOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ng<-DV$nlevs
  pp<-OrdProportions(DV)
  pp<-pp/max(pp)
  l=paste(1:ng,sep="")
  b<-1:ng

  pbreaks<-seq(0,1,1/(ng))
  ebreaks<-(pbreaks-0.5)*2

  x<-seq(-1,1,length.out=varNPoints)*fullRange
  np<-length(x)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ng) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])*pp[id]
    pts<-rbind(pts,plotRibbon(x,y,id))
  }
  pts$value<-pts$value-min(pts$value)
  pts$value<-pts$value/max(pts$value)
  pts$y<-pts$y+pts$yoff
  
  g<-ggplot(pts,aes(x=x*IV$sd+IV$mu,y=y))
  g<-g+geom_polygon(data=pts,aes(x=x*IV$sd+IV$mu,y=y,group=ids,alpha=alpha*value),fill=BrawOpts$plotColours$sampleC,color=NA,show.legend=FALSE)
  g+scale_y_continuous(breaks=b,labels=l)+scale_alpha_continuous(range = c(0, 1))
  
}

plotCatOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats1<-IV$ncats
  pp1<-CatProportions(IV)
  l1=IV$cases
  if (sum(sapply(l1,nchar))>12) {
    l1<-sapply(l1,shrinkString,ceil(12/length(l1)))
  }
  b1<-(1:ncats1)-1
  
  ng2<-DV$nlevs
  b2<-1:ng2
  l2=1:ng2
  
  division<-r2CatProportions(rho,ncats1,ng2)  
  pp<-OrdProportions(DV)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b1))/2*0.9
  y<-c(-1,1,1,-1)*min(diff(b2))/2*0.99
  
  pts=data.frame(x=c(),y=c(),value=c(),ids=c())
  idc<-0
  for (ix in 1:ncats1) {
    for (iy in 1:ng2) {
      idc<-idc+1
      newpts<-data.frame(x=x*s[iy,ix]*pp1[ix]*pp[iy]+b1[ix], y=y+b2[iy], value=rep(s[iy,ix]*pp1[ix]*pp[iy],4),ids=idc)
      pts<-rbind(pts,newpts)
    }
  }
  pts$value<-pts$value/max(pts$value)

  g<-ggplot()
  g<-g+
    geom_polygon(data=pts,aes(x=x,y=y,group=ids,alpha=alpha*value),fill = BrawOpts$plotColours$sampleC,colour=NA,show.legend=FALSE)
  g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)+scale_alpha_continuous(range = c(0, 1))
}

plotOrdCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats2<-DV$ncats
  pp2<-CatProportions(DV)
  l2=DV$cases
  b2<-(1:ncats2)-1
  
  ng1<-IV$nlevs
  b1<-1:ng1
  l1=1:ng1
  
  division<-r2CatProportions(rho,ncats2,ng1)  
  pp<-OrdProportions(IV)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b2))/2*0.999
  y<-c(-1,1,1,-1)*min(diff(b1))/2*0.9
  
  pts=data.frame(x=c(),y=c(),value=c(),ids=c())
  idc<-0
  for (iy in 1:ncats2) {
    for (ix in 1:ng1) {
      idc<-idc+1
      newpts<-data.frame(y=y*s[ix,iy]*pp[ix]*pp2[iy]+b2[iy], x=x+b1[ix], value=rep(s[ix,iy]*pp[ix]*pp2[iy],4),ids=idc)
      pts<-rbind(pts,newpts)
    }
  }
  pts$value<-pts$value/max(pts$value)
  
  g<-ggplot()
  g<-g+
    geom_polygon(data=pts,aes(x=x,y=y,group=ids,alpha=alpha*value),fill = BrawOpts$plotColours$sampleC,colour=NA,show.legend=FALSE)
  g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)+scale_alpha_continuous(range = c(0, 1))
  
  # 
  # 
  # pts=data.frame(x=x,y=y)
  # g<-ggplot(pts,aes(x=x,y=y))
  # for (iy in 1:ncats2) {
  #   for (ix in 1:ng1) {
  #     # xoff<-sign(b1[ix])*abs(x[1])*(1-s[iy,ix])
  #     # yoff<-sign(b2[iy])*abs(y[1])*(1-s[iy,ix])
  #     # pts<-data.frame(x=x*s[iy,ix]+b1[ix]-xoff, y=y*s[iy,ix]+b2[iy]-yoff)
  #     pts<-data.frame(y=y*pp[ix]*pp2[iy]+b2[iy], x=x*s[ix,iy]+b1[ix])
  #     g<-g+
  #       geom_polygon(data=pts,aes(x=x,y=y),fill = BrawOpts$plotColours$sampleC,colour=NA,alpha=alpha*pp[ix])
  #   }
  # }
  # g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)
}

plotParCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats<-DV$ncats
  pp<-CatProportions(DV)
  l=DV$cases
  b<-(1:ncats)-1

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  x<-seq(-1,1,length.out=varNPoints)*fullRange
  np<-length(x)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ncats) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])*pp[id]
    pts<-rbind(pts,plotRibbon(x,y,id-1))
  }
  pts$value<-pts$value/max(pts$value)
  pts$y<-pts$y*pts$value*0.9+pts$yoff
  
  g<-ggplot(pts,aes(x=x*IV$sd+IV$mu,y=y))
  g<-g+geom_polygon(data=pts,aes(x=x*IV$sd+IV$mu,y=y,group=ids,alpha=alpha*value),fill=BrawOpts$plotColours$sampleC,color=NA,show.legend=FALSE)
  g+scale_y_continuous(breaks=b,labels=l)+scale_alpha_continuous(range = c(0, 1))
  
  # 
  # 
  # pts=data.frame(x=x,y=x*0)
  # g<-ggplot(pts,aes(x=x,y=y))
  # xshape<-c(x,rev(x))
  # for (id in 1:ncats) {
  #   y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])
  #   y<-y/max(y)/2.5
  #   
  #   yshape<-c(-y,rev(y))*pp[id]
  #   pts<-data.frame(x=xshape*IV$sd+IV$mu,y=yshape+b[id])
  #   g<-g+
  #     geom_polygon(data=pts,aes(x=x,y=y),fill = BrawOpts$plotColours$sampleC,color=NA,alpha=alpha)
  # }
  # g+scale_y_continuous(breaks=b,labels=l)

}

plotCatCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats1<-IV$ncats
  pp1<-CatProportions(IV)
  b1<-(1:ncats1)-1
  l1=IV$cases
  
  ncats2<-DV$ncats
  pp2<-CatProportions(DV)
  b2<-(1:ncats2)-1
  l2=DV$cases
  
  division<-r2CatProportions(rho,ncats1,ncats2)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b1))/2*0.9
  y<-c(-1,1,1,-1)*min(diff(b2))/2*0.9
  
  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  for (ix in 1:ncats1) {
    for (iy in 1:ncats2) {
      # xoff<-sign(b1[ix])*abs(x[1])*(1-s[iy,ix])
      # yoff<-sign(b2[iy])*abs(y[1])*(1-s[iy,ix])
      # pts<-data.frame(x=x*s[iy,ix]+b1[ix]-xoff, y=y*s[iy,ix]+b2[iy]-yoff)
      pts<-data.frame(x=x*s[iy,ix]*pp1[ix]*pp2[iy]+b1[ix], y=y*s[iy,ix]*pp1[ix]*pp2[iy]+b2[iy])
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),fill = BrawOpts$plotColours$sampleC,colour=NA,alpha=alpha)
    }
  }
  g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)+scale_alpha_continuous(range = c(0, 1))
}

plotOrdOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  nlevs1<-IV$nlevs
  pp1<-OrdProportions(IV)
  b1<-1:nlevs1

  nlevs2<-DV$nlevs
  pp2<-OrdProportions(DV)
  b2<-1:nlevs2

  division<-r2CatProportions(rho,nlevs1,nlevs2)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b1))/2
  y<-c(-1,1,1,-1)*min(diff(b2))/2
  
  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  for (ix in 1:nlevs1) {
    for (iy in 1:nlevs2) {
      pts<-data.frame(x=x+b1[ix], y=y+b2[iy])
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),fill = BrawOpts$plotColours$sampleC,colour=NA,alpha=alpha*pp1[ix]*pp2[iy])
    }
  }
  g+scale_x_continuous(breaks=b1)+scale_y_continuous(breaks=b2)+scale_alpha_continuous(range = c(0, 1))
}

plotPopulation<-function(IV,DV,effect,alpha=1,theme=BrawOpts$diagramTheme){
  rho<-effect$rIV
  if (is.na(rho)) {rho<-0}
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  heteroscedasticity<-effect$Heteroscedasticity[1]
  
  if (IV$type=="empty" || DV$type=="empty") {
    pts<-data.frame(x=100,y=100)
    g<-ggplot(pts,aes(x=x,y=y))
    if (IV$type!="empty") {
      switch (IV$type, 
              "Categorical"={
                ncats1<-IV$ncats
                b1<-(1:ncats1)-1
                l1=IV$cases
                g<-g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=NULL)+coord_cartesian(xlim = c(0,ncats1+1)-1,ylim=c(0,1))
                },
              "Interval"={
                g<-g+scale_x_continuous()+scale_y_continuous(breaks=NULL)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu,ylim=c(0,1))
                }
      )
    }
    if (DV$type!="empty") {
      switch (DV$type, 
              "Categorical"={
                ncats1<-DV$ncats
                b1<-(1:ncats1)-1
                l1=DV$cases
                g<-g+scale_y_continuous(breaks=b1,labels=l1)+scale_x_continuous(breaks=NULL)+coord_cartesian(ylim = c(0,ncats1+1)-1,xlim=c(0,1))
              },
              "Ordinal"={
                ncats1<-DV$nlevs
                b1<-(1:ncats1)
                l1=b1
                g<-g+scale_y_continuous(breaks=b1,labels=l1)+scale_x_continuous(breaks=NULL)+coord_cartesian(ylim = c(0,ncats1+1)-1,xlim=c(0,1))
              },
              "Interval"={
                g<-g+scale_y_continuous()+scale_x_continuous(breaks=NULL)+coord_cartesian(ylim = c(-1,1)*fullRange*DV$sd+DV$mu,xlim=c(0,1))
              }
      )
    }
    if (IV$type=="empty" && DV$type=="empty") {
      g<-g+scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)+coord_cartesian(xlim = c(0,1),ylim=c(0,1))
    }
  } else {
    
  switch (hypothesisType,
          "Interval Interval"={
            g<-plotParParPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
          },
          "Ordinal Interval"={
            g<-plotOrdParPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$nlevs+1), ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
          },
          "Categorical Interval"={
            g<-plotCatParPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
          },
          "Interval Ordinal"={
            g<-plotParOrdPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$nlevs+1))
          },
          "Ordinal Ordinal"={
            g<-plotOrdOrdPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$nlevs+1), ylim = c(0,DV$nlevs+1))
          },
          "Categorical Ordinal"={
            g<-plotCatOrdPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(0,DV$nlevs+1))
          },
          "Interval Categorical"={
            g<-plotParCatPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$ncats+1)-1)
          },
          "Ordinal Categorical"={
            g<-plotOrdCatPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$nlevs+1), ylim = c(0,DV$ncats+1)-1)
          },
          "Categorical Categorical"={
            g<-plotCatCatPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(0,DV$ncats+1)-1)
          }
  )
}
  g+labs(x=IV$name,y=DV$name)+theme
  
}
