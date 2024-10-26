
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

plotParParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  
  theta=seq(0,2*pi,length.out=braw.env$varNPoints)
  d<-acos(rho)
  x=cos(theta+d/2)
  y=cos(theta-d/2)
  y<-(y-x*rho)*(1+x/3*Heteroscedasticity)+x*rho
  pts=data.frame(x=x,y=y)
  radius<-qnorm(seq(0.55,0.95,0.1))*1.5
  for (ir in 1:length(radius)) {
    pts<-data.frame(x=x*radius[ir]*IV$sd+IV$mu,y=y*radius[ir]*DV$sd+DV$mu)
    g<-addG(g,dataPolygon(data=pts,fill = braw.env$plotColours$sampleC, colour=NA, alpha=alpha/(length(radius)-2)))
  }
  return(g)
}

plotOrdParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  ng<-IV$nlevs
  pp<-OrdProportions(IV)
  pp<-pp/max(pp)
  l=paste(1:ng,sep="")
  b<-1:ng
  
  pbreaks<-seq(0,1,1/(ng))
  ebreaks<-(pbreaks-0.5)*2
  
  y<-seq(-1,1,length.out=braw.env$varNPoints)*braw.env$fullRange
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
  
  #aes(x=y,y=x*DV$sd+DV$mu,group=ids,alpha=alpha*value),
  g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$sampleC,colour=NA))
  return(g)
}

plotCatParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  ncats<-IV$ncats
  l<-IV$cases
  if (sum(sapply(l,nchar))>12) {
    l<-sapply(l,shrinkString,ceil(12/length(l)))
  }
  pp<-CatProportions(IV)
  b<-(1:ncats)

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  y<-seq(-1,1,length.out=braw.env$varNPoints)*braw.env$fullRange
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

  x<-seq(-1,1,length.out=braw.env$varNPoints)*braw.env$fullRange
  np<-length(x)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ncats) {
    use<-mu_order[id]
    y<-mv2dens(x,rho,ebreaks[use],ebreaks[use+1])*pp[use]
    newPts<-plotRibbon(x,y,id)
    newPts$x<-newPts$x*sdv[id]+muv[id]
    pts<-rbind(pts,newPts)
  }
  pts$value<-pts$value/max(pts$value)
  pts1<-pts
  pts1$x<-pts$y*pts$value*0.9+pts$yoff
  pts1$y<-pts$x
  
  g<-addG(g,dataPolygon(data=pts1,fill=braw.env$plotColours$sampleC,colour=NA))
  return(g)
}

plotParOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  ng<-DV$nlevs
  pp<-OrdProportions(DV)
  pp<-pp/max(pp)
  l=paste(1:ng,sep="")
  b<-1:ng

  pbreaks<-seq(0,1,1/(ng))
  ebreaks<-(pbreaks-0.5)*2

  x<-seq(-1,1,length.out=braw.env$varNPoints)*braw.env$fullRange
  np<-length(x)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ng) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])*pp[id]
    pts<-rbind(pts,plotRibbon(x*IV$sd+IV$mu,y,id))
  }
  pts$value<-pts$value-min(pts$value)
  pts$value<-pts$value/max(pts$value)
  pts$y<-pts$y+pts$yoff

  g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$sampleC,colour=NA))
  return(g)
}

plotCatOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  ncats1<-IV$ncats
  pp1<-CatProportions(IV)
  l1=IV$cases
  if (sum(sapply(l1,nchar))>12) {
    l1<-sapply(l1,shrinkString,ceil(12/length(l1)))
  }
  b1<-(1:ncats1)
  
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

  g<-addG(g,dataPolygon(data=pts,fill = braw.env$plotColours$sampleC,colour=NA))
  return(g)
}

plotOrdCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
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
      newpts<-data.frame(y=y*s[ix,iy]*pp[ix]*pp2[iy]+b2[iy]+1, x=x+b1[ix], value=rep(s[ix,iy]*pp[ix]*pp2[iy],4),ids=idc)
      pts<-rbind(pts,newpts)
    }
  }
  pts$value<-pts$value/max(pts$value)
  
  g<-addG(g,dataPolygon(data=pts,fill = braw.env$plotColours$sampleC,colour=NA))
  return(g)
}

plotParCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  ncats<-DV$ncats
  pp<-CatProportions(DV)
  l=DV$cases
  b<-(ncats:1)-1

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  x<-seq(-1,1,length.out=braw.env$varNPoints)*braw.env$fullRange
  np<-length(x)
  pts<-data.frame(x=c(),y=c(),yoff=c(),value=c(),ids=c())
  for (id in 1:ncats) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])*pp[id]
    pts<-rbind(pts,plotRibbon(x,y,ncats+1-id))
  }
  pts$value<-pts$value/max(pts$value)
  pts$y<-pts$y*pts$value*0.9+pts$yoff
  
  g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$sampleC,colour=NA))
  return(g)
}

plotCatCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
  ncats1<-IV$ncats
  pp1<-CatProportions(IV)
  b1<-1:ncats1
  l1=IV$cases
  
  ncats2<-DV$ncats
  pp2<-CatProportions(DV)
  b2<-ncats2:1
  l2=DV$cases
  
  division<-r2CatProportions(rho,ncats1,ncats2)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b1))/2*0.9
  y<-c(-1,1,1,-1)*min(diff(b2))/2*0.9
  
  pts=data.frame(x=x,y=y)
  for (ix in 1:ncats1) {
    for (iy in 1:ncats2) {
      pts<-data.frame(x=x*s[iy,ix]*pp1[ix]*pp2[iy]+b1[ix], y=y*s[iy,ix]*pp1[ix]*pp2[iy]+b2[iy])
      g<-addG(g,dataPolygon(data=pts,fill = braw.env$plotColours$sampleC,colour=NA,alpha=alpha))
    }
  }
  return(g)
}

plotOrdOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha,g){
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
  for (ix in 1:nlevs1) {
    for (iy in 1:nlevs2) {
      pts<-data.frame(x=x+b1[ix], y=y+b2[iy])
      g<-addG(g,dataPolygon(data=pts,fill = braw.env$plotColours$sampleC,colour=NA,alpha=alpha*pp1[ix]*pp2[iy]))
    }
  }
  return(g)
}

plotPopulation<-function(IV,DV,effect,alpha=1,g=NULL){

  rho<-effect$rIV
  if (is.na(rho)) {rho<-0}
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  heteroscedasticity<-effect$Heteroscedasticity[1]
  
  switch(IV$type,
         "Interval"=    {
           xlim <- c(-1,1)*braw.env$fullRange*IV$sd+IV$mu
           xtick<-seq(-braw.env$fullRange,braw.env$fullRange,1)*IV$sd+IV$mu
           xtick<-NULL
           xticklabel<-xtick
         },
         "Ordinal"=     {
           xlim <- c(0,IV$nlevs+1)
           xtick<-seq(1,IV$nlevs)
           xticklabel<-xtick
         },
         "Categorical"= {
           xlim <- c(0,IV$ncats+1)
           xtick<-seq(1,IV$ncats)
           xticklabel<-IV$cases
         }
  )
  switch(DV$type,
         "Interval"=    {
           ylim <- c(-1,1)*braw.env$fullRange*DV$sd+DV$mu
           ytick<-seq(-braw.env$fullRange,braw.env$fullRange,1)*DV$sd+DV$mu
           ytick<-NULL
           yticklabel<-ytick
         },
         "Ordinal"=     {
           ylim <- c(0,DV$nlevs+1)
           ytick<-seq(1,DV$nlevs)
           yticklabel<-ytick
         },
         "Categorical"= {
           ylim <- c(0,DV$ncats+1)
           ytick<-seq(1,DV$ncats)
           yticklabel<-rev(DV$cases)
         }
  )
  
  g<-startPlot(xlim,ylim,
               xticks=makeTicks(xtick,xticklabel),xlabel=makeLabel(IV$name),
               yticks=makeTicks(ytick,yticklabel),ylabel=makeLabel(DV$name),
               top=TRUE,g=g)
  # g<-addG(g,xAxisTicks(xtick,xticklabel),xAxisLabel(IV$name))
  # g<-addG(g,yAxisTicks(ytick,yticklabel),yAxisLabel(DV$name))

  switch (hypothesisType,
          "Interval Interval"={
            g<-plotParParPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Ordinal Interval"={
            g<-plotOrdParPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Categorical Interval"={
            g<-plotCatParPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Interval Ordinal"={
            g<-plotParOrdPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Ordinal Ordinal"={
            g<-plotOrdOrdPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Categorical Ordinal"={
            g<-plotCatOrdPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Interval Categorical"={
            g<-plotParCatPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Ordinal Categorical"={
            g<-plotOrdCatPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          },
          "Categorical Categorical"={
            g<-plotCatCatPopulation(IV,DV,rho,heteroscedasticity,alpha,g)
          }
  )
   
  g
  
}
