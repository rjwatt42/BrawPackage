getAxisPrediction<-function(hypothesis,g=NULL) {
  IV<-hypothesis$IV
  DV<-hypothesis$DV
  switch (IV$type,
          "Interval"={
            xlim = c(-1,1)*braw.env$fullRange*IV$sd+IV$mu
            xticks=seq(-2,2)*IV$sd+IV$mu
            xticks<-NULL
            xlabels=xticks
          },
          "Ordinal"={
            xlim = c(0,IV$nlevs+1)
            xticks<-1:IV$nlevs
            xlabels=xticks
          },
          "Categorical"={
            xlim = c(0,IV$ncats+1)
            xticks<-1:IV$ncats
            xlabels=IV$cases
          }
  )
  switch (DV$type,
          "Interval"={
            ylim = c(-1,1)*braw.env$fullRange*DV$sd+DV$mu
            yticks=seq(-2,2)*DV$sd+DV$mu
            yticks<-NULL
            ylabels=yticks
          },
          "Ordinal"={
            ylim = c(0,DV$nlevs+1)
            yticks<-1:DV$nlevs
            ylabels=yticks
          },
          "Categorical"={
            ylim = c(-0.5,1.5)
            yticks<-seq(0,1,0.2)
            ylabels=c(DV$cases[1],rep(" ",4),DV$cases[2])
          }
  )
  
  g<-startPlot(xlim,ylim,
               xticks=makeTicks(xticks,xlabels),xlabel=makeLabel(IV$name),
               yticks=makeTicks(yticks,ylabels),ylabel=makeLabel(DV$name),
               top=FALSE,g=g)
  # g<-addG(g,xAxisTicks(xticks,xlabels),xAxisLabel(IV$name))
  # g<-addG(g,yAxisTicks(yticks,ylabels),yAxisLabel(DV$name))
  
  
}

plotParParPrediction<-function(g,IV,DV,rho,n,offset=1,range=NULL){
  fullrange<-c(-1,1)*braw.env$fullRange
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
  } else {
    off=offset-2
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  x<-seq(fullrange[1],fullrange[2],length.out=braw.env$varNPoints)
  y<-x*rho
  for (ni in 1:length(n)) {
    if (!is.na(n[ni])) {
    se<-sqrt((1+x^2)/n[ni])*qnorm(0.975)
    y_lower<-y-se
    y_upper<-y+se
    yv_lower<-y_lower*DV$sd+DV$mu
    yv_upper<-y_upper*DV$sd+DV$mu
    
    xd<-x*IV$sd+IV$mu
    yd<-y*DV$sd+DV$mu
    
    if (!is.null(range)) {
      use<-(xd>=range[1]) & (xd<=range[2])
      xd<-xd[use]
      yd<-yd[use]
      yv_lower<-yv_lower[use]
      yv_upper<-yv_upper[use]
    }
    xv<-c(xd,rev(xd))
    
    pts1<-data.frame(x=xv,y=c(yv_lower,rev(yv_upper)))
    pts1a<-data.frame(x=xd,y=yv_lower)
    pts1b<-data.frame(x=xd,y=yv_upper)
    gain<-0.5
    if (ni==1) {
    g<-addG(g,dataPolygon(data=pts1,fill = col, colour=NA, alpha=0.5))
    g<-addG(g,dataLine(data=pts1a,colour=col,linewidth=1*gain))
    g<-addG(g,dataLine(data=pts1b,colour=col,linewidth=1*gain))
    } 
    if (ni>1 && ni<4) {
      g<-addG(g,dataLine(data=pts1a,colour='black',linewidth=0.5*gain,linetype='dotted'))
      g<-addG(g,dataLine(data=pts1b,colour='black',linewidth=0.5*gain,linetype='dotted'))
    }
    if (ni==4) {
      g<-addG(g,dataLine(data=pts1a,colour='white',linewidth=0.5*gain,linetype='dotted'))
      g<-addG(g,dataLine(data=pts1b,colour='white',linewidth=0.5*gain,linetype='dotted'))
    }
    }
  }
  pts2<-data.frame(x=xd,y=yd)
  if (offset==1) {
    g<-addG(g,dataLine(data=pts2,colour=col,linewidth=2*gain))
  } else {
    g<-addG(g,dataLine(data=pts2,colour=col,linewidth=2*gain))
  }
  g
  
}

plotCatParPrediction<-function(g,IV,DV,rho,n,offset=1, within=FALSE){
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
    colindex<-1
  } else {
    colindex=offset
    maxoff<-IV$ncats
    col <-braw.env$plotDescriptionCols[[colindex-1]]
    # col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    # col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    off<-(colindex-2)/(maxoff-1)-0.5
    xoff=off*braw.env$CatPlotOffset
  }
  
  ncats<-IV$ncats
  b<-(1:ncats)
  xv<-b
  
  if (length(IV$vals)==0){
    # d<-xv*rho/sqrt(1-rho^2)/2/(sd(xv)*sqrt(1-1/ncats))
    d<-xv*rho/(sd(xv)*sqrt(1-1/ncats))
    d<-d-mean(d)
    d<-d*DV$sd+DV$mu
    se<-rep(DV$sd*sqrt(1-rho^2)/sqrt(n[1]/ncats),ncats)
  } else{
    x<-IV$vals
    y<-DV$vals
    d<-array(0,ncats)
    se<-array(0,ncats)
    for (i in 1:ncats){
      d[i]<-mean(y[x==IV$cases[i]])
      se[i]<-sd(y[x==IV$cases[i]])/sqrt(n[1]/ncats)
    }
  }
  # l<-IV$cases
  # if (sum(sapply(l,nchar))>12) {
  #   l<-sapply(l,shrinkString,ceil(12/length(l)))
  # }
  # 
  # se<-se*2
  mn_pts<-data.frame(x=b+xoff,y=d)
  if (within) {
    se1_pts<-data.frame(x=b+xoff,y=d-se/2)
    se2_pts<-data.frame(x=b+xoff,y=d+se/2)
    g<-addG(g,dataLine(data=se1_pts,colour="white",linewidth=2, linetype = "dotted"))
    g<-addG(g,dataLine(data=se2_pts,colour="white",linewidth=2, linetype = "dotted"))
  } else
    g<-addG(g,dataLine(data=mn_pts))
  
  for (ni in rev(1:length(n))) {
    if (length(IV$vals)==0){
      se<-rep(DV$sd*sqrt(1-rho^2)/sqrt(n[ni]/ncats),ncats)
    } else{
      se<-array(0,ncats)
    for (i in 1:ncats){
      se[i]<-sd(y[x==IV$cases[i]])/sqrt(n[ni]/ncats)*braw.env$errorBarGain
    }
    }
    se_pts<-data.frame(x=b+xoff,ymin=d-se,ymax=d+se)
    if (ni==1) {col1<-col;lw<-2}
    if (ni>1 && ni<4) {col1<-"#000000";lw<-1}
    if (ni==4) {col1<-"white";lw=1}
    g<-addG(g,dataErrorBar(data=se_pts,colour="#000000",linewidth=0.25))
  }
  if (colindex>1) {
    g<-addG(g,dataPoint(data=mn_pts, shape=braw.env$plotShapes$data, colour = "#000000", fill=col, size = braw.env$dotSize))
  }  else {
    g<-addG(g,dataPoint(data=mn_pts,shape=braw.env$plotShapes$data, colour = "#000000", fill=col, size = braw.env$dotSize))
  }
  g
  
}


plotParOrdPrediction<-function(g,IV,DV,rho,n,offset=1){
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
  } else   {
    off=offset-2
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  x<-seq(-braw.env$fullRange,braw.env$fullRange,length.out=braw.env$varNPoints)
  y<-x*rho
  for (ni in 1:length(n)) {
  se<-sqrt((1+x^2)/n[ni])*qnorm(0.975)
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-y_lower*(DV$iqr/2)+(DV$nlevs+1)/2
  yv_upper<-y_upper*(DV$iqr/2)+(DV$nlevs+1)/2
  
  xv<-x*IV$sd+IV$mu
  yv<-y*(DV$iqr/2)+(DV$nlevs+1)/2
  xv<-c(xv,rev(xv))
  yv<-c(yv,rev(yv))
  yboth<-c(yv_lower,rev(yv_upper))
  
  if (ni==1) {
    pts<-data.frame(x=xv,y=yboth)
    g<-addG(g,dataPolygon(data=pts,fill = col, colour=NA, alpha=0.5))
    pts<-data.frame(x=xv,y=yv)
    g<-addG(g,dataLine(data=pts,colour=col,linewidth=2))
  } else {
    pts<-data.frame(x=xv,y=yv)
    if (ni<4) g<-addG(g,dataLine(data=pts,colour="#000000",linewidth=2))
    else g<-addG(g,dataLine(data=pts,colour="white",linewidth=2))
  }
  }
  return(g)
}

plotCatOrdPrediction<-function(g,IV,DV,rho,n,offset= 1,within=FALSE){
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
  } else {
    off=offset-2
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  nlevs<-DV$nlevs
  ncats<-IV$ncats
  b<-(1:ncats)
  xv<-b
  
  if (length(IV$vals)==0){
    d<-rho/sqrt(1-rho^2)/2*xv/(sd(xv)*sqrt(1-1/ncats))
    d<-d+(nlevs+1)/2
  } else{
    x<-IV$vals
    y<-DV$vals
    d<-array(0,ncats)
    for (i in 1:ncats){
      d[i]<-mean(y[x==IV$cases[i]])
    }
  }
  l<-IV$cases
  
  se<-rep(DV$sd^2*sqrt(1-rho^2)/sqrt(n/ncats),ncats)
  se<-se*2
  mn_pts<-data.frame(x=b+xoff,y=d)
  if (within) {
    se1_pts<-data.frame(x=b+xoff,y=d-se/4)
    se2_pts<-data.frame(x=b+xoff,y=d+se/4)
    g<-addG(g,dataLine(data=se1_pts,colour="white"))
    g<-addG(g,dataLine(data=se2_pts,colour="white"))
  } else
    g<-addG(g,dataLine(data=mn_pts))
  
  se_pts<-data.frame(x=b+xoff,ymin=d-se,ymax=d+se)
  g<-addG(g,
    dataErrorBar(data=se_pts),
    dataPoint(data=mn_pts,shape=braw.env$plotShapes$data, colour = "#000000", fill = col, size = 7)
  )
  g
  
}

plotParCatPrediction<-function(g,IV,DV,rho,n,offset= 1){
  plotBars<-FALSE
  plotBaseline<-FALSE
  
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
    barwidth=2/(DV$ncats+1)
  } else {
    off=offset-2
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
    barwidth=0.25
  }
  
  ncats<-DV$ncat
  l<-DV$cases
  b<-(1:ncats)-1
  
  x<-seq(-braw.env$fullRange,braw.env$fullRange,length.out=braw.env$varNPoints)
  yv<-get_logistic_r(rho,ncats,x)
  x1<-x*IV$sd+IV$mu
  xv<-c(x1,rev(x1))
  
  if (braw.env$onesided) i2<-2
  else i2<-1
  for (i in i2:ncats) {
    y<-yv[,i]
    se=sqrt((1+x^2)/n)*qnorm(0.975)
    
    y_lower<-pnorm(qnorm(y)-se)
    y_upper<-pnorm(qnorm(y)+se)
    
    pts2<-data.frame(x=x1,y=y)
    pts1<-data.frame(x=xv,y=c(y_lower,rev(y_upper)))
    g<-addG(g,
      dataPolygon(data=pts1,fill = col, colour=NA, alpha=0.5),
      dataLine(data=pts2,colour=col,linewidth=2)
    )
  }
  
  if (plotBaseline) {
    pts1<-data.frame(x=c(-1,1)*braw.env$fullRange*IV$sd+IV$mu,y=c(0,0))
    g<-addG(g,dataLine(data=pts1,colour="#000000"))
  }
  
  g
  
}

plotCatCatPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
    barwidth=0.5
  } else {
    off=offset-2
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
    barwidth=0.25
  }
  
  ncats1<-IV$ncats
  ncats2<-DV$ncats
  l1=IV$cases
  b1<-(1:ncats1)
  
  if (length(IV$vals)>0)  {
    pp<-matrix(NA,ncats2,ncats1)
    yv<-as.numeric(DV$vals)
    for (i1 in 1:ncats1) {
      for (i2 in 1:ncats2) {
        pp[i2,i1]<-sum(yv[IV$vals==IV$cases[i1]]==i2)/length(IV$vals)
      }
    }
  } else {
    pp<-r2CatProportions(rho,ncats1,ncats2,IV$proportions,DV$proportions)
  }
  
  full_x<-c()
  full_y<-c()
  full_f<-c()
  full_c<-c()
  for (i2 in ncats2:1){
    full_x<-c(full_x,b1+xoff+((i2-1)/(ncats2-1)-0.5)*barwidth/2)
    full_y<-c(full_y,pp[i2,])
    full_f<-c(full_f,rep(i2,length(pp[i2,])))
    if (i2==1) col<-darken(col,0.25,off=0.75)
    full_c<-c(full_c,rep(col,length(pp[i2,])))
  }

  pts<-data.frame(x=full_x,y=full_y,fill=factor(full_f))
  g<-addG(g,dataBar(data=pts,barwidth=barwidth,fill=full_c))

  pts1<-data.frame(x=c(0,ncats1+1),y=c(0,0))
  g<-addG(g,dataLine(data=pts1,colour="#000000"))
  g
  
}


plotPrediction<-function(IV=braw.def$hypothesis$IV,IV2=braw.def$hypothesis$IV2,DV=braw.def$hypothesis$DV,
                         effect=braw.def$hypothesis$effect,design=braw.def$design,
                         offset=1,range=NULL,correction=FALSE,
                         g=NULL){
  if (is.null(g)) {
    g<-getAxisPrediction(hypothesis=list(IV=IV,DV=DV),g=g) 
  }
  
  n<-design$sN
  if (correction && design$sMethod$type=="Convenience") n<-c(n,NA,NA,n/3.18)
  if (correction && design$sMethod$type=="Snowball") n<-c(n,NA,n/2.84)
  if (correction && design$sMethod$type=="Cluster") n<-c(n,n/2.65)
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  
  if (is.null(IV2)){
    if (DV$type=="Categorical" && (is.null(braw.env$CatCatCols) || length(braw.env$CatCatCols)<DV$ncats)) {
      braw.env$CatCatCols <- c()
      cols<-c()
      for (i2 in 1:DV$ncats) {
        off<-(i2-1)/(DV$ncats-1)
        col<-col2rgb(braw.env$plotColours$descriptionC)*off+col2rgb(braw.env$plotColours$descriptionC2)*(1-off)
        col<- rgb(col[1]/255,col[2]/255,col[3]/255)
        cols<-c(cols,col)
      }
      braw.env$CatCatCols<-cols
    }
    
    rho<-effect$rIV
    if (is.null(rho) || is.na(rho)) {rho<-0}
    
    switch (hypothesisType,
            "Interval Interval"={
              g<-plotParParPrediction(g,IV,DV,rho,n,offset=offset,range=range)
            },
            "Ordinal Interval"={
              g<-plotParParPrediction(g,IV,DV,rho,n,offset=offset,range=range)
            },
            "Categorical Interval"={
              g<-plotCatParPrediction(g,IV,DV,rho,n,offset,design$sIV1Use=="Within")
            },
            "Interval Ordinal"={
              g<-plotParOrdPrediction(g,IV,DV,rho,n,offset)
            },
            "Ordinal Ordinal"={
              g<-plotParOrdPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Ordinal"={
              g<-plotCatOrdPrediction(g,IV,DV,rho,n[1],offset,design$sIV1Use=="Within")
            },
            "Interval Categorical"={
              g<-plotParCatPrediction(g,IV,DV,rho,n[1],offset)
            },
            "Ordinal Categorical"={
              g<-plotParCatPrediction(g,IV,DV,rho,n[1],offset)
            },
            "Categorical Categorical"={
              g<-plotCatCatPrediction(g,IV,DV,rho,n[1],offset)
            }
    )
    if (DV$type=="Categorical") {
      cols<-c(braw.env$plotColours$descriptionC,darken(braw.env$plotColours$descriptionC,0.25,0.75))
      g<-addG(g,dataLegend(data.frame(names<-DV$cases,colours=cols),title=DV$name))
    }
  } else {
    # more than 1 IV
    roff=0.82
    # deal with interaction
    switch (IV2$type,
            "Interval"= rho<-effect$rIV+c(-1,1)*effect$rIVIV2DV,
            "Ordinal"= rho<-effect$rIV+c(-1,1)*effect$rIVIV2DV,
            "Categorical"= rho<-effect$rIV+seq(-1,1,length.out=IV2$ncats)*effect$rIVIV2DV
    )
    rho[is.na(rho)] <- 0
    rho[rho > 1] <- 1
    rho[rho < -1] <- -1
    
    cols<-c()
    if (IV2$type=="Categorical") {
      for (i in 1:IV2$ncats){
        off<-(i-1)/(IV2$ncats-1)
        col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
        cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
      }
      names<-IV2$cases
    } else {
      cols<-c( braw.env$plotColours$descriptionC1, braw.env$plotColours$descriptionC2)
      names<-c(paste(IV2$name," < median",sep=""), paste(IV2$name," > median",sep=""))
    }
    braw.env$plotDescriptionCols <- cols
    for (i in 1:length(rho)) {
      offset=2+(i-1)/(length(rho)-1)
      DV1<-DV
      DV1$mu<-DV1$mu+(i-mean(1:length(rho)))/(length(rho)-1)*2*effect$rIV2*DV$sd*qnorm(0.75)
      
      switch (hypothesisType,
              "Interval Interval"={
                g<-plotParParPrediction(g,IV,DV1,rho[i],n,offset=offset,range)
              },
              "Categorical Interval"={
                g<-plotCatParPrediction(g,IV,DV1,rho[i],n,offset,design$sIV1Use=="Within")
              },
              "Interval Categorical"={
                g<-plotParCatPrediction(g,IV,DV1,rho[i],n[1],offset)
              },
              "Categorical Categorical"={
                g<-plotCatCatPrediction(g,IV,DV1,rho[i],n[1],offset)
              }
      )
    }
    g<-addG(g,dataLegend(data.frame(names=names,colours=cols),title=IV2$name))
  }
  
  return(g)  
}

