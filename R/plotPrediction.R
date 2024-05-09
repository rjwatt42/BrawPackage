getAxisPrediction<-function(hypothesis) {
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
            ylim = c(-0.1,1.1)
            yticks<-seq(0,1,0.2)
            ylabels=yticks
          }
  )
  
  g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  g<-startPlot(xlim,ylim,g=g)
  g<-g+xAxisTicks(xticks,xlabels)+xAxisLabel(bquote(bold(.(IV$name))))
  g<-g+yAxisTicks(yticks,ylabels)+yAxisLabel(bquote(bold(.(DV$name))))
  
  
}

plotParParPrediction<-function(g,IV,DV,rho,n,offset=1){
  if (offset==1) {
    col<- braw.env$plotColours$descriptionC
    xoff=0
  } else {
    off=offset-2
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  x<-seq(-braw.env$fullRange,braw.env$fullRange,length.out=braw.env$varNPoints)
  y<-x*rho
  se<-sqrt((1+x^2)/n)*qnorm(0.975)
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-y_lower*DV$sd+DV$mu
  yv_upper<-y_upper*DV$sd+DV$mu
  
  x<-x*IV$sd+IV$mu
  y<-y*DV$sd+DV$mu
  xv<-c(x,rev(x))
  
  pts2<-data.frame(x=x,y=y)
  pts1<-data.frame(x=xv,y=c(yv_lower,rev(yv_upper)))
  g<-g+dataPolygon(data=pts1,fill = col, colour=NA, alpha=0.5)
  if (offset==1) {
    g<-g+dataLine(data=pts2,colour=col,linewidth=2)
  } else {
    g<-g+dataLine(data=pts2,colour=col,linewidth=2)
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
    xoff=off*0.2
  }
  
  ncats<-IV$ncats
  b<-(1:ncats)
  xv<-b
  
  if (length(IV$vals)==0){
    d<-rho/sqrt(1-rho^2)/2*xv/(sd(xv)*sqrt(1-1/ncats))
    d<-d-mean(d)
    d<-d*DV$sd+DV$mu
    se<-rep(DV$sd*sqrt(1-rho^2)/sqrt(n/ncats),ncats)
  } else{
    x<-IV$vals
    y<-DV$vals
    d<-array(0,ncats)
    se<-array(0,ncats)
    for (i in 1:ncats){
      d[i]<-mean(y[x==IV$cases[i]])
      se[i]<-sd(y[x==IV$cases[i]])/sqrt(n/ncats)
    }
  }
  l<-IV$cases
  if (sum(sapply(l,nchar))>12) {
    l<-sapply(l,shrinkString,ceil(12/length(l)))
  }
  
  # se<-se*2
  mn_pts<-data.frame(x=b+xoff,y=d)
  if (within) {
    se1_pts<-data.frame(x=b+xoff,y=d-se/4)
    se2_pts<-data.frame(x=b+xoff,y=d+se/4)
    g<-g+dataLine(data=se1_pts,colour="white")
    g<-g+dataLine(data=se2_pts,colour="white")
  } else
    g<-g+dataLine(data=mn_pts)
  
  se_pts<-data.frame(x=b+xoff,ymin=d-se,ymax=d+se)
  g<-g+dataErrorBar(data=se_pts)
  if (colindex>1) {
    g<-g+dataPoint(data=mn_pts, shape=braw.env$plotShapes$data, colour = "black", fill=col, size = 7)
  }  else {
    g<-g+dataPoint(data=mn_pts,shape=braw.env$plotShapes$data, colour = "black", fill=col, size = 7)
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
  se<-sqrt((1+x^2)/n)*qnorm(0.975)
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-y_lower*(DV$iqr/2)+(DV$nlevs+1)/2
  yv_upper<-y_upper*(DV$iqr/2)+(DV$nlevs+1)/2
  
  xv<-x*IV$sd+IV$mu
  yv<-y*(DV$iqr/2)+(DV$nlevs+1)/2
  xv<-c(xv,rev(xv))
  yv<-c(yv,rev(yv))
  yboth<-c(yv_lower,rev(yv_upper))
  
  pts<-data.frame(x=xv,y=yboth)
  g<-g+dataPolygon(data=pts,fill = col, colour=NA, alpha=0.5)
  pts<-data.frame(x=xv,y=yv)
  g<-g+dataLine(data=pts,colour=col,linewidth=2)
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
    g<-g+dataLine(data=se1_pts,colour="white")
    g<-g+dataLine(data=se2_pts,colour="white")
  } else
    g<-g+dataLine(data=mn_pts)
  
  se_pts<-data.frame(x=b+xoff,ymin=d-se,ymax=d+se)
  g<-g+
    dataErrorBar(data=se_pts)+
    dataPoint(data=mn_pts,shape=braw.env$plotShapes$data, colour = "black", fill = col, size = 7)
  
  g
  
}

plotParCatPrediction<-function(g,IV,DV,rho,n,offset= 1){
  plotBars<-FALSE
  plotBaseline<-TRUE
  
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
    g<-g+
      dataPolygon(data=pts1,fill = col, colour=NA, alpha=0.5)+
      dataLine(data=pts2,colour=col,linewidth=2)
  }
  
  if (plotBaseline) {
    pts1<-data.frame(x=c(-1,1)*braw.env$fullRange*IV$sd+IV$mu,y=c(0,0))
    g<-g+dataLine(data=pts1,colour="black")
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
    pp<-r2CatProportions(rho,ncats1,ncats2)
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
  g<-g+dataBar(data=pts,barwidth=barwidth,fill=full_c)

  pts1<-data.frame(x=c(0,ncats1+1),y=c(0,0))
  g<-g+dataLine(data=pts1,colour="black")
  g
  
}


plotPrediction<-function(IV,IV2,DV,effect,design,offset=1,g=NULL){
  if (is.null(g)) {
    g<-getAxisPrediction(hypothesis=list(IV=IV,DV=DV)) 
  }
  
  n<-design$sN
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
    if (is.na(rho)) {rho<-0}
    
    switch (hypothesisType,
            "Interval Interval"={
              g<-plotParParPrediction(g,IV,DV,rho,n,offset)
            },
            "Ordinal Interval"={
              g<-plotParParPrediction(g,IV,DV,rho,n,offset)
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
              g<-plotCatOrdPrediction(g,IV,DV,rho,n,offset,design$sIV1Use=="Within")
            },
            "Interval Categorical"={
              g<-plotParCatPrediction(g,IV,DV,rho,n,offset)
            },
            "Ordinal Categorical"={
              g<-plotParCatPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Categorical"={
              g<-plotCatCatPrediction(g,IV,DV,rho,n,offset)
            }
    )
    if (DV$type=="Categorical") {
      cols<-c(braw.env$plotColours$descriptionC,darken(braw.env$plotColours$descriptionC,0.25,0.75))
      g<-g+dataLegend(data.frame(names<-DV$cases,colours=cols),title=bquote(bold(.(DV$name))))
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
      names<-c(paste(IV2$name,"<median",sep=""), paste(IV2$name,">median",sep=""))
    }
    braw.env$plotDescriptionCols <- cols
    for (i in 1:length(rho)) {
      offset=2+(i-1)/(length(rho)-1)
      DV1<-DV
      DV1$mu<-DV1$mu+(i-mean(1:length(rho)))/(length(rho)-1)*2*effect$rIV2*DV$sd*qnorm(0.75)
      
      switch (hypothesisType,
              "Interval Interval"={
                g<-plotParParPrediction(g,IV,DV1,rho[i],n,offset)
              },
              "Categorical Interval"={
                g<-plotCatParPrediction(g,IV,DV1,rho[i],n,offset,design$sIV1Use=="Within")
              },
              "Interval Categorical"={
                g<-plotParCatPrediction(g,IV,DV1,rho[i],n,offset)
              },
              "Categorical Categorical"={
                g<-plotCatCatPrediction(g,IV,DV1,rho[i],n,offset)
              }
      )
    }
    g<-g+dataLegend(data.frame(names=names,colours=cols),title=IV2$name)
  }
  
  return(g)  
}

