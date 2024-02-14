plotPoints<-function(g,IV,DV,result,colindex=1,off=0){

  showRawData<-TRUE
  if (colindex==1)
          {  col<- plotcolours$descriptionC
          alphaPoints<-0.95
          xoff=0
          barwidth=1
          } else { 
          col <-plotDescriptionCols[[colindex-1]]
          alphaPoints<-0.95
          xoff=-0.25+off*0.2
          barwidth=0.5
          }

  x<-result$ivplot
  y<-result$dvplot
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  
  dotSize<-(plotTheme$axis.title$size)/3
  shrinkDots=1
  if (length(x)>100) {
    dotSize<-max(dotSize*sqrt(100/length(x)),2)
  }
  
  switch (hypothesisType,
          "Interval Interval"={
            pts<-data.frame(x=x,y=y);
            if (colindex>=2) {
              g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            }
            else
              g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Ordinal Interval"={
            pts<-data.frame(x=x,y=y);
            if (colindex>=2) {
              g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            }
            else
              g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Categorical Interval"={
            pp<-CatProportions(IV)
            pts<-data.frame(IV=x+xoff,DV=y);
            if (showRawData) {
              if (colindex>=2) 
                g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
              else
                g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=shapes$data, colour = col, fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
            }
          },
          
          "Ordinal Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (colindex>=2)
              g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            else
              g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=shapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Interval Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (colindex>=2)
              g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            else
              g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=shapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Categorical Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (showRawData) {
              if (colindex>=2)
                g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
              else
                g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=shapes$data, colour = "black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
            }
          },
          
          "Interval Categorical"={
            bin_breaks<-c(-Inf,seq(-1,1,length.out=varNPoints-1)*fullRange*sd(result$iv)+mean(result$iv),Inf)
            dens2<-hist(result$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            for (i2 in 1:DV$ncats){
              xv<-c()
              yv<-c()
              dens1<-hist(result$iv[result$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
              densities<-dens1$counts/dens2$counts
              for (i in 1:(length(dens1$counts)-1)){
                y<-dens1$counts[i]
                if (y>0){
                  xv<-c(xv,rep(bins[i],y)+runif(y,min=-0.08,max=0.08))
                  yv<-c(yv,runif(y,min=0.05,max=0.9)*densities[i])
                }
              }
              xoff<-(i2-1)/(DV$ncats-1)-(DV$ncats-1)/2
              full_x<-c(full_x,xv+xoff/4)
              full_y<-c(full_y,yv)
              full_f<-c(full_f,rep(i2,length(xv)))
              if (colindex==1) {
                full_c<-c(full_c,rep(CatCatcols[i2],length(xv)))
              }
            }
            pts<-data.frame(x=full_x,y=full_y,fill=full_f)
            if (showRawData) {
              if (colindex>=2) {
                # g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, size =dotSize, alpha=0.95, colour="black")
                g<-g+geom_point(data=pts,aes(x=full_x,y=full_y),shape=shapes$data, size =dotSize, alpha=alphaPoints, colour="black",fill="white")
              } else {
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=full_x,y=full_y,fill=factor(full_f)),shape=shapes$data, size =dotSize*shrinkDots, alpha=alphaPoints)
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black",fill=full_c)
                }
              }
            }
          },
          
          
          "Ordinal Categorical"={
            bin_breaks<-c(-Inf,seq(-1,1,length.out=varNPoints-1)*fullRange*sd(result$iv)+mean(result$iv),Inf)
            dens2<-hist(result$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            for (i2 in 1:DV$ncats){
              xv<-c()
              yv<-c()
              dens1<-hist(result$iv[result$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
              densities<-dens1$counts/dens2$counts
              for (i in 1:(length(dens1$counts)-1)){
                y<-dens1$counts[i]
                if (y>0){
                  xv<-c(xv,rep(bins[i],y)+runif(y,min=-0.08,max=0.08))
                  yv<-c(yv,runif(y,min=0.05,max=0.9)*densities[i])
                }
              }
              xoff<-(i2-1)/(DV$ncats-1)-(DV$ncats-1)/2
              full_x<-c(full_x,xv+xoff/4)
              full_y<-c(full_y,yv)
              full_f<-c(full_f,rep(i2,length(xv)))
              if (colindex==1) {
                full_c<-c(full_c,rep(CatCatcols[i2],length(xv)))
              }
            }
            pts<-data.frame(x=full_x,y=full_y,fill=full_f)
            if (showRawData) {
              if (colindex>=2) {
                # g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, size =dotSize, alpha=0.95, colour="black")
                g<-g+geom_point(data=pts,aes(x=full_x,y=full_y),shape=shapes$data, size =dotSize, alpha=alphaPoints, colour="black",fill="white")
              } else {
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=full_x,y=full_y,fill=factor(full_f)),shape=shapes$data, size =dotSize*shrinkDots, alpha=alphaPoints)
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black",fill=full_c)
                }
              }
            }
          },
          
          "Categorical Categorical"={
            b<-(1:IV$ncats)-1
            xv<-as.numeric(result$iv)
            yv<-as.numeric(result$dv)
            
            pp<-matrix(NA,DV$ncats,IV$ncats)
            for (i1 in 1:IV$ncats) {
              for (i2 in 1:DV$ncats) {
                pp[i2,i1]<-sum(yv[xv==i1]==i2)/length(xv)
              }
            }
            
            for (i2 in 1:DV$ncats) {
              x<-b[xv[yv==i2]]+(i2-(DV$ncats+1)/2)/(DV$ncats+1)+runif(length(xv[yv==i2]),min=-0.1,max=0.1)
              y<-pp[i2,xv[yv==i2]]*runif(length(xv[yv==i2]),min=0.05,max=0.9)
            
            pts<-data.frame(x=x+xoff,y=y)
            if (showRawData) {
              if (colindex>=2)
                g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=shapes$data, size =dotSize, alpha=alphaPoints, colour="black")
              else
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=x,y=y,fill=factor(i2)),shape=shapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black")
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, size =dotSize*shrinkDots, colour="black", fill=CatCatcols[i2], alpha=alphaPoints)
                }
            }
            }
          }
  )
 g  
}

plotCatInterDescription<-function(result,g=NULL){
  plotDescriptionCols <<- c()
  cols<-c()
  for (i in 1:result$IV2$ncats){
    off<-(i-1)/(result$IV2$ncats-1)
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
  }
  names(cols)<-result$IV2$cases
  cols<-as.list(cols)
  plotDescriptionCols <<- cols
  
  Ivals<-result$iv
  Dvals<-result$dv
  rho<-result$rIV+seq(-1,1,length.out=result$IV2$ncats)*result$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
    }
  for (i in 1:result$IV2$ncats){
    use<-result$iv2==result$IV2$cases[i]
    
    result1<-result
    result1$iv<-result$iv[use]
    result1$dv<-result$dv[use]
    result1$ivplot<-result$ivplot[use]
    result1$dvplot<-result$dvplot[use]
    result1$rIV<-rho[i]
    
    result1$IV$vals<-Ivals[use]
    result1$DV$vals<-Dvals[use] 
    if (is.numeric(Ivals)) {
    result1$IV$mu<-mean(Ivals[use],na.rm=TRUE)
    result1$IV$sd<-sd(Ivals[use],na.rm=TRUE)
    }
    if (is.numeric(Dvals)) {
    result1$DV$mu<-mean(Dvals[use],na.rm=TRUE)
    result1$DV$sd<-sd(Dvals[use],na.rm=TRUE)
    }
    g<-plotPoints(g,result$IV,result$DV,result1,i+1,(i-1)/(result$IV2$ncats-1))
    g<-plotPrediction(result1$IV,NULL,result1$DV,result1,result$design,2+(i-1)/(IV2$ncats-1),g,theme=plotTheme)
  }
  
  g<-g+scale_fill_manual(name=result$IV2$name,values=plotDescriptionCols)
  g
}

plotParInterDescription<-function(result,g=NULL){
  col<-c( plotcolours$descriptionC1, plotcolours$descriptionC2)
  names(col)<-c(paste(result$IV2$name,"<median",sep=""), paste(result$IV2$name,">median",sep=""))
  col<-as.list(col)
  plotDescriptionCols <<- col
  
  Ivals<-result$IV$vals
  Dvals<-result$DV$vals
  rho<-result$rIV+seq(-1,1,length.out=2)*result$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
  }
  for (i in 1:2){
    switch (i,
            use<-result$iv2<median(result$iv2),
            use<-result$iv2>=median(result$iv2)
    )
    result1<-result
    result1$iv<-result$iv[use]
    result1$dv<-result$dv[use]
    result1$ivplot<-result$ivplot[use]
    result1$dvplot<-result$dvplot[use]
    result1$rIV<-rho[i]
    
    result1$IV$vals<-Ivals[use]
    result1$DV$vals<-Dvals[use]
    result1$DV$mu<-mean(result$dv[use],na.rm=TRUE)
    g<-plotPoints(g,result1$IV,result1$DV,result1,i+1,(i-1)/(2-1)*0.25)
    g<-plotPrediction(result1$IV,NULL,result1$DV,result1,result$design,i+1,g,theme=plotTheme)
  }
  
  g<-g+scale_fill_manual(name=result$IV2$name,values=plotDescriptionCols)
  g
}

plotParDescription<-function(result,g) {
  
  g<-plotPoints(g,result$IV,result$DV,result,1)
  g<-plotPrediction(result$IV,result$IV2,result$DV,result,result$design,1,g,theme=plotTheme)
  g
}

plotCatDescription<-function(result,g) {

  g<-plotPrediction(result$IV,result$IV2,result$DV,result,result$design,1,g,theme=plotTheme)
  g<-plotPoints(g,result$IV,result$DV,result,1)
  
  if (!doLegendBars && doLegendPoints) {
    g<-g+scale_fill_manual(name=result$DV$name,values=CatCatcols,labels=result$DV$cases)
  }
  
  g
}

showDescription<-function(result=analyseSample()) {

  g<-ggplot()
  if (is.null(result$IV2)){
    switch (result$DV$type,
            "Interval"=g<-plotParDescription(result,g),
            "Ordinal"=g<-plotParDescription(result,g),
            "Categorical"=g<-plotCatDescription(result,g)
    )
  } else{
    switch (result$IV2$type,
            "Interval"=g<-plotParInterDescription(result,g),
            "Ordinal"=g<-plotParInterDescription(result,g),
            "Categorical"=g<-plotCatInterDescription(result,g)
    )
  }
  g
}
