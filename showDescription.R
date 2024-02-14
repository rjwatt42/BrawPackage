plotPoints<-function(g,IV,DV,analysis,colindex=1,off=0){

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

  x<-analysis$ivplot
  y<-analysis$dvplot
  
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
            bin_breaks<-c(-Inf,seq(-1,1,length.out=varNPoints-1)*fullRange*sd(analysis$iv)+mean(analysis$iv),Inf)
            dens2<-hist(analysis$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            for (i2 in 1:DV$ncats){
              xv<-c()
              yv<-c()
              dens1<-hist(analysis$iv[analysis$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
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
            bin_breaks<-c(-Inf,seq(-1,1,length.out=varNPoints-1)*fullRange*sd(analysis$iv)+mean(analysis$iv),Inf)
            dens2<-hist(analysis$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            for (i2 in 1:DV$ncats){
              xv<-c()
              yv<-c()
              dens1<-hist(analysis$iv[analysis$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
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
            xv<-as.numeric(analysis$iv)
            yv<-as.numeric(analysis$dv)
            
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

plotCatInterDescription<-function(analysis,g=NULL){
  plotDescriptionCols <<- c()
  cols<-c()
  for (i in 1:analysis$IV2$ncats){
    off<-(i-1)/(analysis$IV2$ncats-1)
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
  }
  names(cols)<-analysis$IV2$cases
  cols<-as.list(cols)
  plotDescriptionCols <<- cols
  
  Ivals<-analysis$iv
  Dvals<-analysis$dv
  rho<-analysis$rIV+seq(-1,1,length.out=analysis$IV2$ncats)*analysis$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
    }
  for (i in 1:analysis$IV2$ncats){
    use<-analysis$iv2==analysis$IV2$cases[i]
    
    analysis1<-analysis
    analysis1$iv<-analysis$iv[use]
    analysis1$dv<-analysis$dv[use]
    analysis1$ivplot<-analysis$ivplot[use]
    analysis1$dvplot<-analysis$dvplot[use]
    analysis1$rIV<-rho[i]
    
    analysis1$IV$vals<-Ivals[use]
    analysis1$DV$vals<-Dvals[use] 
    if (is.numeric(Ivals)) {
    analysis1$IV$mu<-mean(Ivals[use],na.rm=TRUE)
    analysis1$IV$sd<-sd(Ivals[use],na.rm=TRUE)
    }
    if (is.numeric(Dvals)) {
    analysis1$DV$mu<-mean(Dvals[use],na.rm=TRUE)
    analysis1$DV$sd<-sd(Dvals[use],na.rm=TRUE)
    }
    g<-plotPoints(g,analysis$IV,analysis$DV,analysis1,i+1,(i-1)/(analysis$IV2$ncats-1))
    g<-plotPrediction(analysis1$IV,NULL,analysis1$DV,analysis1,analysis$design,2+(i-1)/(IV2$ncats-1),g,theme=plotTheme)
  }
  
  g<-g+scale_fill_manual(name=analysis$IV2$name,values=plotDescriptionCols)
  g
}

plotParInterDescription<-function(analysis,g=NULL){
  col<-c( plotcolours$descriptionC1, plotcolours$descriptionC2)
  names(col)<-c(paste(analysis$IV2$name,"<median",sep=""), paste(analysis$IV2$name,">median",sep=""))
  col<-as.list(col)
  plotDescriptionCols <<- col
  
  Ivals<-analysis$IV$vals
  Dvals<-analysis$DV$vals
  rho<-analysis$rIV+seq(-1,1,length.out=2)*analysis$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
  }
  for (i in 1:2){
    switch (i,
            use<-analysis$iv2<median(analysis$iv2),
            use<-analysis$iv2>=median(analysis$iv2)
    )
    analysis1<-analysis
    analysis1$iv<-analysis$iv[use]
    analysis1$dv<-analysis$dv[use]
    analysis1$ivplot<-analysis$ivplot[use]
    analysis1$dvplot<-analysis$dvplot[use]
    analysis1$rIV<-rho[i]
    
    analysis1$IV$vals<-Ivals[use]
    analysis1$DV$vals<-Dvals[use]
    analysis1$DV$mu<-mean(analysis$dv[use],na.rm=TRUE)
    g<-plotPoints(g,analysis1$IV,analysis1$DV,analysis1,i+1,(i-1)/(2-1)*0.25)
    g<-plotPrediction(analysis1$IV,NULL,analysis1$DV,analysis1,analysis$design,i+1,g,theme=plotTheme)
  }
  
  g<-g+scale_fill_manual(name=analysis$IV2$name,values=plotDescriptionCols)
  g
}

plotParDescription<-function(analysis,g) {
  
  g<-plotPoints(g,analysis$IV,analysis$DV,analysis,1)
  g<-plotPrediction(analysis$IV,analysis$IV2,analysis$DV,analysis,analysis$design,1,g,theme=plotTheme)
  g
}

plotCatDescription<-function(analysis,g) {

  g<-plotPrediction(analysis$IV,analysis$IV2,analysis$DV,analysis,analysis$design,1,g,theme=plotTheme)
  g<-plotPoints(g,analysis$IV,analysis$DV,analysis,1)
  
  if (!doLegendBars && doLegendPoints) {
    g<-g+scale_fill_manual(name=analysis$DV$name,values=CatCatcols,labels=analysis$DV$cases)
  }
  
  g
}

showDescription<-function(analysis=makeAnalysis()) {

  g<-ggplot()
  if (is.null(analysis$IV2)){
    switch (analysis$DV$type,
            "Interval"=g<-plotParDescription(analysis,g),
            "Ordinal"=g<-plotParDescription(analysis,g),
            "Categorical"=g<-plotCatDescription(analysis,g)
    )
  } else{
    switch (analysis$IV2$type,
            "Interval"=g<-plotParInterDescription(analysis,g),
            "Ordinal"=g<-plotParInterDescription(analysis,g),
            "Categorical"=g<-plotCatInterDescription(analysis,g)
    )
  }
  g
}
