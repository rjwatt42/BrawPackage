plotPoints<-function(g,IV,DV,analysis,colindex=1,maxoff=1){

  if (allScatter) showRawData<-TRUE
  else showRawData<-FALSE
  
  if (colindex==1)
          {  col<- BrawOpts$plotColours$descriptionC
          alphaPoints<-0.95
          xoff=0
          barwidth=1
          } else { 
          col <-plotDescriptionCols[[colindex-1]]
          alphaPoints<-0.95
          off<-(colindex-2)/(maxoff-1)-0.5
          xoff=off*0.2
          barwidth=0.5
          }

  x<-analysis$ivplot
  y<-analysis$dvplot
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")

  dotSize<-(BrawOpts$plotTheme$axis.title$size)/3
  shrinkDots=1
  if (length(x)>100) {
    dotSize<-max(dotSize*sqrt(100/length(x)),2)
  }
  dotSize<-dotSize/2
  
  switch (hypothesisType,
          "Interval Interval"={
            pts<-data.frame(x=x,y=y)
            # if (colindex>=2) {
            #   g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            # }
            # else
              g<-g+geom_point(data=pts,aes(x=x,y=y),shape=BrawOpts$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Ordinal Interval"={
            pts<-data.frame(x=x,y=y)
            # if (colindex>=2) {
            #   g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            # }
            # else
              g<-g+geom_point(data=pts,aes(x=x,y=y),shape=BrawOpts$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Categorical Interval"={
            pp<-CatProportions(IV)
            pts<-data.frame(IV=x+xoff,DV=y);
            if (showRawData) {
              # if (colindex>=2) 
              #   g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
              # else
                g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=BrawOpts$plotShapes$data, colour = "black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
            }
          },
          
          "Ordinal Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            # if (colindex>=2)
            #   g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            # else
              g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=BrawOpts$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Interval Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (colindex>=2)
              g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
            else
              g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=BrawOpts$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
          },
          
          "Categorical Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (showRawData) {
              # if (colindex>=2)
              #   g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize)
              # else
                g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=BrawOpts$plotShapes$data, colour = "black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots)
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
                # g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, size =dotSize, alpha=0.95, colour="black")
                g<-g+geom_point(data=pts,aes(x=full_x,y=full_y),shape=BrawOpts$plotShapes$data, size =dotSize, alpha=alphaPoints, colour="black",fill="white")
              } else {
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=full_x,y=full_y,fill=factor(full_f)),shape=BrawOpts$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints)
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=BrawOpts$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black",fill=full_c)
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
                # g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, size =dotSize, alpha=0.95, colour="black")
                g<-g+geom_point(data=pts,aes(x=full_x,y=full_y),shape=BrawOpts$plotShapes$data, size =dotSize, alpha=alphaPoints, colour="black",fill="white")
              } else {
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=full_x,y=full_y,fill=factor(full_f)),shape=BrawOpts$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints)
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=BrawOpts$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black",fill=full_c)
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
                g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=BrawOpts$plotShapes$data, size =dotSize, alpha=alphaPoints, colour="black")
              else
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=x,y=y,fill=factor(i2)),shape=BrawOpts$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black")
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=BrawOpts$plotShapes$data, size =dotSize*shrinkDots, colour="black", fill=CatCatcols[i2], alpha=alphaPoints)
                }
            }
            }
          }
  )
 g  
}

plotCatInterDescription<-function(analysis,g=NULL){
  hypothesis<-analysis$hypothesis
  
  plotDescriptionCols <<- c()
  cols<-c()
  for (i in 1:analysis$hypothesis$IV2$ncats){
    off<-(i-1)/(analysis$hypothesis$IV2$ncats-1)
    col<- col2rgb(BrawOpts$plotColours$descriptionC1)*(1-off)+col2rgb(BrawOpts$plotColours$descriptionC2)*off
    cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
  }
  names(cols)<-hypothesis$IV2$cases
  cols<-as.list(cols)
  plotDescriptionCols <<- cols
  
  Ivals<-analysis$iv
  Dvals<-analysis$dv
  rho<-analysis$rIV+seq(-1,1,length.out=hypothesis$IV2$ncats)*analysis$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
    }
  for (i in 1:hypothesis$IV2$ncats){
    use<-analysis$iv2==hypothesis$IV2$cases[i]
    
    analysis1<-analysis
    analysis1$iv<-analysis$iv[use]
    analysis1$dv<-analysis$dv[use]
    analysis1$ivplot<-analysis$ivplot[use]
    analysis1$dvplot<-analysis$dvplot[use]
    analysis1$rIV<-rho[i]
    
    analysis1$hypothesis$IV$vals<-Ivals[use]
    analysis1$hypothesis$DV$vals<-Dvals[use] 
    if (is.numeric(Ivals)) {
    analysis1$hypothesis$IV$mu<-mean(Ivals[use],na.rm=TRUE)
    analysis1$hypothesis$IV$sd<-sd(Ivals[use],na.rm=TRUE)
    }
    if (is.numeric(Dvals)) {
    analysis1$hypothesis$DV$mu<-mean(Dvals[use],na.rm=TRUE)
    analysis1$hypothesis$DV$sd<-sd(Dvals[use],na.rm=TRUE)
    }
    g<-plotPoints(g,analysis1$hypothesis$IV,analysis1$hypothesis$DV,analysis1,i+1,hypothesis$IV2$ncats)
    g<-plotPrediction(analysis1$hypothesis$IV,NULL,analysis1$hypothesis$DV,analysis1,analysis$design,2+(i-1)/(hypothesis$IV2$ncats-1),g,theme=BrawOpts$plotTheme)
  }
  
  # g<-g+scale_fill_manual(name=analysis$hypothesis$IV2$name,values=plotDescriptionCols)
  g
}

plotParInterDescription<-function(analysis,g=NULL){
  col<-c( BrawOpts$plotColours$descriptionC1, BrawOpts$plotColours$descriptionC2)
  names(col)<-c(paste(analysis$hypothesis$IV2$name,"<median",sep=""), paste(analysis$hypothesis$IV2$name,">median",sep=""))
  col<-as.list(col)
  plotDescriptionCols <<- col
  
  Ivals<-analysis$hypothesis$IV$vals
  Dvals<-analysis$hypothesis$DV$vals
  rho<-analysis$rIV+seq(-1,1,length.out=2)*analysis$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
  }
  # long-winded but ensures that means are above the raw data
            use1<-analysis$iv2<median(analysis$iv2)
        analysis1<-analysis
        analysis1$iv<-analysis$iv[use1]
        analysis1$dv<-analysis$dv[use1]
        analysis1$ivplot<-analysis$ivplot[use1]
        analysis1$dvplot<-analysis$dvplot[use1]
        analysis1$rIV<-rho[1]
        
        analysis1$hypothesis$IV$vals<-Ivals[use1]
        analysis1$hypothesis$DV$vals<-Dvals[use1]
        analysis1$hypothesis$DV$mu<-mean(analysis$dv[use1],na.rm=TRUE)
        
            use2<-analysis$iv2>=median(analysis$iv2)
        analysis2<-analysis
        analysis2$iv<-analysis$iv[use2]
        analysis2$dv<-analysis$dv[use2]
        analysis2$ivplot<-analysis$ivplot[use2]
        analysis2$dvplot<-analysis$dvplot[use2]
        analysis2$rIV<-rho[2]
        
        analysis2$hypothesis$IV$vals<-Ivals[use2]
        analysis2$hypothesis$DV$vals<-Dvals[use2]
        analysis2$hypothesis$DV$mu<-mean(analysis$dv[use2],na.rm=TRUE)
        
        g<-plotPoints(g,analysis1$hypothesis$IV,analysis1$hypothesis$DV,analysis1,2,2)
        g<-plotPoints(g,analysis2$hypothesis$IV,analysis2$hypothesis$DV,analysis2,3,2)
        g<-plotPrediction(analysis1$hypothesis$IV,NULL,analysis1$hypothesis$DV,analysis1,analysis$design,2,g,theme=BrawOpts$plotTheme)
        g<-plotPrediction(analysis2$hypothesis$IV,NULL,analysis2$hypothesis$DV,analysis2,analysis$design,3,g,theme=BrawOpts$plotTheme)
        
  g<-g+scale_fill_manual(name=analysis$hypothesis$IV2$name,values=plotDescriptionCols)
  g
}

plotParDescription<-function(analysis,g) {
  
  g<-plotPoints(g,analysis$hypothesis$IV,analysis$hypothesis$DV,analysis,1)
  g<-plotPrediction(analysis$hypothesis$IV,analysis$hypothesis$IV2,analysis$hypothesis$DV,analysis,analysis$design,1,g,theme=BrawOpts$plotTheme)
  g
}

plotCatDescription<-function(analysis,g) {

  g<-plotPrediction(analysis$hypothesis$IV,analysis$hypothesis$IV2,analysis$hypothesis$DV,analysis,analysis$design,1,g,theme=BrawOpts$plotTheme)
  g<-plotPoints(g,analysis$hypothesis$IV,analysis$hypothesis$DV,analysis,1)
  
  if (!doLegendBars && doLegendPoints) {
    g<-g+scale_fill_manual(name=analysis$hypothesis$DV$name,values=CatCatcols,labels=analysis$hypothesis$DV$cases)
  }
  
  g
}

showDescription<-function(analysis=makeAnalysis()) {

  g<-ggplot()
  if (is.null(analysis$hypothesis$IV2)){
    switch (analysis$hypothesis$DV$type,
            "Interval"=g<-plotParDescription(analysis,g),
            "Ordinal"=g<-plotParDescription(analysis,g),
            "Categorical"=g<-plotCatDescription(analysis,g)
    )
  } else{
    switch (analysis$hypothesis$IV2$type,
            "Interval"=g<-plotParInterDescription(analysis,g),
            "Ordinal"=g<-plotParInterDescription(analysis,g),
            "Categorical"=g<-plotCatInterDescription(analysis,g)
    )
  }
  g
}
