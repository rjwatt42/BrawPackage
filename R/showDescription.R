
plotPoints<-function(g,IV,DV,analysis,colindex=1,maxoff=1){

  if (braw.env$allScatter && !braw.env$newSampleDisplay) showRawData<-TRUE
  else showRawData<-FALSE
  
  if (colindex==1)
          {  col<- braw.env$plotColours$descriptionC
          alphaPoints<-0.95
          xoff=0
          off=0
          barwidth=1
          } else { 
          col <-braw.env$plotDescriptionCols[[colindex-1]]
          alphaPoints<-0.95
          off<-(colindex-2)/(maxoff-1)-0.5
          xoff=0.1
          barwidth=0.5
          }

  x<-analysis$ivplot
  y<-analysis$dvplot
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")

  dotSize<-braw.env$dotSize
  shrinkDots=1
  if (length(x)>100) {
    dotSize<-max(dotSize*sqrt(100/length(x)),2)
  }
  # dotSize<-dotSize/2
  
  switch (hypothesisType,
          "Interval Interval"={
            pts<-data.frame(x=x,y=y)
            if (showRawData) {
              g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots))
            }
          },
          
          "Ordinal Interval"={
            pts<-data.frame(x=x,y=y)
            if (showRawData) {
              g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots))
            }
          },
          
          "Categorical Interval"={
            pp<-CatProportions(IV)
            if (colindex>1)
            x<-(analysis$ivplot-as.numeric(analysis$iv))/2+as.numeric(analysis$iv)
            pts<-data.frame(x=x+off/4,y=y)
            if (showRawData) {
                g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour = "black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots))
            }
          },
          
          "Ordinal Ordinal"={
            pts<-data.frame(x=x,y=y)
            if (showRawData) {
              g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots))
            }
          },
          
          "Interval Ordinal"={
            pts<-data.frame(x=x,y=y)
            if (showRawData) {
              if (colindex>=2) {
              pts<-data.frame(x=x,y=y,fill=names(braw.env$plotDescriptionCols)[colindex-1])
              g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour = "black", alpha=alphaPoints, size =dotSize))
            } else
              g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour="black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots))
            }
          },
          
          "Categorical Ordinal"={
            pts<-data.frame(x=x,y=y);
            if (showRawData) {
                g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, colour = "black", fill=col, alpha=alphaPoints, size =dotSize*shrinkDots))
            }
          },
          
          "Interval Categorical"={
            np<-7
            bin_breaks<-c(-Inf,seq(-1,1,length.out=np)*braw.env$fullRange*sd(analysis$iv)+mean(analysis$iv),Inf)
            dens2<-hist(analysis$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            if (braw.env$onesided) i2<-2:1
            else i2=1:2
            for (i2 in i2){
              xv<-c()
              yv<-c()
              dens1<-hist(analysis$iv[analysis$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
              densities<-dens1$counts/dens2$counts
              for (i in 1:(length(dens1$counts)-1)){
                y<-dens1$counts[i]
                if (y>0){
                  xv<-c(xv,rep(dens1$mids[i],y)+runif(y,min=-0.08,max=0.08))
                  yv<-c(yv,seq(0,densities[i],length.out=y))
                }
              }
              if (i2==1) yv<-(1-yv)
              full_x<-c(full_x,xv)
              full_y<-c(full_y,yv)
              full_f<-c(full_f,rep(i2,length(xv)))
              if (i2==1) col<-darken(col,0.25,off=0.75)
              full_c<-c(full_c,rep(col,length(xv)))
            }

            pts<-data.frame(x=full_x+xoff,y=full_y)
            if (showRawData) {
              if (colindex>=2) {
                g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, size =dotSize, alpha=alphaPoints, colour="black", fill=full_c))
              } else {
                g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black",fill=full_c))
              }
            }
          },
          
          
          "Ordinal Categorical"={
            bin_breaks<-c(-Inf,seq(-1,1,length.out=braw.env$varNPoints-1)*braw.env$fullRange*sd(analysis$iv)+mean(analysis$iv),Inf)
            dens2<-hist(analysis$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            if (braw.env$onesided) i2<-1
            else i2=1
            for (i2 in i2:DV$ncats){
              xv<-c()
              yv<-c()
              dens1<-hist(analysis$iv[analysis$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
              densities<-dens1$counts/dens2$counts
              for (i in 1:(length(dens1$counts)-1)){
                y<-dens1$counts[i]
                if (y>0){
                  xv<-c(xv,rep(bins[i],y)+runif(y,min=-0.08,max=0.08))
                  yv<-c(yv,seq(0,densities[i],length.out=y))
                }
              }
              if (i2==1) yv<-(1-yv)
              full_x<-c(full_x,xv)
              full_y<-c(full_y,yv)
              full_f<-c(full_f,rep(i2,length(xv)))
              if (colindex==1) {
                col<-braw.env$plotColours$descriptionC
                if (i2==1) col<-darken(col,0.25,off=0.75)
                full_c<-c(full_c,rep(col,length(xv)))
              }
            }
            pts<-data.frame(x=full_x,y=full_y)
            if (showRawData) {
              if (colindex>=2) {
                g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, size =dotSize, alpha=alphaPoints, colour="black",fill="white"))
              } else {
                g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, size =dotSize*shrinkDots, alpha=alphaPoints, colour="black",fill=full_c))
              }
            }
          },
          
          "Categorical Categorical"={
            b<-(1:IV$ncats)
            xv<-as.numeric(analysis$iv)
            yv<-as.numeric(analysis$dv)
            
            pp<-matrix(NA,DV$ncats,IV$ncats)
            for (i1 in 1:IV$ncats) {
              for (i2 in 1:DV$ncats) {
                pp[i2,i1]<-sum(yv[xv==i1]==i2)/length(xv)
              }
            }
            
            if (colindex==1) barwidth<-0.5
            else barwidth<-0.25
            for (i2 in DV$ncats:1) {
              x<-b[xv[yv==i2]]+((i2-1)/(DV$ncats-1)-0.5)*barwidth/2+runif(length(xv[yv==i2]),min=-0.1,max=0.1)*0
              y<-c()
              x<-c()
              for (i1 in 1:IV$ncats) {
                np1<-sum(yv[xv==i1]==i2)
                y<-c(y,seq(0,pp[i2,i1],length.out=np1))
                x<-c(x,rep(i1,np1)+((i2-1)/(DV$ncats-1)-0.5)*barwidth/2+runif(np1,min=-0.1,max=0.1)/5)
              }
              # y<-pp[i2,xv[yv==i2]]*runif(length(xv[yv==i2]),min=0.05,max=0.9)
              # y<-y-min(y)
              if (colindex>1) {
                # x<-x/2.5
                xoff<-(colindex-2.5)*0.5
                } else xoff<-0
              pts<-data.frame(x=x+xoff,y=y)
              if (showRawData) {
                if (colindex>=2) {
                  if (i2==1) col<-darken(col,0.25,off=0.75)
                  g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, size =dotSize, alpha=alphaPoints, colour="black", fill=col))
                } else {
                  col<-braw.env$plotColours$descriptionC
                  if (i2==1) col<-darken(col,0.5,off=0.5)
                  g<-addG(g,dataPoint(data=pts,shape=braw.env$plotShapes$data, size =dotSize*shrinkDots, colour="black", fill=col, alpha=alphaPoints))
                }
              }
            }
          }
  )
 g  
}

plotCatInterDescription<-function(analysis,g=NULL){
  hypothesis<-analysis$hypothesis
  
  braw.env$plotDescriptionCols <- c()
  cols<-c()
  for (i in 1:analysis$hypothesis$IV2$ncats){
    off<-(i-1)/(analysis$hypothesis$IV2$ncats-1)
    col<- col2rgb(braw.env$plotColours$descriptionC1)*(1-off)+col2rgb(braw.env$plotColours$descriptionC2)*off
    cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
  }
  names<-hypothesis$IV2$cases
  braw.env$plotDescriptionCols <- cols
  
  Ivals<-analysis$iv
  Dvals<-analysis$dv
  rho<-analysis$rIV+seq(-1,1,length.out=hypothesis$IV2$ncats)*analysis$rIVIV2DV
  
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
    
    if (analysis1$hypothesis$DV$type=="Categorical") {
      g<-plotPrediction(analysis1$hypothesis$IV,NULL,analysis1$hypothesis$DV,analysis1,analysis$design,2+(i-1)/(hypothesis$IV2$ncats-1),g=g)
      g<-plotPoints(g,analysis1$hypothesis$IV,analysis1$hypothesis$DV,analysis1,i+1,hypothesis$IV2$ncats)
    } else {
      g<-plotPoints(g,analysis1$hypothesis$IV,analysis1$hypothesis$DV,analysis1,i+1,hypothesis$IV2$ncats)
      g<-plotPrediction(analysis1$hypothesis$IV,NULL,analysis1$hypothesis$DV,analysis1,analysis$design,2+(i-1)/(hypothesis$IV2$ncats-1),g=g)
    }
  }
  g<-addG(g,dataLegend(data.frame(names=names,colours=cols),title=analysis$hypothesis$IV2$name))
  
  g
}

plotParInterDescription<-function(analysis,g=NULL){
  col<-c( braw.env$plotColours$descriptionC1, braw.env$plotColours$descriptionC2)
  names<-c(paste(analysis$hypothesis$IV2$name," < median",sep=""), paste(analysis$hypothesis$IV2$name," > median",sep=""))
  # col<-as.list(col)
  braw.env$plotDescriptionCols <- col

  Ivals<-analysis$iv
  Dvals<-analysis$dv
  rho<-analysis$rIV+seq(-1,1,length.out=2)*analysis$rIVIV2DV
  
  # long-winded but ensures that means are above the raw data
            use1<-analysis$iv2<median(analysis$iv2)
        analysis1<-analysis
        analysis1$participant<-analysis$participant[use1]
        analysis1$iv<-analysis$iv[use1]
        analysis1$iv2<-NULL
        analysis1$dv<-analysis$dv[use1]
        analysis1$ivplot<-analysis$ivplot[use1]
        analysis1$iv2plot<-NULL
        analysis1$dvplot<-analysis$dvplot[use1]
        # analysis1$rIV<-rho[1]
        
        analysis1$hypothesis$IV$vals<-Ivals[use1]
        analysis1$hypothesis$DV$vals<-Dvals[use1]
        analysis1$hypothesis$DV$mu<-mean(analysis$dv[use1],na.rm=TRUE)
        analysis1<-doAnalysis(analysis1)

            use2<-analysis$iv2>=median(analysis$iv2)
        analysis2<-analysis
        analysis2$participant<-analysis$participant[use2]
        analysis2$iv<-analysis$iv[use2]
        analysis2$iv2<-NULL
        analysis2$dv<-analysis$dv[use2]
        analysis2$ivplot<-analysis$ivplot[use2]
        analysis2$iv2plot<-NULL
        analysis2$dvplot<-analysis$dvplot[use2]
        # analysis2$rIV<-rho[2]
        
        analysis2$hypothesis$IV$vals<-Ivals[use2]
        analysis2$hypothesis$DV$vals<-Dvals[use2]
        analysis2$hypothesis$DV$mu<-mean(analysis$dv[use2],na.rm=TRUE)
        analysis2<-doAnalysis(analysis2)
        
        range1<-c(min(analysis1$ivplot),max(analysis1$ivplot))
        range2<-c(min(analysis2$ivplot),max(analysis2$ivplot))
        if (analysis1$hypothesis$DV$type=="Categorical") {
          g<-plotPrediction(analysis1$hypothesis$IV,NULL,analysis1$hypothesis$DV,analysis1,analysis$design,offset=2,g=g)
          g<-plotPrediction(analysis2$hypothesis$IV,NULL,analysis2$hypothesis$DV,analysis2,analysis$design,offset=3,g=g)
          g<-plotPoints(g,analysis1$hypothesis$IV,analysis1$hypothesis$DV,analysis1,2,2)
          g<-plotPoints(g,analysis2$hypothesis$IV,analysis2$hypothesis$DV,analysis2,3,2)
        } else {
          g<-plotPoints(g,analysis1$hypothesis$IV,analysis1$hypothesis$DV,analysis1,2,2)
          g<-plotPoints(g,analysis2$hypothesis$IV,analysis2$hypothesis$DV,analysis2,3,2)
          g<-plotPrediction(analysis1$hypothesis$IV,NULL,analysis1$hypothesis$DV,analysis1,analysis$design,offset=2,range=range1,g=g)
          g<-plotPrediction(analysis2$hypothesis$IV,NULL,analysis2$hypothesis$DV,analysis2,analysis$design,offset=3,range=range2,g=g)
        }
   g<-addG(g,dataLegend(data.frame(names=names,colours=col),title=analysis1$hypothesis$IV2$name))     
  g
}

plotParDescription<-function(analysis,g) {
  
  analysis$hypothesis$IV$vals<-analysis$iv
  analysis$hypothesis$DV$vals<-analysis$dv
  
  g<-plotPoints(g,analysis$hypothesis$IV,analysis$hypothesis$DV,analysis,1)
  g<-plotPrediction(analysis$hypothesis$IV,analysis$hypothesis$IV2,analysis$hypothesis$DV,analysis,analysis$design,offset=1,g=g)
  g
}

plotCatDescription<-function(analysis,g) {

  analysis$hypothesis$IV$vals<-analysis$iv
  analysis$hypothesis$DV$vals<-analysis$dv
  
  g<-plotPrediction(analysis$hypothesis$IV,analysis$hypothesis$IV2,analysis$hypothesis$DV,analysis,analysis$design,offset=1,g=g)
  g<-plotPoints(g,analysis$hypothesis$IV,analysis$hypothesis$DV,analysis,1)
  
  g
}

#' show the sample effect-size analysis of a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDescription(analysis=doAnalysis())
#' @export
showDescription<-function(analysis=braw.res$result,plotArea=c(0,0,1,1),g=NULL) {
  if(is.null(analysis)) analysis<-doAnalysis(autoShow=FALSE)
  
  braw.env$plotArea<-plotArea
  
  old_newSampleDisplay<-braw.env$newSampleDisplay
  if (!is.null(analysis$hypothesis$IV2)) setBrawEnv("newSampleDisplay",FALSE)
  if (analysis$hypothesis$DV$type=="Categorical") setBrawEnv("newSampleDisplay",FALSE)
  
  if (braw.env$newSampleDisplay && braw.env$allScatter) {
    g<-showSample(analysis,marginals=FALSE)
  } else {
    if (is.null(g)) {
      g<-getAxisPrediction(analysis$hypothesis) 
    }
  }
  if (is.null(analysis$hypothesis$IV2)){
    switch (analysis$hypothesis$DV$type,
            "Interval"=g<-plotParDescription(analysis,g),
            "Ordinal"=g<-plotParDescription(analysis,g),
            "Categorical"=g<-plotCatDescription(analysis,g)
    )
    g<-addG(g,dataLegend(data.frame(names=c(paste0("n=",analysis$nval),
                                            paste0("r[s]=",round(analysis$rIV,3))),
                                    colours=c(braw.env$plotColours$sampleC,
                                              braw.env$plotColours$descriptionC)),
                         title="",shape=c(21,22)))
  } else{
    g<-nullPlot()
    if (analysis$evidence$rInteractionOn) {
      if (analysis$evidence$rInteractionOnly) 
        braw.env$plotArea<-c(0,0,1,1)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
      else
        braw.env$plotArea<-c(0.25,0.5,0.45,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
      
      g<-getAxisPrediction(analysis$hypothesis,g=g) 
      switch (analysis$hypothesis$IV2$type,
              "Interval"=g<-plotParInterDescription(analysis,g),
              "Ordinal"=g<-plotParInterDescription(analysis,g),
              "Categorical"=g<-plotCatInterDescription(analysis,g)
      )
      yoff<-0
    } else yoff<-0.25
    
    if (!analysis$evidence$rInteractionOn || !analysis$evidence$rInteractionOnly) {
      analysis1<-analysis
      analysis1$hypothesis$IV2<-NULL
      analysis2<-analysis
      analysis2$hypothesis$IV<-analysis2$hypothesis$IV2
      analysis2$iv<-analysis2$iv2
      analysis2$ivplot<-analysis2$iv2plot
      analysis2$rIV<-analysis2$rIV2
      
      analysis2$hypothesis$IV2<-NULL
      
      braw.env$plotArea<-c(0,yoff,0.45,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
      g<-getAxisPrediction(analysis1$hypothesis,g=g) 
      switch (analysis$hypothesis$DV$type,
              "Interval"=g<-plotParDescription(analysis1,g=g),
              "Ordinal"=g<-plotParDescription(analysis1,g=g),
              "Categorical"=g<-plotCatDescription(analysis1,g=g)
      )
      braw.env$plotArea<-c(0.55,yoff,0.45,0.5)*plotArea[c(3,4,3,4)] +c(plotArea[c(1,2)],0,0)
      g<-getAxisPrediction(analysis2$hypothesis,g=g) 
      switch (analysis$hypothesis$DV$type,
              "Interval"=g<-plotParDescription(analysis2,g=g),
              "Ordinal"=g<-plotParDescription(analysis2,g=g),
              "Categorical"=g<-plotCatDescription(analysis2,g=g)
      )
    }
  }
  
  setBrawEnv("newSampleDisplay",old_newSampleDisplay)

  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}
