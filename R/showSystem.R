##################################################################################    
# SYSTEM diagrams   
# hypothesis diagram
# population diagram
# prediction diagram


#' show a system - hypothesis & design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showSystem(hypothesis=makeHypothesis(),design=makeDesign())
#' @export
showSystem<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence) {
  ygain<-0.95
  g<-NULL
  if (is.null(hypothesis$IV2))
    g<-showHypothesis(hypothesis=hypothesis,doWorld=TRUE,plotArea=c(0.0,0.05,0.45,0.8),g=g)
  else
    g<-showHypothesis(hypothesis=hypothesis,doWorld=TRUE,plotArea=c(0.0,0.05,0.33,0.8),g=g)
  g<-showDesign(hypothesis=hypothesis,design=design,plotArea=c(0.33,0.3,0.28,0.33),g=g)
  g<-showPrediction(hypothesis=hypothesis,design=design,evidence=evidence,plotArea=c(0.65,0.55,0.33,0.4),g=g)
  g<-showWorldSampling(hypothesis=hypothesis,design=design,sigOnly=FALSE,plotArea=c(0.7,0.05,0.28,0.4),g=g)
  
  braw.env$plotArea<-c(0,0,1,1)
  g<-addG(g,axisText(data=data.frame(x=0.02,y=1),"Hypothesis",vjust=1,size=1.5,fontface="bold"))
  g<-addG(g,axisText(data=data.frame(x=0.25,y=1.005),"\uff0b",vjust=1,size=2,fontface="plain"))
  # g<-addG(g,axisPath(data=data.frame(x=c(0,0.33,0.33,0,0),y=c(0,0,1,1,0))))
  g<-addG(g,axisText(data=data.frame(x=0.35,y=1),"Design",vjust=1,size=1.5,fontface="bold"))
  g<-addG(g,axisText(data=data.frame(x=0.6,y=1.005),"\u21d2",vjust=1,size=2,fontface="plain"))
  # g<-addG(g,axisPath(data=data.frame(x=0.33+c(0,0.33,0.33,0),y=c(0,0,1,1))))
  g<-addG(g,axisText(data=data.frame(x=0.73,y=1),"Expected",vjust=1,size=1.5,fontface="bold"))
  # g<-addG(g,axisPath(data=data.frame(x=0.66+c(0,0.33,0.33,0),y=c(0,0,1,1))))
  
  return(g)
}

#' show a hypothesis
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showHypothesis(hypothesis=makeHypothesis())
#' @export
showHypothesis<-function(hypothesis=braw.def$hypothesis,doWorld=TRUE,plotArea=c(0.25,0.0,0.5,1),g=NULL) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(nullPlot())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
    
  doWorld<-doWorld && effect$world$worldOn
  if (doWorld) effect$rIV<-NULL
  ygain<-plotArea[4]
  yoff<-plotArea[2]
  switch(no_ivs,
         { xgain<-plotArea[3]/2
           xoff<-plotArea[1]
           g<-showVariable(IV,plotArea=c(xoff,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
           g<-showVariable(DV,plotArea=c(xoff,yoff,xgain,0.35*ygain),g=g)
           g<-showEffect(effect$rIV,showValue=!doWorld,plotArea=c(xoff,yoff+0.35*ygain,xgain,0.3*ygain),1,g)
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(xoff+0.15,0.35*ygain,xgain*0.9,0.3*ygain),g=g)
         },
         {
           xgain<-plotArea[3]/2
           xoff<-plotArea[1]
           g<-showVariable(IV,plotArea=c(xoff,yoff+0.6*ygain,xgain,0.4*ygain),g=g)
           g<-showVariable(IV2,plotArea=c(xoff+xgain,yoff+0.6*ygain,xgain,0.4*ygain),g=g)
           g<-showVariable(DV,plotArea=c(xoff+xgain/2,yoff,xgain,0.4*ygain),g=g)
           g<-showEffect(effect$rIV,2,showValue=!doWorld,plotArea=c(xoff,yoff+0.4*ygain,xgain,0.22*ygain),g)
           g<-showEffect(effect$rIV2,3,showValue=!doWorld,plotArea=c(xoff+xgain,yoff+0.4*ygain,xgain,0.22*ygain),g)
           g<-showEffect(effect$rIVIV2,4,showValue=!doWorld,plotArea=c(xoff+xgain/2,yoff+0.7*ygain,xgain,0.22*ygain),g)
           g<-showEffect(effect$rIVIV2DV,5,showValue=!doWorld,plotArea=c(xoff+xgain/2,yoff+0.4*ygain,xgain,0.22*ygain),g)
           wgain<-0.8
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(xoff+0.27,0.3*ygain,0.275*wgain,0.38*wgain*ygain),g=g)
         })
  braw.env$plotArea<-plotArea
  return(g)
}

#' show a world object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorld(world=makeWorld())
#' @export
showWorld<-function(hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),g=NULL) {
# world diagram

  world<-hypothesis$effect$world
  if (!world$worldOn) {
    world<-makeWorld(worldOn=TRUE,populationPDF="Single",populationRZ="r",
                     populationPDFk=hypothesis$effect$rIV,populationNullp=0)
  }
    
  braw.env$plotArea<-plotArea

  range<-braw.env$r_range
  if (braw.env$RZ=="z"){range<-tanh(braw.env$z_range)}

  switch(braw.env$RZ,
         "r"={ xticks<-makeTicks(seq(-1,1,0.5));xlabel<-makeLabel(braw.env$rpLabel)},
         "z"={ xticks<-makeTicks(seq(-2,2,1));xlabel<-makeLabel(braw.env$zpLabel)}
  )
  g<-startPlot(xlim=c(-1,1)*range,ylim=c(0,1.05),
               xticks=xticks,xlabel=xlabel,
               box="x",g=g)
  # if (world$worldAbs) {
  #   rx<-seq(0,1,length.out=braw.env$worldNPoints)*range
  # } else {
    rx<-seq(-1,1,length.out=braw.env$worldNPoints)*range
  # }

  rdens<-rPopulationDist(rx,world)
  if (braw.env$RZ=="z") {
    rdens<-rdens2zdens(rdens,rx)
    rx<-atanh(rx)
  }
  if (is.element(world$populationPDF,c("Single","Double"))) {
    rdens<-rdens/sum(rdens)*(1-world$populationNullp)
    rdens[rx==0]<-rdens[rx==0]+world$populationNullp
  } else 
    rdens<-rdens*(1-world$populationNullp)
  rdens<-rdens/max(rdens)
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g<-addG(g,dataPolygon(pts,fill=braw.env$plotColours$descriptionC,colour=NA))
  g<-addG(g,dataLine(pts))
  
  return(g)
}

#' show a design object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDesign(design=makeDesign())
#' @export
showDesign<-function(design=braw.def$design,hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),g=NULL) {
  nRange<-plotAxis("n",hypothesis)
  binRange<-nRange$lim
  
  nbin<-seq(binRange[1],binRange[2],length.out=braw.env$worldNPoints)
  
  if (braw.env$nPlotScale=="log10")  nbin<-10^(nbin)
  if (design$sNRand) {
    ndens<-dgamma(nbin-braw.env$minN,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
    ndens<-ndens/max(ndens)
  } else {
    ndens<-nbin*0
    use=which.min(abs(nbin-design$sN))
    ndens[use]<-1
  }
  
  x<-c(min(nbin),nbin,max(nbin))
  y<-c(0,ndens,0)*0.8
  pts=data.frame(x=log10(x),y=y)
  
  braw.env$plotArea<-plotArea
  g<-startPlot(xlim=binRange, ylim=c(0,1),
               xticks=makeTicks(nRange$ticks,10^nRange$ticks),xlabel=makeLabel(nRange$label),
               box="x",g=g)
  g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC))
  g<-addG(g,dataLine(data=pts))

  if (design$Replication$On) {
    if (!hypothesis$effect$world$worldOn) {
      hypothesis$effect$world$worldOn<-TRUE
      hypothesis$effect$world$populationPDF<-"Single"
      hypothesis$effect$world$populationPDFk<-hypothesis$effect$rIV
      hypothesis$effect$world$populationRZ<-"r"
      hypothesis$effect$world$populationNullp<-0
    }
    nRepDens<-fullRSamplingDist(nbin,hypothesis$effect$world,design,"nw",logScale=(braw.env$nPlotScale=="log10"),sigOnly=FALSE)
    y<-c(0,nRepDens,0)/max(nRepDens)*0.4
    pts=data.frame(x=log10(x),y=y)
    g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$replicationC,alpha=0.5))
    g<-addG(g,dataLine(data=pts))
  }
  return(g)
}

# population diagram
#' show the population corresponding to a hypothesis object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showPopulation(hypothesis=makeHypothesis())
#' @export
showPopulation <- function(hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1)) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(nullPlot())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  switch (no_ivs,
          {
            braw.env$plotArea<-plotArea
            g<-plotPopulation(IV,DV,effect)
            # g<-addG(g,plotTitle(paste0("r[p]=",brawFormat(effect$rIV)),position="centre",size=1,fontface="plain"))
          },
          {
            effect1<-effect
            effect2<-effect
            effect2$rIV<-effect2$rIV2
            effect3<-effect
            effect3$rIV<-effect3$rIVIV2

            braw.env$plotArea<-c(0,0,0.45,0.45)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
            g<-plotPopulation(IV,IV2,effect3)
            braw.env$plotArea<-c(0.55,0,0.45,0.45)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
            g<-plotPopulation(IV,DV,effect1,g=g)
            braw.env$plotArea<-c(0.55/2,0.55,0.45,0.45)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
            g<-plotPopulation(IV2,DV,effect2,g=g)
          }
  )
  return(g)
}

# prediction diagram
#' show the prediction corresponding to a hypothesis & design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showPrediction(hypothesis=makeHypothesis()=makeDesign(),evidence=makeEvidence())
#' @export
showPrediction <- function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=makeEvidence(),plotArea=c(0,0,1,1),g=NULL ){
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(nullPlot())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  if (is.null(g)) g<-nullPlot()
  switch (no_ivs,
          { braw.env$plotArea<-plotArea 
            g<-plotPopulation(IV,DV,effect,g=g)
            g<-plotPrediction(IV,IV2,DV,effect,design,g=g)
            # g<-addG(g,plotTitle(paste0("r[p]=",brawFormat(effect$rIV)),position="centre",size=1,fontface="plain"))
          },
          {
            if (evidence$rInteractionOn==FALSE){
              effect1<-effect
              effect2<-effect
              effect2$rIV<-effect2$rIV2

                braw.env$plotArea<-c(0,0.25,0.5,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
                g1<-plotPrediction(IV,NULL,DV,effect1,design)
                braw.env$plotArea<-c(0.5,0.25,0.5,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
                g2<-plotPrediction(IV2,NULL,DV,effect2,design)
                g<-addG(g1,g2)
              
            } else{
              if (evidence$rInteractionOnly){
                braw.env$plotArea<-plotArea 
                g<-plotPrediction(IV,IV2,DV,effect,design)
              } else{
                effect1<-effect
                effect2<-effect
                effect2$rIV<-effect2$rIV2

                braw.env$plotArea<-c(0,0,0.5,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
                g<-plotPrediction(IV,NULL,DV,effect1,design)
                braw.env$plotArea<-c(0,0.5,0.5,0.5)*plotArea[c(3,4,3,4)] +c(plotArea[c(1,2)],0,0)
                g<-plotPrediction(IV2,NULL,DV,effect2,design,g=g)
                braw.env$plotArea<-c(0.25,0.5,0.5,0.5)*plotArea[c(3,4,3,4)] +c(plotArea[c(1,2)],0,0)
                g<-plotPrediction(IV,IV2,DV,effect,design,g=g)
                
              }
            }
          }
  )
  return(g)
}
##################################################################################    

# world sampling distribution
#' show the prediction corresponding to a hypothesis & design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorldSampling(hypothesis=makeHypothesis(),design=makeDesign(),sigOnly=FALSE)
#' @export
showWorldSampling<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,sigOnly=FALSE,plotArea=c(0,0,1,1),g=NULL) {
  world<-hypothesis$effect$world
  if (!world$worldOn) 
    world<-list(worldOn=TRUE,
                populationPDF="Single",
                populationPDFk=hypothesis$effect$rIV,
                populationRZ="r",
                populationNullp=0)
  
  np<-braw.env$worldNPoints
  # if (world$worldAbs) np<-braw.env$worldNPoints*2+1
  
  vals<-seq(-1,1,length=np)*braw.env$r_range
  if (braw.env$RZ=="z") {
    vals<-tanh(seq(-1,1,length=np*2)*braw.env$z_range*2)
  }
  
  design1<-design
  design$Replication$On<-FALSE
  
  dens<-fullRSamplingDist(vals,world,design,sigOnly=sigOnly) 
  if (braw.env$RZ=="z") {
    dens<-rdens2zdens(dens,vals)
    vals<-atanh(vals)
    use<-abs(vals)<=braw.env$z_range
    dens<-dens[use]
    vals<-vals[use]
  }
  dens<-dens/sum(dens)
  if (design1$Replication$On) {
    dens1<-fullRSamplingDist(vals,world,design1,sigOnly=sigOnly) 
    dens1<-dens1/sum(dens1)
  } else dens1<-NA
  gain<-max(max(dens),max(dens1),na.rm=TRUE)
  dens<-dens/gain
  dens1<-dens1/gain
  
  x<-c(vals[1],vals,vals[length(vals)])
  y<-c(0,dens,0)
  pts=data.frame(x=x,y=y)
  
  braw.env$plotArea<-plotArea

  switch(braw.env$RZ,
         "r"={ xticks<-makeTicks(seq(-1,1,0.5));xlabel<-makeLabel(braw.env$rsLabel)},
         "z"={ xticks<-makeTicks(seq(-2,2,1));xlabel<-makeLabel(braw.env$zsLabel)}
  )
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1.05),
               xticks=xticks,xlabel=xlabel,
               top=TRUE,box="x",g=g)
  g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC))
  g<-addG(g,dataLine(data=pts))
  
  if (!is.na(dens1[1])) {
    y<-c(0,dens1,0)
    pts=data.frame(x=x,y=y)
    g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$replicationC,alpha=0.5))
    g<-addG(g,dataLine(data=pts))
    
  }
  return(g)
}

