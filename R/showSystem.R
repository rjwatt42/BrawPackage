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
showSystem<-function(hypothesis=braw.def$hypothesis,design=braw.def$design) {
  ygain<-0.95
  g<-NULL
  g<-showHypothesis(hypothesis=hypothesis,doWorld=TRUE,ygain=ygain,g=g)
  g<-showDesign(hypothesis=hypothesis,design=design,plotArea=c(0.525,0.5*ygain,0.45,0.525*ygain),g=g)
  g<-showWorldSampling(hypothesis=hypothesis,design=design,sigOnly=FALSE,plotArea=c(0.525,0.0,0.45,0.525*ygain),g=g)
  g<-g+geom_vline(xintercept=0.5,linetype="dotted")
  g<-g+geom_text(data=data.frame(x=0.22,y=0.99),aes(x = x, y = y), label="Hypothesis",
                 vjust=0,fontface="bold",size=1.3*braw.env$labelSize)
  g<-g+geom_text(data=data.frame(x=0.75,y=0.99),aes(x = x, y = y), label="Design",
                 vjust=0,fontface="bold",size=1.3*braw.env$labelSize)
  return(g)
}

#' show a hypothesis
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showHypothesis(hypothesis=makeHypothesis())
#' @export
showHypothesis<-function(hypothesis=braw.def$hypothesis,doWorld=TRUE,ygain=1,xgain=0.3,g=NULL) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
    
  doWorld<-doWorld && effect$world$worldOn
  if (doWorld) {
    xoff<-0.05
    effect$rIV<-NULL
  } else xoff<-0.1
  switch(no_ivs,
         {  w<-xgain*0.85
           g<-showVariable(IV,plotArea=c(xoff,0.6*ygain,w,0.4*ygain),g=g)
           g<-showVariable(DV,plotArea=c(xoff,0.0,w,0.4*ygain),g=g)
           g<-showEffect(effect$rIV,showValue=!doWorld,plotArea=c(xoff,0.36*ygain,w,0.3*ygain),1,g)
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(xoff+0.15,0.3*ygain,w*0.9,0.38*ygain),g=g)
         },
         {
           w<-xgain*0.25/0.3
           g<-showVariable(IV,plotArea=c(0.0,0.6*ygain,w,0.4*ygain),g=g)
           g<-showVariable(IV2,plotArea=c(w,0.6*ygain,w,0.4*ygain),g=g)
           g<-showVariable(DV,plotArea=c(w/2,0.0,w,0.4*ygain),g)
           g<-showEffect(effect$rIV,2,showValue=!doWorld,plotArea=c(0,0.4*ygain,w,0.22*ygain),g)
           g<-showEffect(effect$rIV2,3,showValue=!doWorld,plotArea=c(w,0.4*ygain,w,0.22*ygain),g)
           g<-showEffect(effect$rIVIV2,4,showValue=!doWorld,plotArea=c(w/2,0.7*ygain,w,0.22*ygain),g)
           g<-showEffect(effect$rIVIV2DV,5,showValue=!doWorld,plotArea=c(w/2,0.4*ygain,w,0.22*ygain),g)
           wgain<-0.8
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(xoff+0.27,0.3*ygain,0.275*wgain,0.38*wgain*ygain),g=g)
         })
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
    
  if (is.null(g)) 
    g<-ggplot()+coord_cartesian(xlim = c(0,1)+c(-1,1)*0.1, ylim = c(0, 1)+c(-1,1)*0.1)+braw.env$blankTheme()
  
  braw.env$plotArea<-plotArea

  range<-braw.env$r_range
  if (braw.env$RZ=="z"){range<-tanh(braw.env$z_range)}

  g<-startPlot(xlim=c(-1,1)*range,ylim=c(0,1.05),box="x",g=g,fontScale=1)
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
    rdens[rx==0]<-world$populationNullp
  } else 
    rdens<-rdens*(1-world$populationNullp)
  rdens<-rdens/max(rdens)
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g<-g+dataPolygon(pts,fill=braw.env$plotColours$descriptionC,colour=NA)
  g<-g+dataLine(pts)
  switch(braw.env$RZ,
         "r"={ g<-g+xAxisTicks(seq(-1,1,0.5))+xAxisLabel(braw.env$rpLabel)},
         "z"={ g<-g+xAxisTicks(seq(-2,2,1))+xAxisLabel(braw.env$zpLabel)}
  )
  
  return(g)
}

#' show a design object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDesign(design=makeDesign())
#' @export
showDesign<-function(design=braw.def$design,hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),g=NULL) {
  nRange<-plotAxis("n")
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
  if (is.null(g))
    g<-ggplot()+coord_cartesian(xlim = c(0,1)+c(-1,1)*0.1, ylim = c(0,1)+c(-1,1)*0.1) + braw.env$blankTheme()
  g<-startPlot(xlim=binRange, ylim=c(0,1),
               box="x",g=g,fontScale=1)
  g<-g+xAxisTicks(nRange$ticks,10^nRange$ticks)+xAxisLabel(nRange$label)
  g<-g+dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC)
  g<-g+dataLine(data=pts)

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
    g<-g+dataPolygon(data=pts,fill=braw.env$plotColours$replicationC,alpha=0.5)
    g<-g+dataLine(data=pts)
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
showPopulation <- function(hypothesis=braw.def$hypothesis) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  switch (no_ivs,
          {
            braw.env$plotArea<-c(0,0,1,1)
            g<-plotPopulation(IV,DV,effect)
          },
          {
            effect1<-effect
            effect2<-effect
            effect2$rIV<-effect2$rIV2
            effect3<-effect
            effect3$rIV<-effect3$rIVIV2

            braw.env$plotArea<-c(0,0,0.45,0.45)
            g<-plotPopulation(IV,IV2,effect3)
            braw.env$plotArea<-c(0.55,0,0.45,0.45)
            g<-plotPopulation(IV,DV,effect1,g=g)
            braw.env$plotArea<-c(0.55/2,0.55,0.45,0.45)
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
showPrediction <- function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=makeEvidence()){
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  switch (no_ivs,
          { braw.env$plotArea<-c(0,0,1,1) 
            g<-plotPrediction(IV,IV2,DV,effect,design)
          },
          {
            if (evidence$rInteractionOn==FALSE){
              effect1<-effect
              effect2<-effect
              effect2$rIV<-effect2$rIV2

                braw.env$plotArea<-c(0,0,0.5,1) 
                g<-plotPrediction(IV,NULL,DV,effect1,design)
                braw.env$plotArea<-c(0,0.5,0.5,1) 
                g<-plotPrediction(IV2,NULL,DV,effect2,design,g=g)
              
            } else{
              if (evidence$rInteractionOnly){
                g<-plotPrediction(IV,IV2,DV,effect,design)
              } else{
                effect1<-effect
                effect2<-effect
                effect2$rIV<-effect2$rIV2

                braw.env$plotArea<-c(0,0,0.5,0.5) 
                g<-plotPrediction(IV,NULL,DV,effect1,design)
                braw.env$plotArea<-c(0,0.5,0.5,0.5) 
                g<-plotPrediction(IV2,NULL,DV,effect2,design,g=g)
                braw.env$plotArea<-c(0.25,0.5,0.5,0.5) 
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
  if (is.null(g))
    g<-ggplot()+coord_cartesian(xlim = c(0,1)+c(-1,1)*0.1, ylim = c(0,1)+c(-1,1)*0.1) + braw.env$blankTheme()
  
  g<-startPlot(xlim=c(-1,1), ylim=c(0,1.05),box="x",g=g)
  switch(braw.env$RZ,
         "r"={g<-g+xAxisTicks(seq(-1,1,0.5))+xAxisLabel(braw.env$rsLabel)},
         "z"={g<-g+xAxisTicks(seq(-1,1,0.5))+xAxisLabel(braw.env$zsLabel)}
  )
  g<-g+dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC)
  g<-g+dataLine(data=pts)
  
  if (!is.na(dens1[1])) {
    y<-c(0,dens1,0)
    pts=data.frame(x=x,y=y)
    g<-g+dataPolygon(data=pts,fill=braw.env$plotColours$replicationC,alpha=0.5)
    g<-g+dataLine(data=pts)
    
  }
  return(g)
}

