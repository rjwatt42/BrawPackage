##################################################################################    
# SYSTEM diagrams   
# hypothesis diagram
# population diagram
# prediction diagram

#' show a hypothesis
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showHypothesis(hypothesis=makeHypothesis())
#' @export
showHypothesis<-function(hypothesis=makeHypothesis(),doWorld=FALSE) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
    
  doWorld<-doWorld && effect$world$worldOn
  if (doWorld) {
    xoff<-0.025
    effect$rIV<-NULL
  } else xoff<-0.325
  g<-NULL
  switch(no_ivs,
         { 
           g<-showVariable(IV,plotArea=c(xoff,0.6,0.35,0.4),g)
           g<-showVariable(DV,plotArea=c(xoff,0.0,0.35,0.4),g)
           g<-drawEffectES(effect$rIV,plotArea=c(xoff+0.05,0.42,0.25,0.15),1,g)
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(0.425,0.2,0.55,0.6),g=g)
         },
         {
           g<-showVariable(IV,plotArea=c(0.0,0.6,0.4,0.4),g)
           g<-showVariable(IV2,plotArea=c(0.6,0.6,0.4,0.4),g)
           g<-showVariable(DV,plotArea=c(0.3,0.0,0.4,0.4),g)
           g<-drawEffectES(effect$rIV,2,plotArea=c(0.1,0.4,0.4,0.22),g)
           g<-drawEffectES(effect$rIV2,3,plotArea=c(0.5,0.4,0.4,0.22),g)
           g<-drawEffectES(effect$rIVIV2,4,plotArea=c(0.3,0.7,0.4,0.22),g)
           g<-drawEffectES(effect$rIVIV2DV,5,plotArea=c(0.3,0.4,0.4,0.22),g)
         })
  return(g)
}

#' show a world object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorld(world=makeWorld())
#' @export
showWorld<-function(hypothesis=makeHypothesis(effect=makeEffect(world=makeWorld())),plotArea=c(0,0,1,1),g=NULL) {
# world diagram

  world<-hypothesis$effect$world
  if (!world$worldOn) {
    world<-makeWorld(worldOn=TRUE,populationPDF="Single",populationRZ="r",
                     populationPDFk=hypothesis$effect$rIV,populationNullp=0)
  }
    
  if (is.null(g)) 
    g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0, 1))+braw.env$blankTheme()
  
  braw.env$plotArea<-plotArea

  switch(braw.env$RZ,
         "r"={range<-braw.env$r_range},
         "z"={range<-tanh(braw.env$z_range)}
  )
  g<-startPlot(xlim=c(-1,1)*range,ylim=c(0,1.05),box="x",g=g)
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
  rdens<-rdens/max(rdens)*(1-world$populationNullp)
  
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g<-g+dataPolygon(pts,fill=braw.env$plotColours$descriptionC,colour=NA)
  g<-g+dataLine(pts)
  switch(braw.env$RZ,
         "r"={ g<-g+xAxisLabel(braw.env$rpLabel)+xAxisTicks(seq(-1,1,0.25))},
         "z"={ g<-g+xAxisLabel(braw.env$zpLabel)+xAxisTicks(seq(-2,2,0.5))}
  )
  
  return(g)
}

#' show a design object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDesign(design=makeDesign())
#' @export
showDesign<-function(design=makeDesign()) {
  if (design$sNRand) {
    nbin<-seq(braw.env$minN,braw.env$maxRandN*design$sN,length.out=braw.env$worldNPoints)
    # nbin<-5+seq(0,qgamma(0.99,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK),length.out=101)
    ndens<-dgamma(nbin-braw.env$minN,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
    ndens<-ndens/max(ndens)
  } else {
    nbin<-seq(1,250,length.out=braw.env$worldNPoints)
    ndens<-nbin*0
    use=which.min(abs(nbin-design$sN))
    ndens[use]<-1
  }
  x<-c(min(nbin),nbin,max(nbin))
  y<-c(0,ndens,0)*0.8
  pts=data.frame(x=x,y=y)
  
  braw.env$plotArea<-c(0,0,1,1)
  g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + braw.env$blankTheme()
  g<-startPlot(xlim=c(braw.env$minN,design$sN*braw.env$maxRandN), ylim=c(0,1),
               box="x",g=g)
  g<-g+xAxisLabel("n")+xAxisTicks()
  g<-g+dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC)
  g<-g+dataLine(data=pts)

  return(g)
}

# population diagram
#' show the population corresponding to a hypothesis object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showPopulation(hypothesis=makeHypothesis())
#' @export
showPopulation <- function(hypothesis=makeHypothesis()) {
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
showPrediction <- function(hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence()){
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
showWorldSampling<-function(hypothesis=makeHypothesis(),design=makeDesign(),sigOnly=FALSE) {
  world<-hypothesis$effect$world
  
  np<-braw.env$worldNPoints
  if (world$worldAbs) np<-braw.env$worldNPoints*2+1
  
  vals<-seq(-1,1,length=np)*braw.env$r_range
  if (braw.env$RZ=="z") {
    vals<-tanh(seq(-1,1,length=np*2)*braw.env$z_range*2)
  }
  
  dens<-fullRSamplingDist(vals,world,design,sigOnly=sigOnly) 
  if (world$worldAbs) {
    vals<-vals[braw.env$worldNPoints+(1:braw.env$worldNPoints)]
    dens<-dens[braw.env$worldNPoints+(1:braw.env$worldNPoints)]
  }
  
  if (braw.env$RZ=="z") {
    dens<-rdens2zdens(dens,vals)
    vals<-atanh(vals)
    use<-abs(vals)<=braw.env$z_range
    dens<-dens[use]
    vals<-vals[use]
  }
  dens<-dens/max(dens)
  
  x<-c(vals[1],vals,vals[length(vals)])
  y<-c(0,dens,0)
  pts=data.frame(x=x,y=y)
  
  braw.env$plotArea<-c(0,0,1,1)
  g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0, 1))+braw.env$blankTheme()
  g<-startPlot(xlim=c(braw.env$minN,braw.env$maxN), ylim=c(0,1.05),box="both",g=g)
  switch(braw.env$RZ,
         "r"={g<-g+xAxisLabel(braw.env$rsLabel)},
         "z"={g<-g+xAxisLabel(braw.env$zsLabel)}
  )
  g<-g+xAxisTicks()
  
  g<-g+dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g<-g+dataLine(data=pts)
  return(g)
}

