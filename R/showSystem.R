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
showHypothesis<-function(hypothesis=makeHypothesis()) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme)}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
    
  PlotNULL<-ggplot()+braw.env$blankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  switch (no_ivs,
          {
            xmin<-4
            xmax<-6
            g<-PlotNULL+
              annotation_custom(grob=ggplotGrob(showVariable(IV)),xmin=xmin,xmax=xmax,ymin=6,ymax=10)+
              annotation_custom(grob=ggplotGrob(showVariable(DV)),xmin=xmin,xmax=xmax,ymin=0,ymax=4)
            # arrow
            g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,1)),xmin=xmin,xmax=xmax,ymin=3.5,ymax=6)
          },
          {
            xmin<-2
            xmax<-8
            g<-PlotNULL+
              annotation_custom(grob=ggplotGrob(showVariable(IV)), xmin=0,  xmax=4,  ymin=6, ymax=9)+
              annotation_custom(grob=ggplotGrob(showVariable(IV2)),xmin=6,  xmax=10, ymin=6, ymax=9)+
              annotation_custom(grob=ggplotGrob(showVariable(DV)), xmin=3,  xmax=7,  ymin=0.5, ymax=3.5)
            # arrows
            g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,2)),xmin=1.5,xmax=5.5,ymin=3, ymax=7)+
              annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV2,3)),xmin=4.5,xmax=8.5,ymin=3, ymax=7)+
              annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2,4)),xmin=3,  xmax=7,  ymin=6, ymax=9)+
              annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2DV,5)),xmin=3,  xmax=7,  ymin=3, ymax=7)
          }
  )
  g
}

#' show a world object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorld(world=makeWorld())
#' @export
showWorld<-function(world=makeWorld()) {
# world diagram

  PlotNULL<-ggplot()+braw.env$blankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)

  switch(braw.env$RZ,
         "r"={range<-braw.env$r_range},
         "z"={range<-tanh(braw.env$z_range)}
  )
  if (world$worldAbs) {
    rx<-seq(0,1,length.out=braw.env$worldNPoints)*range
  } else {
    rx<-seq(-1,1,length.out=braw.env$worldNPoints)*range
  }

  rdens<-fullRPopulationDist(rx,world)

  if (braw.env$RZ=="z") {
    rdens<-rdens2zdens(rdens,rx)
    rx<-atanh(rx)
  }
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g1<-ggplot(pts,aes(x=x,y=y))
  g1<-g1+geom_polygon(data=pts,aes(x=x,y=y),fill=braw.env$plotColours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g1<-g1+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  switch(braw.env$RZ,
         "r"={ g1<-g1+labs(x=braw.env$rpLabel,y="Density")+braw.env$diagramTheme },
         "z"={ g1<-g1+labs(x=braw.env$zpLabel,y="Density")+braw.env$diagramTheme }
         )

  g<-g1

  g
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
    ndens<-nbin*0+0.01
    use=which.min(abs(nbin-design$sN))
    ndens[use]<-1
  }
  x<-c(min(nbin),nbin,max(nbin))
  y<-c(0,ndens,0)
  
  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  g<-g+geom_polygon(data=pts,aes(x=x,y=y),fill=braw.env$plotColours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g<-g+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  g<-g+labs(x="n",y="Density")+braw.env$diagramTheme
  
  g  
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
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme)}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  switch (no_ivs,
          {
            g<-plotPopulation(IV,DV,effect,alpha=1)
          },
          {
            effect1<-effect
            effect2<-effect
            effect2$rIV<-effect2$rIV2
            effect3<-effect
            effect3$rIV<-effect3$rIVIV2

            g<-joinPlots(
              plotPopulation(IV,DV,effect1,alpha=1),
              plotPopulation(IV2,DV,effect2,alpha=1),
              plotPopulation(IV,IV2,effect3,alpha=1)
            )
          }
  )
  g
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
  if (is.null(IV) || is.null(DV)) {return(ggplot()+braw.env$blankTheme)}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  switch (no_ivs,
          {  g<-plotPrediction(IV,IV2,DV,effect,design)
          },
          {
            if (evidence$rInteractionOn==FALSE){
              effect1<-effect
              effect2<-effect
              effect2$rIV<-effect2$rIV2

              g<-joinPlots(
                plotPrediction(IV,NULL,DV,effect1,design),
                plotPrediction(IV2,NULL,DV,effect2,design)
              )
            } else{
              if (braw.env$showInteractionOnly){
                g<-plotPrediction(IV,IV2,DV,effect,design)
              } else{
                effect1<-effect
                effect2<-effect
                effect2$rIV<-effect2$rIV2

                g<-joinPlots(
                  plotPrediction(IV,NULL,DV,effect1,design),
                  plotPrediction(IV2,NULL,DV,effect2,design),
                  plotPrediction(IV,IV2,DV,effect,design)
                )
              }
            }
          }
  )
  g
}
##################################################################################    

plotWorldSampling<-function(effect,design,sigOnly=FALSE) {
  g<-ggplot()
  
  np<-braw.env$worldNPoints
  if (effect$world$worldAbs) np<-braw.env$worldNPoints*2+1
  
  vals<-seq(-1,1,length=np)*braw.env$r_range
  if (braw.env$RZ=="z") {
    vals<-tanh(seq(-1,1,length=np*2)*braw.env$z_range*2)
  }
  
  dens<-fullRSamplingDist(vals,effect$world,design,sigOnly=sigOnly) 
  if (effect$world$worldAbs) {
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
  g<-g+geom_polygon(data=pts,aes(x=x,y=y),fill="yellow")+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g<-g+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  switch(braw.env$RZ,
         "r"={g<-g+labs(x=braw.env$rsLabel,y="Frequency")+braw.env$diagramTheme},
         "z"={g<-g+labs(x=braw.env$zsLabel,y="Frequency")+braw.env$diagramTheme}
  )
  g+theme(plot.margin=margin(1.3,0.8,0,0.25,"cm"))
}

