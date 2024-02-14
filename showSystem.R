##################################################################################    
# SYSTEM diagrams   
# hypothesis diagram
# population diagram
# prediction diagram

showHypothesis<-function(hypothesis) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
    
  PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  xmin<-2
  xmax<-8
  switch (no_ivs,
          {
            g<-PlotNULL+
              annotation_custom(grob=ggplotGrob(showVariable(IV)),xmin=xmin,xmax=xmax,ymin=6,ymax=10)+
              annotation_custom(grob=ggplotGrob(showVariable(DV)),xmin=xmin,xmax=xmax,ymin=0,ymax=4)
            # arrow
            g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,1)),xmin=xmin,xmax=xmax,ymin=3.5,ymax=6)
          },
          {
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

showWorld<-function(world) {
# world diagram

  PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)

  switch(RZ,
         "r"={range<-r_range},
         "z"={range<-tanh(z_range)}
  )
  if (world$worldAbs) {
    rx<-seq(0,1,length.out=worldNPoints)*range
  } else {
    rx<-seq(-1,1,length.out=worldNPoints)*range
  }

  rdens<-fullRPopulationDist(rx,world)

  if (RZ=="z") {
    rdens<-rdens2zdens(rdens,rx)
    rx<-atanh(rx)
  }
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g1<-ggplot(pts,aes(x=x,y=y))
  g1<-g1+geom_polygon(data=pts,aes(x=x,y=y),fill=plotcolours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g1<-g1+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  switch(RZ,
         "r"={ g1<-g1+labs(x=rpLabel,y="Density")+diagramTheme },
         "z"={ g1<-g1+labs(x=zpLabel,y="Density")+diagramTheme }
         )

  g<-g1

  g
}

showDesign<-function(design) {
  if (design$sNRand) {
    nbin<-seq(minN,maxRandN*design$sN,length.out=worldNPoints)
    # nbin<-5+seq(0,qgamma(0.99,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK),length.out=101)
    ndens<-dgamma(nbin-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
    ndens<-ndens/max(ndens)
  } else {
    nbin<-seq(1,250,length.out=worldNPoints)
    ndens<-nbin*0+0.01
    use=which.min(abs(nbin-design$sN))
    ndens[use]<-1
  }
  x<-c(min(nbin),nbin,max(nbin))
  y<-c(0,ndens,0)
  
  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  g<-g+geom_polygon(data=pts,aes(x=x,y=y),fill=plotcolours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g<-g+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  g<-g+labs(x="n",y="Density")+diagramTheme
  
  g  
}

# population diagram
showPopulation <- function(hypothesis) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
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
showPrediction <- function(hypothesis,design){
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
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
              if (showInteractionOnly){
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
