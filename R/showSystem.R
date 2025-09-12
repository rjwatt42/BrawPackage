##################################################################################    
# SYSTEM diagrams   
# hypothesis diagram
# population diagram
# prediction diagram


#' show a system - hypothesis & design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showSystem(whichShow="all",hypothesis=makeHypothesis(),design=makeDesign())
#' @export
showSystem<-function(whichShow="all",hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence) {
  braw.env$addHistory<-FALSE
  switch(whichShow,
         "all"={
           ygain<-0.95
           g<-NULL
           if (is.null(hypothesis$IV2))
             g<-showHypothesis(hypothesis=hypothesis,evidence=evidence,doWorld=TRUE,plotArea=c(0.0,0.05,0.45,0.8),autoShow=FALSE,g=g)
           else
             g<-showHypothesis(hypothesis=hypothesis,evidence=evidence,doWorld=TRUE,plotArea=c(0.0,0.05,0.33,0.8),autoShow=FALSE,g=g)
           g<-showDesign(hypothesis=hypothesis,design=design,plotArea=c(0.3,0.3,0.28,0.33),autoShow=FALSE,g=g)
           g<-showPrediction(hypothesis=hypothesis,design=design,evidence=evidence,plotArea=c(0.65,0.55,0.33,0.4),autoShow=FALSE,g=g)
           g<-showWorldSampling(hypothesis=hypothesis,design=design,evidence=evidence,plotArea=c(0.7,0.05,0.28,0.4),autoShow=FALSE,g=g)
           
           braw.env$plotArea<-c(0,0,1,1)
           g<-addG(g,axisText(data=data.frame(x=0.02,y=1),"Hypothesis",vjust=1,size=1.2,fontface="bold"))
           g<-addG(g,axisText(data=data.frame(x=0.35,y=1),"Design",vjust=1,size=1.2,fontface="bold"))
           g<-addG(g,axisText(data=data.frame(x=0.73,y=1),"Expected",vjust=1,size=1.2,fontface="bold"))
         },
         "hypothesis"={
           g<-showHypothesis(hypothesis=hypothesis,evidence=evidence)
         },
         "world"={
           g<-showFullWorld(hypothesis=hypothesis)
         },
         "design"={
           g<-showDesign(hypothesis=hypothesis,design=design)
         },
         "population"={
           g<-showPopulation(hypothesis=hypothesis)
         },
         "prediction"={
           g<-showPrediction(hypothesis=hypothesis,design=design,evidence=evidence)
         },
  )
  braw.env$addHistory<-TRUE
  
  if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
    print(g)
    return(invisible(g))
  }
  return(g)  
}

#' show a hypothesis
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showHypothesis(hypothesis=makeHypothesis())
#' @export
showHypothesis<-function(hypothesis=braw.def$hypothesis,evidence=braw.def$evidence,
                         doWorld=TRUE,showValue=TRUE,plotArea=NULL,autoShow=FALSE,g=NULL) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(nullPlot())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
    
  if (is.null(plotArea)) {
    if (no_ivs==1) plotArea<-c(0.15,0.0,0.7,1)
    else           plotArea<-c(0.1,0.0,0.8,1)
  }
  doWorld<-doWorld && effect$world$On
  if (doWorld) {effect$rIV<-NULL; showValue=FALSE}
  ygain<-plotArea[4]
  yoff<-plotArea[2]
  switch(no_ivs,
         { xgain<-plotArea[3]/2
           xoff<-plotArea[1]
           g<-showVariable(IV,plotArea=c(xoff,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
           g<-showVariable(DV,plotArea=c(xoff,yoff,xgain,0.35*ygain),g=g)
           g<-showEffect(c(effect$rIV,effect$rSD),moderator=effect$rM1,type=1,useCols=TRUE,showValue=showValue,plotArea=c(xoff,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(xoff+0.23,0.4*ygain,xgain*0.65,0.27*ygain),g=g)
         },
         {
           xgain<-plotArea[3]/2.5
           xoff<-plotArea[1]
           ygain<-ygain
           r1<-effect$rIV
           r2<-effect$rIV2
           r12<-effect$rIVIV2
           if (effect$rIVIV2!=0) {
             r1<-c(r1,0,r1*sqrt(1-r12^2))
             r2<-c(r2,0,r2*sqrt(1-r12^2))
           } 
           cols<-evidence$AnalysisTerms

           switch(hypothesis$layout,
                  "simple"={
                    g<-showVariable(IV,plotArea=c(xoff-xgain*0.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain/2,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=2,useCols=cols,showValue=showValue,plotArea=c(xoff-xgain*0.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=3,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain*1.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                  },
                  "normal"={
                    g<-showVariable(IV,plotArea=c(xoff-xgain*0.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain/2,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=2,useCols=cols,showValue=showValue,plotArea=c(xoff-xgain*0.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=3,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain*1.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r12,type=4,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain/2,yoff+0.7*ygain,xgain,0.22*ygain),g=g)
                    g<-showEffect(effect$rIVIV2DV,type=5,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain/2,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                  },
                  "path"={
                    g<-showVariable(IV,plotArea=c(xoff,yoff+0.65*ygain,xgain*0.9,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.1,yoff+0.35*ygain,xgain*0.9,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=6,showValue=showValue,plotArea=c(xoff,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=7,showValue=showValue,plotArea=c(xoff+xgain*1.1/2,yoff+0.05*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r12,type=8,showValue=showValue,plotArea=c(xoff+xgain*1.1/2,yoff+0.65*ygain,xgain,0.3*ygain),g=g)
                  },
                  "lpath"={
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.1,yoff+0.65*ygain,xgain*0.9,0.35*ygain),g=g)
                    g<-showVariable(IV,plotArea=c(xoff,yoff+0.35*ygain,xgain*0.9,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain*1.1,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=9,showValue=showValue,plotArea=c(xoff,yoff+0.05*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=10,showValue=showValue,plotArea=c(xoff+xgain*1.1/2,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r12,type=11,showValue=showValue,plotArea=c(xoff,yoff+0.65*ygain,xgain,0.3*ygain),g=g)
                  }
             
           )
           wgain<-0.8
           if (doWorld) g<-showWorld(hypothesis,plotArea=c(xoff+0.27,0.3*ygain,0.275*wgain,0.38*wgain*ygain),g=g)
         })
  braw.env$plotArea<-plotArea
  if (braw.env$graphicsType=="HTML" && autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}


#' show a world object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showFullWorld(world=makeWorld())
#' @export
showFullWorld<-function(hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),fontScale=1,autoShow=FALSE,g=NULL) {
  g<-showWorld(hypothesis=hypothesis,joinNulls=FALSE,plotArea=c(0.05,0.35,0.45,0.55),fontScale=1,g=g)
  s<-hypothesis$effect$world$pRPlus
  
  wd<-0.25
  if ((1-s)<1) {
  braw.env$plotArea<-c(0.1,0.1,0.45,0.3)
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1),back="transparent",box="none",g=g)
  g<-addG(g,drawArrow(c(0,0.9),1,45,"last",col="#000000",
                      fill=braw.env$plotColours$populationC,alpha=1, 
                      width=wd*(s),position="end",finAngle=45),
            dataText(data.frame(x=0,y=0.5),brawFormat(1-s,digits=3),size=0.75))
  }
  
  # g<-showEffect(1-hypothesis$effect$world$pRPlus,showValue=TRUE,
  #               plotArea=c(0.0,0.15,0.6,0.4),2,g)
  
  hypothesis1<-hypothesis
  hypothesis1$effect$world<-makeWorld(TRUE,"Single","r",0)
  g<-showWorld(hypothesis=hypothesis1,plotArea=c(0.55,0.35,0.45,0.55),fontScale=1,g=g)
  
  if (s<1) {
  braw.env$plotArea<-c(0.45,0.1,0.45,0.3)
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1),back="transparent",box="none",g=g)
  g<-addG(g,drawArrow(c(0,0.9),1,-45,"last",col="#000000",
                      fill=braw.env$plotColours$populationC,alpha=1, 
                      width=wd*s,position="end",finAngle=45),
          dataText(data.frame(x=0,y=0.5),brawFormat(1-s,digits=3),hjust=1,size=0.75)
  )
  }
  
  # g<-showEffect(hypothesis$effect$world$pRPlus,showValue=TRUE,
  #               plotArea=c(0.4,0.15,0.6,0.4),3,g)
  
  
  return(g)
  }

#' show a world object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorld(world=makeWorld())
#' @export
showWorld<-function(hypothesis=braw.def$hypothesis,joinNulls=TRUE,
                    plotArea=c(0,0,1,1),fontScale=1,autoShow=FALSE,g=NULL) {
# world diagram

  world<-hypothesis$effect$world
  if (!world$On) {
    world<-makeWorld(On=TRUE,PDF="Single",RZ="r",
                     PDFk=hypothesis$effect$rIV,pRPlus=1)
  }
    
  braw.env$plotArea<-plotArea

  range<-braw.env$r_range
  switch(braw.env$RZ,
      "r"={},
      "z"={range<-tanh(braw.env$z_range)}
  )

  switch(braw.env$RZ,
         "r"={ xticks<-makeTicks(seq(-1,1,0.5));xlabel<-makeLabel(braw.env$rpLabel)},
         "z"={ xticks<-makeTicks(seq(-2,2,1));xlabel<-makeLabel(braw.env$zpLabel)}
  )
  g<-startPlot(xlim=c(-1,1)*range,ylim=c(0,1.05),
               xticks=xticks,xlabel=xlabel,fontScale = fontScale,unitGap=0.45,
               box="x",g=g)
  # if (world$worldAbs) {
  #   rx<-seq(0,1,length.out=braw.env$worldNPoints)*range
  # } else {
    rx<-seq(-1,1,length.out=braw.env$worldNPoints)*range
  # }

  rdens<-rPopulationDist(rx,world)
  switch(braw.env$RZ,
         "r"={},
         "z"={
    rdens<-rdens2zdens(rdens,rx)
    rx<-atanh(rx)
         }
  )
  if (is.element(world$PDF,c("Single","Double"))) {
    width<-0.02
    rx<-world$PDFk+c(-1,-1,1,1)*width
    rdens<-c(0,1,1,0)*(world$pRPlus)
    if (world$PDF=="Double") {
      rx<-c(-rx,rx)
      rdens<-c(rdens,rdens)/2
    }
    if (joinNulls)
    if (world$pRPlus<1) {
      rdens<-rdens/sum(rdens)
      rx<-c(rx,c(-1,-1,1,1)*width)
      rdens<-c(rdens,c(0,1,1,0)*(1-world$pRPlus))
    }
    
    # rdens[rx==0]<-rdens[rx==0]+world$pRPlus
  } 
  if (!is.element(world$PDF,c("sample","biasedsample"))) {
    rdens<-rdens*(world$pRPlus)
  }
  rdens<-rdens/max(rdens)
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g<-addG(g,dataPolygon(pts,fill=braw.env$plotColours$populationC,colour=NA))
  g<-addG(g,dataLine(pts))
  
  if (braw.env$graphicsType=="HTML" && autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}

#' show a design object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDesign(design=makeDesign())
#' @export
showDesign<-function(design=braw.def$design,hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),autoShow=FALSE,g=NULL) {

  nRange<-plotAxis("n",hypothesis)
  binRange<-nRange$lim
  
  braw.env$plotArea<-plotArea
  g<-startPlot(xlim=binRange, ylim=c(0,1),
               xticks=makeTicks(nRange$ticks,10^nRange$ticks),xlabel=makeLabel(nRange$label),
               box="x",g=g)
  
  nbin<-seq(binRange[1],binRange[2],length.out=braw.env$worldNPoints)
  xpts<-c(-1,-1,1,1)*(max(log10(nbin))-min(log10(nbin)))/100
  
  if (braw.env$nPlotScale=="log10")  nbin<-10^(nbin)
  if (design$sNRand) {
    ndens<-nDistrDens(nbin,design)
    ndens<-ndens/max(ndens)
    x<-c(min(nbin),nbin,max(nbin))
    y<-c(0,ndens,0)*0.8
    pts=data.frame(x=log10(x),y=y)
  } else {
    pts<-data.frame(x=log10(design$sN)+xpts,
                    y=c(0,1,1,0)*0.8)
  }
  g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$designC))
  # g<-addG(g,dataLine(data=pts))

  if (is.element(design$sMethod$type,c("Cluster","Snowball","Convenience"))) {
    if (design$sMethodSeverity<1)
      nEffective<-design$sN-design$sN*design$sMethodSeverity
    else
      nEffective<-design$sN-design$sMethodSeverity
    pts<-data.frame(x=log10(nEffective)+xpts,
                      y=c(0,1,1,0)*0.5)
    g<-addG(g,dataPolygon(data=pts,fill=complementary(braw.env$plotColours$designC)))
  }

  if (design$sCheating!="None") {
    switch(design$sCheating,
           "Grow"={
             pts<-data.frame(x=log10(design$sN+design$sN*c(0,0,1,1)*design$sCheatingBudget),
                             y=c(0,1,1,0)*0.5)
             g<-addG(g,dataPolygon(data=pts,fill=complementary(braw.env$plotColours$designC),alpha=0.2))
           },
           "Prune"={
             pts<-data.frame(x=log10(design$sN+design$sN*c(0,0,-1,-1)*design$sCheatingBudget),y=c(0,1,1,0)*0.5)
             g<-addG(g,dataPolygon(data=pts,fill=complementary(braw.env$plotColours$designC),alpha=0.2))
            },
           "Replace"={
             pts<-data.frame(x=log10(design$sN+design$sN*c(-1,-1,1,1)*design$sCheatingBudget),y=c(0,1,1,0)*0.5)
             g<-addG(g,dataPolygon(data=pts,fill=complementary(braw.env$plotColours$designC),alpha=0.2))
           },
           "Retry"={
           }
    )
  }
  
  if (design$Replication$On) {
    if (!hypothesis$effect$world$On) {
      hypothesis$effect$world$On<-TRUE
      hypothesis$effect$world$PDF<-"Single"
      hypothesis$effect$world$PDFk<-hypothesis$effect$rIV
      hypothesis$effect$world$RZ<-"r"
      hypothesis$effect$world$pRPlus<-1
    }
    nRepDens<-fullRSamplingDist(nbin,hypothesis$effect$world,design,"nw",logScale=(braw.env$nPlotScale=="log10"),sigOnly=0)
    y<-c(0,nRepDens,0)/max(nRepDens)*0.4
    x<-nbin[c(1,1:length(nbin),length(nbin))]
    pts=data.frame(x=log10(x),y=y)
    g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$replicationC,alpha=0.5))
    g<-addG(g,dataLine(data=pts))
  }
  if (braw.env$graphicsType=="HTML" && autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}

# population diagram
#' show the population corresponding to a hypothesis object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showPopulation(hypothesis=makeHypothesis())
#' @export
showPopulation <- function(hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),autoShow=FALSE,g=NULL) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(nullPlot())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  if (is.null(g)) g<-nullPlot()
  switch (no_ivs,
          {
            braw.env$plotArea<-plotArea
            g<-plotPopulation(IV,DV,effect,g=g)
            # g<-addG(g,plotTitle(paste0("r[p]=",brawFormat(effect$rIV)),position="centre",size=1,fontface="plain"))
          },
          {
            effect1<-effect
            effect2<-effect
            effect2$rIV<-effect2$rIV2
            effect3<-effect
            effect3$rIV<-effect3$rIVIV2

            braw.env$plotArea<-c(0,0,0.45,0.45)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
            g<-plotPopulation(IV,IV2,effect3,g=g)
            braw.env$plotArea<-c(0.55,0,0.45,0.45)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
            g<-plotPopulation(IV,DV,effect1,g=g)
            braw.env$plotArea<-c(0.55/2,0.55,0.45,0.45)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
            g<-plotPopulation(IV2,DV,effect2,g=g)
          }
  )
  if (braw.env$graphicsType=="HTML" && autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}

# prediction diagram
#' show the prediction corresponding to a hypothesis & design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showPrediction(hypothesis=makeHypothesis()=makeDesign(),evidence=makeEvidence())
#' @export
showPrediction <- function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=makeEvidence(),plotArea=c(0,0,1,1),autoShow=FALSE,g=NULL ){
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  if (is.null(IV) || is.null(DV)) {return(nullPlot())}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2

  switch (no_ivs,
          { braw.env$plotArea<-plotArea 
            g<-getAxisPrediction(hypothesis=list(IV=IV,DV=DV),g=g) 
            g<-plotPopulation(IV,DV,effect,g=g)
            g<-plotPrediction(IV,IV2,DV,effect,design,correction=TRUE,g=g)
            # g<-addG(g,plotTitle(paste0("r[p]=",brawFormat(effect$rIV)),position="centre",size=1,fontface="plain"))
          },
          {
            if (sum(evidence$AnalysisTerms)==1){
              effect1<-effect
              effect2<-effect
              effect2$rIV<-effect2$rIV2
              
              if (is.null(g)) g<-nullPlot()
                braw.env$plotArea<-c(0,0.25,0.45,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
                g<-getAxisPrediction(hypothesis=list(IV=IV,DV=DV),g=g) 
                g<-plotPopulation(IV,DV,effect1,g=g)
                g<-plotPrediction(IV,NULL,DV,effect1,design,g=g)
                braw.env$plotArea<-c(0.55,0.25,0.45,0.5)*plotArea[c(3,4,3,4)]+c(plotArea[c(1,2)],0,0)
                g<-getAxisPrediction(hypothesis=list(IV=IV2,DV=DV),g=g) 
                g<-plotPopulation(IV2,DV,effect2,g=g)
                g<-plotPrediction(IV2,NULL,DV,effect2,design,g=g)

            } else{
              if (evidence$rInteractionOnly){
                braw.env$plotArea<-plotArea 
                g<-getAxisPrediction(hypothesis=list(IV=IV,DV=DV),g=g)
                g<-plotPrediction(IV,IV2,DV,effect,design,g=g)
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
  
  if (braw.env$graphicsType=="HTML" && autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}
##################################################################################    

# world sampling distribution
#' show the prediction corresponding to a hypothesis & design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorldSampling(hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence())
#' @export
showWorldSampling<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,plotArea=c(0,0,1,1),autoShow=braw.env$autoShow,g=NULL) {
  world<-hypothesis$effect$world
  if (!world$On) 
    world<-list(On=TRUE,
                PDF="Single",
                PDFk=hypothesis$effect$rIV,
                RZ="r",
                pRPlus=1)
  
  np<-braw.env$worldNPoints
  # if (world$worldAbs) np<-braw.env$worldNPoints*2+1
  
  vals<-seq(-1,1,length=np)*braw.env$r_range
  switch(braw.env$RZ,
         "r"={},
         "z"={
    vals<-tanh(seq(-1,1,length=np*2)*braw.env$z_range*2)
         }
  )
  
  design1<-design
  design$Replication$On<-FALSE
  
  dens<-fullRSamplingDist(vals,world,design,sigOnly=evidence$sigOnly) 
  switch(braw.env$RZ,
         "r"={},
         "z"={
           dens<-rdens2zdens(dens,vals)
           vals<-atanh(vals)
           use<-abs(vals)<=braw.env$z_range
           dens<-dens[use]
           vals<-vals[use]
         }
  )
  dens<-dens/sum(dens)
  if (design1$Replication$On) {
    dens1<-fullRSamplingDist(vals,world,design1,sigOnly=evidence$sigOnly) 
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

