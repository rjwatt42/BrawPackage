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
           r1x2<-effect$rIVIV2DV
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
                  "noCovariation"={
                    g<-showVariable(IV,plotArea=c(xoff-xgain*0.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain/2,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=2,useCols=cols,showValue=showValue,plotArea=c(xoff-xgain*0.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=3,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain*1.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r1x2,type=5,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain/2,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                  },
                  "noInteraction"={
                    g<-showVariable(IV,plotArea=c(xoff-xgain*0.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain/2,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=2,useCols=cols,showValue=showValue,plotArea=c(xoff-xgain*0.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=3,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain*1.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r12,type=4,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain/2,yoff+0.7*ygain,xgain,0.22*ygain),g=g)
                  },
                  "normal"={
                    g<-showVariable(IV,plotArea=c(xoff-xgain*0.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*1.3,yoff+0.65*ygain,xgain,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain/2,yoff,xgain,0.35*ygain),g=g)
                    g<-showEffect(r1,moderator=effect$rM1,type=2,useCols=cols,showValue=showValue,plotArea=c(xoff-xgain*0.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r2,moderator=effect$rM2,type=3,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain*1.3,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r12,type=4,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain/2,yoff+0.7*ygain,xgain,0.22*ygain),g=g)
                    g<-showEffect(r1x2,type=5,useCols=cols,showValue=showValue,plotArea=c(xoff+xgain/2,yoff+0.35*ygain,xgain,0.3*ygain),g=g)
                  },
                  "mediation"={
                    g<-showVariable(IV,plotArea=c(xoff,yoff+0.15*ygain,xgain*0.9*0.85,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*0.9,yoff+0.45*ygain,xgain*0.9*0.85,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain*0.9*2,yoff+0.15*ygain,xgain*0.9*0.85,0.35*ygain),g=g)
                    g<-showEffect(r1,type=13,showValue=showValue,
                                  plotArea=c(xoff+xgain*0.1,yoff+0.15*ygain,xgain*2,0.3*ygain),g=g)
                    g<-showEffect(r2,type=14,showValue=showValue,
                                  plotArea=c(xoff+xgain*1.4,yoff+0.45*ygain,xgain,0.3*ygain),g=g)
                    g<-showEffect(r12,type=15,showValue=showValue,
                                  plotArea=c(xoff+xgain*0.1,yoff+0.45*ygain,xgain,0.3*ygain),g=g)
                  },
                  "moderation"={
                    g<-showVariable(IV,plotArea=c(xoff,yoff+0.15*ygain,xgain*0.9*0.85,0.35*ygain),g=g)
                    g<-showVariable(IV2,plotArea=c(xoff+xgain*0.9,yoff+0.65*ygain,xgain*0.9*0.85,0.35*ygain),g=g)
                    g<-showVariable(DV,plotArea=c(xoff+xgain*0.9*2,yoff+0.15*ygain,xgain*0.9*0.85,0.35*ygain),g=g)
                    g<-showEffect(r1,type=13,showValue=showValue,
                                  plotArea=c(xoff+xgain*0.1,yoff+0.15*ygain,xgain*2,0.3*ygain),g=g)
                    g<-showEffect(r1x2,type=12,showValue=showValue,
                                  plotArea=c(xoff+xgain*0.79,yoff+0.25*ygain,xgain,0.4*ygain),g=g)
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
  s<-hypothesis$effect$world$pRplus
  
  g<-showWorld(hypothesis=hypothesis,joinNulls=FALSE,plotArea=c(0.05,0.35,0.45,0.55),fontScale=1,g=g)
  g<-addG(g,plotTitle(paste0(brawFormat((s)*100,digits=2),"%"),"right",size=1))

  wd<-0.25
  if ((1-s)<1) {
  braw.env$plotArea<-c(0.1,0.1,0.45,0.3)
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1),back="transparent",box="none",g=g)
  # g<-addG(g,drawArrow(c(0,0.9),1,45,"last",col="#000000",
  #                     fill=braw.env$plotColours$populationC,alpha=1, 
  #                     width=wd*(s),position="end",finAngle=45),
  #           dataText(data.frame(x=0,y=0.5),brawFormat(1-s,digits=3),size=0.75))
  }
  
  # g<-showEffect(1-hypothesis$effect$world$pRplus,showValue=TRUE,
  #               plotArea=c(0.0,0.15,0.6,0.4),2,g)
  
  hypothesis1<-hypothesis
  hypothesis1$effect$world<-makeWorld(TRUE,"Single","r",0)
  g<-showWorld(hypothesis=hypothesis1,plotArea=c(0.55,0.35,0.45,0.55),fontScale=1,g=g)
  g<-addG(g,plotTitle(paste0(brawFormat((1-s)*100,digits=2),"%"),"left",size=1))
  
  if (s<1) {
  braw.env$plotArea<-c(0.45,0.1,0.45,0.3)
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1),back="transparent",box="none",g=g)
  # g<-addG(g,drawArrow(c(0,0.9),1,-45,"last",col="#000000",
  #                     fill=braw.env$plotColours$populationC,alpha=1, 
  #                     width=wd*s,position="end",finAngle=45),
  #         dataText(data.frame(x=0,y=0.5),brawFormat(1-s,digits=3),hjust=1,size=0.75)
  # )
  }
  
  # g<-showEffect(hypothesis$effect$world$pRplus,showValue=TRUE,
  #               plotArea=c(0.4,0.15,0.6,0.4),3,g)
  
  
  return(g)
  }

#' show a world object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showWorld(world=makeWorld())
#' @export
showWorld<-function(hypothesis=braw.def$hypothesis,joinNulls=TRUE,showSingle=NULL,
                    totalArea=1,
                    plotArea=c(0,0,1,1),fontScale=1,autoShow=FALSE,g=NULL) {
# world diagram

  totalArea<-totalArea*30
  world<-hypothesis$effect$world
  if (!world$On) {
    world<-makeWorld(On=TRUE,PDF="Single",RZ="r",
                     PDFk=hypothesis$effect$rIV,pRplus=1)
  }
    
  braw.env$plotArea<-plotArea

  np<-braw.env$worldNPoints
  if(braw.env$RZ=="r")
    rx<-seq(-1,1,length=np)*braw.env$r_range
  else
    rx<-seq(-1,1,length=np)*braw.env$z_range
  xlim<-c(min(rx),max(rx))
  
  if (braw.env$RZ=="z") rx<-tanh(rx)
  rdens<-rPopulationDist(rx,world)
  if (braw.env$RZ=="z") {
    rdens<-rdens2zdens(rdens,rx)
    rx<-atanh(rx)
  }
  if (max(rdens)>1) rdens<-rdens/max(rdens)
  
  if (is.element(world$PDF,c("Single","Double"))) {
    width<-0.01*diff(xlim)
    rx<-world$PDFk+c(-1,-1,1,1)*width
    rdens<-c(0,1,1,0)*(world$pRplus)
    if (world$PDF=="Double") {
      rx<-c(-rx,rx)
      rdens<-c(rdens,rdens)/2
    }
    if (joinNulls)
    if (world$pRplus<1) {
      rdens<-rdens/sum(rdens)
      rx<-c(rx,c(-1,-1,1,1)*width)
      rdens<-c(rdens,c(0,1,1,0)*(1-world$pRplus))
    }
  }
  # rdens<-rdens/sum(rdens)*totalArea
  
  if (!is.element(world$PDF,c("sample","biasedsample"))) {
    rdens<-rdens*(world$pRplus)
  }
  
  switch(braw.env$RZ,
         "r"={ xticks<-makeTicks(seq(-1,1,0.5));xlabel<-makeLabel(braw.env$rpLabel)},
         "z"={ xticks<-makeTicks(seq(-2,2,1));xlabel<-makeLabel(braw.env$zpLabel)}
  )
  g<-startPlot(xlim=xlim,ylim=c(0,1.05),
               xticks=xticks,xlabel=xlabel,fontScale = fontScale,
               unitGap=0.35,top=TRUE,box="x",
               g=g)
  
  
  x<-c(rx[1],rx,rx[length(rx)])
  y<-c(0,rdens,0)
  pts=data.frame(x=x,y=y)
  fill<-braw.env$plotColours$populationC
  if (!is.null(showSingle)) fill<-darken(desat(fill,0.5),off=0)
  g<-addG(g,dataPolygon(pts,fill=fill,colour=NA))
  g<-addG(g,dataLine(pts))
  
  if (!is.null(showSingle)) {
    width<-0.02
    srx<-showSingle+c(-1,-1,1,1)*width
    srdens<-c(0,1,1,0)*approx(rx,rdens,showSingle)$y
    
    pts<-data.frame(x=srx,y=srdens)
    g<-addG(g,dataPolygon(pts,fill=braw.env$plotColours$populationC))
    g<-addG(g,dataLine(pts))
  }

  if (braw.env$graphicsType=="HTML" && autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else 
    return(g)  
}

#' show a design object
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDesign(design=makeDesign())
#' @export
showDesign<-function(design=braw.def$design,hypothesis=braw.def$hypothesis,plotArea=c(0,0,1,1),showPsych=TRUE,autoShow=FALSE,g=NULL) {

  nRange<-plotAxis("n",hypothesis)
  binRange<-nRange$lim
  
  braw.env$plotArea<-plotArea
  g<-startPlot(xlim=binRange, ylim=c(0,1),
               xticks=makeTicks(nRange$ticks,10^nRange$ticks),xlabel=makeLabel(nRange$label),
               unitGap=0.35,top=TRUE,box="x",g=g)
  
  nbin<-seq(binRange[1],binRange[2],length.out=braw.env$worldNPoints)
  xpts<-c(-1,-1,1,1)*(max(log10(nbin))-min(log10(nbin)))/50
  
  if (braw.env$nPlotScale=="log10")  nbin<-10^(nbin)
  if (design$sNRand) {
    ndens<-nDistrDens(nbin,design)
    ndens<-ndens/max(ndens)
    x<-c(min(nbin),nbin,max(nbin))
    y<-c(0,ndens,0)*0.8
    pts=data.frame(x=log10(x),y=y)
  } else {
    design1<-getDesign("Psych")
    ndens<-nDistrDens(nbin,design1)
    ndens<-ndens/max(ndens)
    x<-c(min(nbin),nbin,max(nbin))
    y<-c(0,ndens,0)*0.8
    pts=data.frame(x=log10(x),y=y)
    g<-addG(g,dataPolygon(data=pts,fill=darken(desat(braw.env$plotColours$designC,0.0),off=0.3),linetype="dashed"))
    pts<-data.frame(x=log10(design$sN)+xpts,
                    y=c(0,1,1,0)*0.8)
  }
  g<-addG(g,dataPolygon(data=pts,fill=darken(braw.env$plotColours$designC,off=0.1)))
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
      hypothesis$effect$world$pRplus<-1
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
showWorldSampling<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,
                            showSinglePopulation=NULL,showSingleSample=NULL,HQ=TRUE,totalArea=1,
                            plotArea=c(0,0,1,1),fontScale=1,autoShow=braw.env$autoShow,g=NULL) {

  world<-hypothesis$effect$world
  if (!world$On) 
    world<-list(On=TRUE,
                PDF="Single",
                PDFk=hypothesis$effect$rIV,
                RZ="r",
                pRplus=1)
  
  totalArea<-totalArea*60-evidence$sigOnly*10
  if (world$PDF=="Single") totalArea<-totalArea*0.5
  
  np<-braw.env$worldNPoints
  if(braw.env$RZ=="r")
    rx<-seq(-1,1,length=np)*braw.env$r_range
  else
    rx<-tanh(seq(-1,1,length=np)*braw.env$z_range)

  design1<-design
  design$Replication$On<-FALSE
  
  rdens<-fullRSamplingDist(rx,world,design,sigOnly=evidence$sigOnly,HQ=HQ) 
  rdens0<-fullRSamplingDist(rx,world,design,sigOnly=0,HQ=HQ) 
  if (braw.env$RZ=="z") {
    rdens<-rdens2zdens(rdens,rx)
    rdens0<-rdens2zdens(rdens0,rx)
    rx<-atanh(rx)
  }
  rdens<-rdens/sum(rdens)*totalArea
  rdens0<-rdens0/sum(rdens0)*totalArea
  gn<-median(rdens[rdens>0]/rdens0[rdens>0])
  rdens0<-rdens0*gn
  
  if (design1$Replication$On) {
    dens1<-fullRSamplingDist(rx,world,design1,sigOnly=evidence$sigOnly,HQ=HQ) 
    dens1<-dens1/sum(dens1)
  } else dens1<-NA
  gain<-max(max(rdens),max(dens1),na.rm=TRUE)
  dens1<-dens1/gain
  
  x<-c(rx[1],rx,rx[length(rx)])
  y<-c(0,rdens,0)
  pts=data.frame(x=x,y=y)
  xlim<-c(min(rx),max(rx))
  braw.env$plotArea<-plotArea

  switch(braw.env$RZ,
         "r"={ xticks<-makeTicks(seq(-1,1,0.5));xlabel<-makeLabel(braw.env$rsLabel)},
         "z"={ xticks<-makeTicks(seq(-2,2,1));xlabel<-makeLabel(braw.env$zsLabel)}
  )
  g<-startPlot(xlim=xlim,ylim=c(0,1.05),
               xticks=xticks,xlabel=xlabel,fontScale = fontScale,
               unitGap=0.35,top=TRUE,box="x",g=g)
  
  fill<-braw.env$plotColours$descriptionC
  # if (!is.null(showSingle)) fill<-darken(desat(fill,0.5),off=0)
  g<-addG(g,dataPolygon(data=pts,fill=fill))
  g<-addG(g,dataLine(data=pts))
  
  if (!is.null(showSinglePopulation)) {
    world<-makeWorld(TRUE,"Single","r",showSinglePopulation,pRplus=1)
    if (braw.env$RZ=="r")
      sdens<-fullRSamplingDist(rx,world,design,sigOnly=evidence$sigOnly) 
    else
      sdens<-fullRSamplingDist(tanh(rx),world,design,sigOnly=evidence$sigOnly) 
    
    sdens<-sdens/max(sdens)*approx(rx,rdens0,showSinglePopulation)$y
    pts<-data.frame(x=c(rx[1],rx,rx[length(rx)]),y=c(0,sdens,0))
    g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$descriptionC))
    g<-addG(g,dataLine(data=pts))
  } else {
    sdens<-rdens
  }
  if (!is.null(showSingleSample)) {
    width<-0.01*diff(xlim)
    srx<-showSingleSample+c(-1,-1,1,1)*width
    srdens<-c(0,1,1,0)*approx(rx,sdens,showSingleSample)$y
    
    pts<-data.frame(x=srx,y=srdens)
    g<-addG(g,dataPolygon(pts,fill=braw.env$plotColours$sampleC))
    g<-addG(g,dataLine(pts))
  }
  
  
  if (!is.na(dens1[1])) {
    y<-c(0,dens1,0)
    pts=data.frame(x=x,y=y)
    g<-addG(g,dataPolygon(data=pts,fill=braw.env$plotColours$replicationC,alpha=0.5))
    g<-addG(g,dataLine(data=pts))
    
  }
  return(g)
}

