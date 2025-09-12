# braw.env<-c()
# braw.def<-c()
# braw.res<-c()
.onLoad<- function(...) {
  BrawOpts()
}



#' @import ggplot2
#' @import ggtext
#' @import grDevices
#' @import grid

#' @import mnormt
#' @import lme4
#' @import readxl
#' @import writexl
#' @import stringr
#' @import clipr
#' @import car
#' @import pracma
#' @import abind
#' @import meta
#' @import lavaan

#' @export
newBrawDev<-function(fontScale=1,height=1000,aspect=1) {
  while (dev.cur()!=1) dev.off()
  if (Sys.info()[['sysname']]=="Darwin")
    quartz("BrawStats",width=height*aspect/144, height=height/144)
  else
    dev.new(width=height*aspect/144, height=height/144, noRStudioGD = TRUE)
  fontScale<-min(dev.size(units="px"))/200/1.75
  assign("labelSize",3.2*fontScale,braw.env)
  setBrawEnv("plotRect",ggplot2::coord_cartesian(xlim=c(0,1),ylim=c(-0.25,1.5)))
  print(startPlot(box="none",backC=braw.env$plotColours$graphC))
}

#' @export
BrawOpts<-function(BW=FALSE,graphC="transparent",fontScale=1,graphicsSize=c(16,10),
                   reportHTML=FALSE, graphicsType="ggplot", reportFontSize=0.85, 
                   colBlind=TRUE,
                   newDev=FALSE,height=400,aspect=1.3,autoShow=FALSE,autoPrint=FALSE,timeLimit=Inf,fullGraphSize=1,
                   npointsMax=1000,
                   maxBins=251,
                   fullOutput=0) {
  if (graphC=="white") graphC<-"#FFFFFF"
  if (graphC=="normal") graphC<-"#BFECFF"
  braw.env <- new.env(parent = emptyenv())
  braw.def <- new.env(parent = emptyenv())
  braw.res <- new.env(parent = emptyenv())
  
  # if (is.null(height)) height<-convertHeight(unit(1, "npc"),"pt",valueOnly = TRUE)
  if (newDev) {
    while (dev.cur()!=1) dev.off()
    dev.new(width=height*aspect/144, height=height/144, noRStudioGD = TRUE)
    fontScale<-min(dev.size(units="px"))/200
  }
  
  braw.env$plotSize<-c(aspect,1)*height
  braw.env$fullGraphSize<-fullGraphSize
  braw.env$reportFontSize<-reportFontSize
  
  # genuine globals (for now)
  braw.env$plotDescriptionCols<-c()
  braw.env$CatCatCols<-c()
  braw.env$lastSample<-NULL
  
  braw.env$plotArea<-c(0,0,1,1) 
  # left, bottom, x-size, y-size
  ################################
  # graph design
  
  sigCol<-"#44EE11"
  nsCol<-"#FF4400"
  
  if (colBlind) {
    # sigCol<-"#4FB"
    nsCol<-"#F82"
    nsCol<-"#FC0"
    nsCol<-"#BBB"
  }
  
  sigNullCol<-darken(sigCol,gain=0.7)
  nsNonNullCol<-darken(nsCol,gain=0.7)
  isigNullCol<-darken(sigCol,off=0.5)
  isigNonNullCol<-darken(nsCol,off=0.5)
  # graph themes
  plotColours<-list(graphC=graphC,graphBack="#999999",
                    variableC="#FFCC00",maineffectES="#DD8844",covariationES="#FF1100",interactionES="#0011FF",
                    populationC="#FFCC00",sampleC="#FFCC00",descriptionC="#DD8844",
                    designC="#CC4422",replicationC="#FF7755",
                    descriptionC1="#FF5533",descriptionC2="#CCBB33",
                    descriptionTotal=darken(desat("#DD8844",0.1),0.7),descriptionsUnique=darken(desat("#DD8844",0.1),1.3),
                    metaAnalysis="#FFEE00",metaMultiple="#FF8800",metaAnalysisTheory="#FFFFFF",
                    infer_sigC=sigCol,infer_nsigC=nsCol,infer_none="#AAAAAA",infer_miss=NULL,
                    infer_sigNonNull=sigCol,infer_nsigNonNull=nsNonNullCol,infer_isigNonNull=isigNonNullCol,infer_nsdNonNull="#DDCCCC",
                    infer_sigNull=sigNullCol,infer_nsigNull=nsCol,infer_isigNull=isigNullCol,infer_nsdNull="#CCDDCC",
                    psig="#FFAA00",alpha="#44FF22",
                    fdr="#227700",fmr="#BB5555",
                    powerPopulation="#0049FF",powerSample="#88BDFF")

  if (BW) {
    plotColours<-list(graphC="#FFFFFF",graphBack="#999999",
                      maineffectES="#FFFFFF",covariationES="#FFFFFF",interactionES="#FFFFFF",
                      populationC="#FFFFFF",sampleC="#FFFFFF",descriptionC="#FFFFFF",
                      designC="#444444",replicationC="#777777",
                      descriptionC1="#888888",descriptionC2="#111111",
                      descriptionTotal=darken(desat("#FFFFFF",0.1),0.7),descriptionsUnique=darken(desat("#DD8844",0.1),1.3),
                      metaAnalysis="#FFFFFF",metaAnalysisTheory="#FFFFFF",
                      infer_sigC="#FFFFFF",infer_nsigC="#111111",infer_none="#AAAAAA",
                      infer_sigNonNull="#FFFFFF",infer_isigNonNull="#555555",infer_nsigNonNull="#555555",infer_nsdNonNull="#333333",
                      infer_sigNull="#BBBBBB",infer_isigNull="#111111",infer_nsigNull="#FFFFFF",infer_nsdNull="#DDDDDD",
                      psig="#FFFFFF",alpha="#FFFFFF",
                      fdr="#BBBBBB",fmr="#555555")
  }
  
  mainTheme<-ggplot2::theme(panel.background = ggplot2::element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),
                   panel.grid.major = ggplot2::element_line(linetype="blank"),panel.grid.minor = ggplot2::element_line(linetype="blank"),
                   plot.background = ggplot2::element_rect(fill=plotColours$graphC, colour=plotColours$graphC))
  SMplotTheme<-ggplot2::theme(plot.title=ggplot2::element_text(size=14,face="bold"),axis.title=ggplot2::element_text(size=16,face="bold"),
                     axis.text.x=ggplot2::element_text(size=12),axis.text.y=ggplot2::element_text(size=12))
  LGplotTheme<-ggplot2::theme(plot.title=ggplot2::element_text(size=21,face="bold"),axis.title=ggplot2::element_text(size=24,face="bold"),
                     axis.text.x=ggplot2::element_text(size=18),axis.text.y=ggplot2::element_text(size=18))
  
  
  alphaSig<-0.05
  
  #################################
  
          braw.env$plotColours<-plotColours
          braw.env$plotShapes<-list(data=21,study=22,parameter=21,meta=24)
          
          # braw.env$plotTheme<-mainTheme+SMplotTheme+theme(plot.margin=margin(1.0,1.5,0.5,0.5,"cm"))
          # braw.env$diagramTheme<-mainTheme+SMplotTheme+theme(panel.background = ggplot2::element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),
          #                                                    panel.spacing=margin(0,0,0,0),plot.margin=margin(0.5,0.5,0.3,0.3,"cm"))
          # braw.env$plainDiagramTheme<-mainTheme+SMplotTheme+theme(panel.background = ggplot2::element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),
          #                                                         panel.spacing=margin(0,0,0,0),plot.margin=margin(0.5,0.5,0.3,-0.2,"cm"))
  braw.env$blankTheme<-function() {
    ggplot2::theme(panel.background = ggplot2::element_rect(fill=plotColours$graphC, colour=plotColours$graphC),
          panel.spacing=margin(0,0,0,0,"cm"),plot.margin=margin(-0.2,-0.4,-0.2,-0.5,"cm"),
          panel.grid.major = ggplot2::element_line(linetype="blank"),panel.grid.minor = ggplot2::element_line(linetype="blank"),
          legend.position = "none",
                    plot.background = ggplot2::element_rect(fill=plotColours$graphC, colour=plotColours$graphC),
                         axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.y=element_blank()
          )
  }
          # braw.env$reportTheme<-braw.env$blankTheme()+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))
          
          braw.env$graphicsSize<-graphicsSize
          braw.env$labelSize<-height/100*fontScale
          braw.env$dotSize<-braw.env$labelSize*1.25
          braw.env$fontSize<-1
          
          braw.env$autoShow<-autoShow
          braw.env$autoPrint<-autoPrint
          braw.env$plotRect<-ggplot2::coord_cartesian(xlim=c(0,1),ylim=c(0,1))
          
          braw.env$addHistory<-TRUE
          braw.env$plotLimits<-c()
          
          ##########################
          # NHST constants
          
          braw.env$alphaSig<-alphaSig
          braw.env$alphaLLR<-0.5*qnorm(1-alphaSig/2)^2
          braw.env$STMethod<-"NHST"
          braw.env$lrRange<-10
          braw.env$anovaMethod<-"F"
          braw.env$modelType<-"Norm"
          
          #########################
          # display choices
          
          braw.env$reportHTML<-reportHTML
          braw.env$graphicsType<-graphicsType
          braw.env$newSampleDisplay<-TRUE
          
          braw.env$report_precision<-3
          braw.env$graph_precision<-2
          
          braw.env$RZ<-"r"
          
          braw.env$r_range<-0.95
          braw.env$z_range<-atanh(braw.env$r_range)
          braw.env$d_range<-5
          braw.env$w_range<-c(0.05,1)
          braw.env$npoints<-101
          braw.env$fullRange<-3
          braw.env$nNpoints<-201
          braw.env$worldNPoints<-201
          braw.env$varNPoints<-201
          braw.env$nscaleLog<-FALSE
          braw.env$maxnPlot<-200
          
          braw.env$maxBins<-maxBins
          
          braw.env$min_p<-0.0001
          braw.env$truncate_p<-FALSE
          braw.env$min_nw<-10
          braw.env$max_nw<-10000
          
          braw.env$npointsMax<-npointsMax
          
          braw.env$dist_zi<-0.05
          braw.env$dist_range<-4
          
          braw.env$allScatter<-TRUE
          braw.env$showMedians<-FALSE
          braw.env$minN<-10
          braw.env$maxN<-2000
          braw.env$maxRandN<-5 # times mean sample size
          braw.env$reportGroupMeans<-TRUE
          braw.env$doLegendPoints<-FALSE
          braw.env$onesided<-TRUE
          braw.env$simData<-TRUE
          
          braw.env$wPlotScale<-"linear"
          braw.env$pPlotScale<-"log10"
          braw.env$nPlotScale<-"log10"
          
          braw.env$CatPlotOffset<-0.0
          braw.env$errorBarGain<-2
          
          braw.env$useSignificanceCols<-TRUE

          braw.env$includeSingle<-FALSE  # in "All" meta-analysis
          braw.env$includeGamma<-FALSE
          braw.env$includeGenExp<-FALSE
          
          braw.env$alphaChar<-'&#x03B1'
          
          braw.env$showTheoryHQ<-TRUE
          braw.env$showTheory<-TRUE
          
          braw.env$fullOutput<-fullOutput
          braw.env$reportCounts<-FALSE

          ##################################
          # default variables
          
          braw.env$variables<-makeDefaultVariables()
          
          braw.env$when_string <- "="
          braw.env$interaction_string <- "&#x00D7"
          
          
          ##################################
          # notation for effect sizes
          
          braw.env$rpLabel<-'r[p]'
          braw.env$rsLabel<-'r[s]'
          braw.env$zpLabel<-'z[p]'
          braw.env$zsLabel<-'z[s]'
          braw.env$dpLabel<-'d[p]'
          braw.env$dsLabel<-'d[s]'
          
          ###############################
          # notation for world
          #
          
          useLabels<-list(hypLabel="R",psig="psig",UD="D",P="0")
          
          Pchar<-"\u03c6"
          HypChar<-useLabels$hypLabel
          Lchar<-'\u03bb'
          
          switch(useLabels$psig,
                 "psig"={pSigLabel<-"p[sig]"},
                 "w"=   {pSigLabel<-"w"}
          )
          
          posChar<-"+"
          negChar<-"n"
          switch(useLabels$P,
                 "+"={
                   nullChar<-"0"
                   Ptypechar<-posChar 
                 },
                 "0"={
                   nullChar<-"0"
                   Ptypechar<-nullChar
                 },
                 "-"={
                   nullChar<-'\u2013'
                   Ptypechar<-nullChar
                 }
          )
          
          Ltypechar<-posChar
          
          switch (useLabels$UD, 
                  "U"= {
                    Plabel<-paste0(Pchar,"^",Ptypechar)
                    Llabel<-paste0(Lchar,"^",Ltypechar)
                    
                    nullSig<-paste0(HypChar,"^",nullChar,'(sig)')  # "Z+ +ve"
                    nullPositive<-paste0(HypChar,"^",nullChar,'(+sig)')  # "Z+ +ve"
                    nullNS<-paste0(HypChar,"^",nullChar,'(ns) ')  # "Z+ -ve"
                    nullNegative<-paste0(HypChar,"^",nullChar,'(-sig)')  # "Z+ -ve"
                    nonNullSig<-paste0(HypChar,"^",posChar,'(sig)')  # "Z+ +ve"
                    nonNullPositive<-paste0(HypChar,"^",posChar,'(+sig)')  # "Z+ +ve"
                    nonNullNS<-paste0(HypChar,"^",posChar,'(ns) ')  # "Z+ -ve"
                    nonNullNegative<-paste0(HypChar,"^",posChar,'(-sig)')  # "Z+ -ve"
                  },
                  "D"= {
                    braw.env$nonNull<-paste0(HypChar,"[",posChar,"]")
                    braw.env$Null<-paste0(HypChar,"[",nullChar,"]")
                    braw.env$Inactive<-paste0(HypChar,"[",negChar,"]")
                    # braw.env$nonNull<-paste0(HypChar,posChar)
                    # braw.env$Null<-paste0(HypChar,nullChar)
                    
                    Plabel<-paste0(Pchar,"[",Ptypechar,"]")
                    Llabel<-paste0(Lchar,"[",Ltypechar,"]")

                    nullSig<-paste0(braw.env$Null,'(sig)')  # "Z+ +ve"
                    nullPositive<-paste0(braw.env$Null,'(+sig)')  # "Z+ +ve"
                    nullNS<-paste0(braw.env$Null,'(ns) ')  # "Z+ -ve"
                    nullNegative<-paste0(braw.env$Null,'(-sig)')  # "Z+ -ve"
                    nonNullSig<-paste0(braw.env$nonNull,'(sig)')  # "Z+ +ve"
                    nonNullPositive<-paste0(braw.env$nonNull,'(+sig)')  # "Z+ +ve"
                    nonNullNS<-paste0(braw.env$nonNull,'(ns) ')  # "Z+ -ve"
                    nonNullNegative<-paste0(braw.env$nonNull,'(-sig)')  # "Z+ -ve"
                  }
          )
          
          braw.env$Pchar<-Pchar 
          braw.env$HypChar<-HypChar
          braw.env$Lchar<-Lchar
          
          braw.env$pSigLabel<-pSigLabel
          braw.env$LabelUD<-useLabels$UD
          braw.env$pPlusLabel<-paste0("P(",Ptypechar,")")
          
          braw.env$posChar<-"+"
          braw.env$nullChar<-nullChar
          
          braw.env$Plabel<-Plabel
          braw.env$Llabel<-Llabel

          braw.env$nonNullSig<-nonNullSig
          braw.env$nonNullPositive<-nonNullPositive
          braw.env$nonNullNS<-nonNullNS
          braw.env$nonNullNegative<-nonNullNegative
          braw.env$nullSig<-nullSig
          braw.env$nullPositive<-nullPositive
          braw.env$nullNS<-nullNS
          braw.env$nullNegative<-nullNegative
          braw.env$nonNullSig<-paste0(HypChar,"[",posChar,"]~sig")
          braw.env$nonNullPositive<-nonNullPositive
          braw.env$nonNullNS<-paste0(HypChar,"[",posChar,"]~ns")
          braw.env$nonNullNegative<-nonNullNegative
          braw.env$nullSig<-paste0(HypChar,"[",nullChar,"]~sig")
          braw.env$inactiveSig<-paste0(HypChar,"[n]~sig")
          braw.env$nullPositive<-nullPositive
          braw.env$nullNS<-paste0(HypChar,"[",nullChar,"]~ns")
          braw.env$inactiveNS<-paste0(HypChar,"[n]~ns")
          braw.env$nullNegative<-nullNegative
          
          braw.env$allPositive<-paste0(HypChar,"+ve")
          braw.env$allNegative<-paste0(HypChar,"ns")
          
          braw.env$activeTitle<-"Active"
          braw.env$inactiveTitle<-"Inactive"
          
          braw.env$nonnullTitle<-"Non Null"
          braw.env$nullTitle<-"Null"
          braw.env$nonnullTitle<-paste0(HypChar,"[",posChar,"]")
          braw.env$nullTitle<-paste0(HypChar,"[",nullChar,"]")
          
          braw.env$timeLimit<-timeLimit # seconds
          

braw.res$result<-NULL 
braw.res$multiple<-NULL 
braw.res$explore<-NULL 
braw.res$metaSingle<-NULL
braw.res$metaMultiple<-NULL
braw.res$possibleResult<-NULL


braw.def$IV<-makeVariable("IV")
braw.def$IV2<-NULL
braw.def$DV<-makeVariable("DV",cases="E1,E2")
braw.def$hypothesis<-makeHypothesis(IV=braw.def$IV,IV2=braw.def$IV2,DV=braw.def$DV,effect=makeEffect())
braw.def$design<-makeDesign()
braw.def$evidence<-makeEvidence()
braw.def$metaAnalysis<-makeMetaAnalysis()
braw.def$explore<-makeExplore()

braw.def$defaultHypothesis<-braw.def$hypothesis
braw.def$defaultDesign<-braw.def$design
braw.def$defaultEvidence<-braw.def$evidence
braw.def$defaultMetaAnalysis<-braw.def$metaAnalysis
braw.def$defaultExplore<-braw.def$explore

# braw.def$possible<-makePossible(targetSample=0.3,sims=NULL,
#                                 hypothesis=braw.def$hypothesis,design=braw.def$design)
braw.def$possible<-NULL

braw.def<<-braw.def
braw.env<<-braw.env          
braw.res<<-braw.res

}


#' @export
setBrawOpts<-function(BW=NULL,graphC=NULL,fontScale=NULL,reportFontSize=NULL,
                      graphicsType=NULL,fullGraphSize=NULL,
                      reportHTML=NULL, fullOutput=NULL, 
                      npointsMax=NULL,timeLimit=NULL,
                      autoShow=NULL,autoPrint=NULL
) {
  
  if(!is.null(BW)) setBrawEnv("BW",BW)
  if(!is.null(graphC)) setBrawEnv("graphC",graphC)
  if(!is.null(fontScale)) {
    height<-braw.env$plotSize[2]
    setBrawEnv("labelSize",height/100*fontScale)
    setBrawEnv("dotSize",braw.env$labelSize*1.25)
  }
  if(!is.null(graphicsType)) setBrawEnv("graphicsType",graphicsType)
  if(!is.null(reportHTML)) setBrawEnv("reportHTML",reportHTML)
  if(!is.null(reportFontSize)) setBrawEnv("reportFontSize",reportFontSize)
  if(!is.null(timeLimit)) setBrawEnv("timeLimit",timeLimit)
  if(!is.null(fullOutput)) setBrawEnv("fullOutput",fullOutput)
  if(!is.null(autoShow)) setBrawEnv("autoShow",autoShow)
  if(!is.null(autoPrint)) setBrawEnv("autoPrint",autoPrint)
  if(!is.null(fullGraphSize)) setBrawEnv("fullGraphSize",fullGraphSize)
  if(!is.null(npointsMax)) setBrawEnv("npointsMax",npointsMax)
  
}

#' @export
setBrawEnv<-function(which,value) {
  assign(which,value,braw.env)
}

#' @export
setBrawRes<-function(which,value) {
  assign(which,value,braw.res)
}

#' @export
setBrawDef<-function(which,value) {
  assign(which,value,braw.def)
  if (is.element(which,c("world"))) {
    assign("effect",makeEffect(),braw.def)
  }
  if (is.element(which,c("world","effect","IV","IV2","DV"))) {
    assign("hypothesis",makeHypothesis(),braw.def)
  }
}

setHTML<-function() {
  setBrawEnv("graphicsType","HTML")
  setBrawEnv("reportHTML",TRUE)
}
setggplot<-function() setBrawEnv("graphicsType","ggplot")
