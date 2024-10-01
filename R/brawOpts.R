# braw.env<-c()
# braw.def<-c()
# braw.res<-c()
# .onLoad<- function(...) {
#   BrawOpts()
# }

newBrawDev<-function(fontScale=1,height=1000,aspect=1) {
  while (dev.cur()!=1) dev.off()
  if (Sys.info()[['sysname']]=="Darwin")
    quartz("BrawStats",width=height*aspect/144, height=height/144)
  else
    dev.new(width=height*aspect/144, height=height/144, noRStudioGD = TRUE)
  fontScale<-min(dev.size(units="px"))/200/1.75
  assign("labelSize",3.2*fontScale,braw.env)
  setBrawEnv("plotRect",coord_cartesian(xlim=c(0,1),ylim=c(-0.25,1.5)))
  print(startPlot(box="none",backC=braw.env$plotColours$graphC))
}

BrawOpts<-function(BW=FALSE,graphC="normal",fontScale=1,graphicsSize=c(16,10),
                   reportHTML=FALSE,
                   newDev=FALSE,height=576,aspect=1.736,timeLimit=Inf,
                   reducedOutput=FALSE) {
  if (graphC=="white") graphC<-"#FFFFFF"
  if (graphC=="normal") graphC<-"#BFECFF"
  braw.env <- new.env(parent = emptyenv())
  braw.def <- new.env(parent = emptyenv())
  braw.res <- new.env(parent = emptyenv())
  
  if (newDev) {
    while (dev.cur()!=1) dev.off()
    dev.new(width=height*aspect/144, height=height/144, noRStudioGD = TRUE)
    fontScale<-min(dev.size(units="px"))/200
  }
  # genuine globals (for now)
  braw.env$plotDescriptionCols<-c()
  braw.env$CatCatCols<-c()
  braw.env$lastSample<-NULL
  
  braw.env$plotArea<-c(0,0,1,1) 
  # left, bottom, x-size, y-size
  ################################
  # graph design
  
  # graph themes
  plotColours<-list(graphC=graphC,graphBack="#999999",
                    maineffectES="#FFCC00",covariationES="#FF1100",interactionES="#0011FF",
                    sampleC="#FFCC00",descriptionC="#FF9955",replicationC="#FF7755",
                    descriptionC1="#FF5533",descriptionC2="#CCBB33",
                    infer_sigC="#11CC00",infer_nsigC="#FF4400",infer_none="#AAAAAA",infer_miss=NULL,
                    infer_sigNonNull="#11CC00",infer_isigNonNull="#881100",infer_nsigNonNull="#881100",infer_siglsNonNull="#AABB99",infer_nsdNonNull="#DDCCCC",
                    infer_sigNull="#118800",infer_isigNull="#FF4400",infer_nsigNull="#FF4400",infer_nsiglsNull="#CC9988",infer_nsdNull="#CCDDCC",
                    psig="#FFAA00",alpha="#44FF22",
                    fdr="#227700",fmr="#BB5555")

  if (BW) {
    plotColours<-list(graphC="#FFFFFF",graphBack="#999999",
                      maineffectES="#FFFFFF",covariationES="#FFFFFF",interactionES="#FFFFFF",
                      sampleC="#FFFFFF",descriptionC="#FFFFFF",
                      descriptionC1="#888888",descriptionC2="#111111",
                      infer_sigC="#FFFFFF",infer_nsigC="#111111",infer_none="#AAAAAA",
                      infer_sigNonNull="#FFFFFF",infer_isigNonNull="#555555",infer_nsigNonNull="#555555",infer_nsdNonNull="#333333",
                      infer_sigNull="#BBBBBB",infer_isigNull="#111111",infer_nsigNull="#FFFFFF",infer_nsdNull="#DDDDDD",
                      psig="#FFFFFF",alpha="#FFFFFF",
                      fdr="#BBBBBB",fmr="#555555")
  }
  
  mainTheme<-theme(panel.background = element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),
                   panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                   plot.background = element_rect(fill=plotColours$graphC, colour=plotColours$graphC))
  SMplotTheme<-theme(plot.title=element_text(size=14,face="bold"),axis.title=element_text(size=16,face="bold"),
                     axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
  LGplotTheme<-theme(plot.title=element_text(size=21,face="bold"),axis.title=element_text(size=24,face="bold"),
                     axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))
  
  
  alphaSig<-0.05
  
  #################################
  
          braw.env$plotColours<-plotColours
          braw.env$plotShapes<-list(data=21,study=22,parameter=21,meta=24)
          
          # braw.env$plotTheme<-mainTheme+SMplotTheme+theme(plot.margin=margin(1.0,1.5,0.5,0.5,"cm"))
          # braw.env$diagramTheme<-mainTheme+SMplotTheme+theme(panel.background = element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),
          #                                                    panel.spacing=margin(0,0,0,0),plot.margin=margin(0.5,0.5,0.3,0.3,"cm"))
          # braw.env$plainDiagramTheme<-mainTheme+SMplotTheme+theme(panel.background = element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),
          #                                                         panel.spacing=margin(0,0,0,0),plot.margin=margin(0.5,0.5,0.3,-0.2,"cm"))
  braw.env$blankTheme<-function() {
    theme(panel.background = element_rect(fill=plotColours$graphC, colour=plotColours$graphC),
          panel.spacing=margin(0,0,0,0,"cm"),plot.margin=margin(-0.2,-0.4,-0.2,-0.5,"cm"),
          panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                    plot.background = element_rect(fill=plotColours$graphC, colour=plotColours$graphC),
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
          braw.env$labelSize<-3.2*fontScale
          braw.env$dotSize<-3.2*2
          
          braw.env$autoShow<-FALSE
          braw.env$plotRect<-coord_cartesian(xlim=c(0,1),ylim=c(0,1))
          
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
          braw.env$newSampleDisplay<-TRUE
          
          braw.env$report_precision<-3
          braw.env$graph_precision<-2
          
          braw.env$RZ<-"r"
          
          braw.env$z_range<-1.5
          braw.env$r_range<-0.99
          braw.env$w_range<-c(0.05,1)
          braw.env$fullRange<-3
          braw.env$nNpoints<-201
          braw.env$worldNPoints<-201
          braw.env$varNPoints<-201
          braw.env$nscaleLog<-FALSE
          braw.env$maxnPlot<-200
          
          braw.env$min_p<-0.0001
          braw.env$truncate_p<-FALSE
          braw.env$min_nw<-10
          braw.env$max_nw<-10000
          
          braw.env$dist_zi<-0.05
          braw.env$dist_range<-4
          
          braw.env$allScatter<-TRUE
          braw.env$showMedians<-FALSE
          braw.env$minN<-10
          braw.env$maxN<-250
          braw.env$maxRandN<-5 # times mean sample size
          braw.env$reportGroupMeans<-TRUE
          braw.env$doLegendPoints<-FALSE
          braw.env$onesided<-TRUE
          braw.env$simData<-TRUE
          
          braw.env$wPlotScale<-"linear"
          braw.env$pPlotScale<-"log10"
          braw.env$nPlotScale<-"log10"
          
          braw.env$useSignificanceCols<-TRUE

          braw.env$includeSingle<-TRUE  # in "All" meta-analysis
          
          braw.env$alphaChar<-'\u03B1'
          
          braw.env$showTheoryHQ<-TRUE
          braw.env$showTheory<-TRUE
          
          braw.env$reducedOutput<-reducedOutput
          
          ##################################
          # default variables
          
          braw.env$variables<-makeDefaultVariables()
          
          braw.env$when_string <- "="
          braw.env$interaction_string <- "\u2217"
          
          
          ##################################
          # notation for effect sizes
          
          braw.env$rpLabel<-bquote(bold(r[p]))
          braw.env$rsLabel<-bquote(bold(r[s]))
          braw.env$zpLabel<-bquote(bold(z[p]))
          braw.env$zsLabel<-bquote(bold(z[s]))
          
          ###############################
          # notation for world
          #
          
          useLabels<-list(hypLabel="H",psig="psig",UD="D",P="0")
          
          Pchar<-"P"
          Zchar<-useLabels$hypLabel
          Lchar<-'\u03BB'
          
          switch(useLabels$psig,
                 "psig"={pSigLabel<-bquote(bold(p[.('sig')]))},
                 "w"={pSigLabel<-bquote(bold(w))}
          )
          
          posChar<-"+"
          switch(useLabels$P,
                 "+"={
                   nullChar<-"0"
                   pPlus<-TRUE
                   Ptypechar<-posChar 
                 },
                 "0"={
                   nullChar<-"0"
                   pPlus<-FALSE
                   Ptypechar<-nullChar
                 },
                 "-"={
                   nullChar<-'\u2013'
                   pPlus<-FALSE
                   Ptypechar<-nullChar
                 }
          )
          
          Ltypechar<-posChar
          
          switch (useLabels$UD, 
                  "U"= {
                    Plabel<-bquote(bold(.(Pchar)^.(Ptypechar)))
                    Llabel<-bquote(bold(.(Lchar)^.(Ltypechar)))
                    
                    nonNullPositive<-bquote(.(Zchar)^.(posChar)~'+sig')  # "Z+ +ve"
                    nonNullNS<-bquote(.(Zchar)^.(posChar) ~"ns")  # "Z+ -ve"
                    nonNullNegative<-bquote(.(Zchar)^.(posChar) ~"-sig")  # "Z+ -ve"
                    nullPositive<-bquote(.(Zchar)^.(nullChar) ~"+sig")   # "Z0 +ve"
                    nullNS<-bquote(.(Zchar)^.(nullChar) ~"ns")  # "Z0 -ve"
                    nullNegative<-bquote(.(Zchar)^.(nullChar) ~"-sig")  # "Z0 -ve"
                  },
                  "D"= {
                    Plabel<-bquote(bold(.(Pchar)[.(Ptypechar)]))
                    Llabel<-bquote(bold(.(Lchar)[.(Ltypechar)]))
                    
                    nonNullPositive<-bquote(.(Zchar)[.(posChar)] ~"+sig")  # "Z+ +ve"
                    nonNullNS<-bquote(.(Zchar)[.(posChar)] ~"ns")  # "Z+ -ve"
                    nonNullNegative<-bquote(.(Zchar)[.(posChar)] ~"-sig")  # "Z+ -ve"
                    nullPositive<-bquote(.(Zchar)[.(nullChar)] ~"+sig")   # "Z0 +ve"
                    nullNS<-bquote(.(Zchar)[.(nullChar)] ~"ns")  # "Z0 -ve"
                    nullNegative<-bquote(.(Zchar)[.(nullChar)] ~"-sig")  # "Z0 -ve"
                  }
          )
          
          braw.env$pPlus<-pPlus 
          braw.env$Pchar<-Pchar 
          braw.env$Zchar<-Zchar
          braw.env$Lchar<-Lchar
          
          braw.env$pSigLabel<-pSigLabel
          braw.env$LabelUD<-useLabels$UD
          braw.env$pPlusLabel<-paste0("P(",Ptypechar,")")
          
          braw.env$posChar<-"+"
          braw.env$nullChar<-"0"
          
          braw.env$Plabel<-Plabel
          braw.env$Llabel<-Llabel
          
          braw.env$nonNullPositive<-nonNullPositive
          braw.env$nonNullNS<-nonNullNS
          braw.env$nonNullNegative<-nonNullNegative
          braw.env$nullPositive<-nullPositive
          braw.env$nullNS<-nullNS
          braw.env$nullNegative<-nullNegative
          
          braw.env$allPositive<-bquote(.(Zchar) ~"+ve")
          braw.env$allNegative<-bquote(.(Zchar) ~"ns")

          braw.env$timeLimit<-timeLimit # seconds
          
braw.env<<-braw.env          

braw.def$IV<-makeVariable("IV")
braw.def$IV2<-NULL
braw.def$DV<-makeVariable("DV",cases="E1,E2")
braw.def$world<-makeWorld()
braw.def$effect<-makeEffect(world=braw.def$world)
braw.def$hypothesis<-makeHypothesis(IV=braw.def$IV,IV2=braw.def$IV2,DV=braw.def$DV,effect=braw.def$effect)
braw.def$design<-makeDesign()
braw.def$evidence<-makeEvidence()
braw.def$metaAnalysis<-makeMetaAnalysis()
braw.def$explore<-makeExplore()
braw.def$possible<-makePossible(targetSample=0.3,
                                hypothesis=braw.def$hypothesis,design=braw.def$design)

braw.def<<-braw.def

braw.res$result<-NULL 
braw.res$expected<-NULL 
braw.res$explore<-NULL 
braw.res$metaResult<-NULL
braw.res$metaMultiple<-NULL
braw.res$possibleResult<-NULL

braw.res<<-braw.res
}

setBrawEnv<-function(which,value) {
  assign(which,value,braw.env)
}

setBrawRes<-function(which,value) {
  assign(which,value,braw.res)
}

setBrawDef<-function(which,value) {
  assign(which,value,braw.def)
  if (is.element(which,c("world"))) {
    assign("effect",makeEffect(),braw.def)
  }
  if (is.element(which,c("world","effect","IV","IV2","DV"))) {
    assign("hypothesis",makeHypothesis(),braw.def)
  }
}
