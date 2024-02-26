
braw.env<-NULL
.onLoad<- function(...) {
  BrawOpts()
}

BrawOpts<-function(BW=FALSE,fontScale=1) {
  braw.env <- new.env(parent = emptyenv())
  
  # genuine globals (for now)
  braw.env$plotDescriptionCols<-c()
  braw.env$CatCatCols<-c()
  braw.env$lastSample<-NULL
  
  ################################
  # graph design
  
  # graph themes
  plotColours<-list(graphC="#BFECFF",graphBack="#999999",
                    maineffectES="#FFCC00",covariationES="#FF1100",interactionES="#0011FF",
                    sampleC="#FFCC00",descriptionC="#FF9955",
                    descriptionC1="#FF5533",descriptionC2="#CCBB33",
                    infer_sigC="#11CC00",infer_nsigC="#FF4400",infer_none="#AAAAAA",
                    infer_sigNonNull="#11CC00",infer_isigNonNull="#881100",infer_nsNonNull="#881100",infer_nsdNonNull="#DDCCCC",
                    infer_sigNull="#118800",infer_isigNull="#FF4400",infer_nsNull="#FF4400",infer_nsdNull="#CCDDCC",
                    psig="#FFAA00",alpha="#44FF22",
                    fdr="#227700",fmr="#BB5555")
  
  if (BW) {
    plotColours<-list(graphC="#FFFFFF",graphBack="#999999",
                      maineffectES="#FFFFFF",covariationES="#FFFFFF",interactionES="#FFFFFF",
                      sampleC="#FFFFFF",descriptionC="#FFFFFF",
                      descriptionC1="#888888",descriptionC2="#111111",
                      infer_sigC="#FFFFFF",infer_nsigC="#111111",infer_none="#AAAAAA",
                      infer_sigNonNull="#FFFFFF",infer_isigNonNull="#555555",infer_nsNonNull="#555555",infer_nsdNonNull="#333333",
                      infer_sigNull="#BBBBBB",infer_isigNull="#111111",infer_nsNull="#FFFFFF",infer_nsdNull="#DDDDDD",
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
          
          braw.env$plotTheme<-mainTheme+SMplotTheme+theme(plot.margin=margin(1.0,1.5,0.5,0.5,"cm"))
          braw.env$diagramTheme<-mainTheme+SMplotTheme+theme(panel.background = element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),panel.spacing=margin(0,0,0,0),plot.margin=margin(0.5,0.5,0.3,0.3,"cm"))
          braw.env$plainDiagramTheme<-mainTheme+SMplotTheme+theme(panel.background = element_rect(fill=plotColours$graphBack, colour=plotColours$graphBack),panel.spacing=margin(0,0,0,0),plot.margin=margin(0.5,0.5,0.3,-0.2,"cm"))
          braw.env$blankTheme<-mainTheme+theme(panel.spacing=margin(0,0,0,0,"cm"),plot.margin=margin(0,0,0,0,"cm"),panel.background = element_rect(fill=plotColours$graphC, colour=plotColours$graphC),
                                               axis.title.x=element_blank(),
                                               axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank(),
                                               axis.title.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.ticks.y=element_blank()
          )
          braw.env$reportTheme<-braw.env$blankTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"),
                                          axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank(),
                                          axis.title.y=element_blank(),
                                          axis.text.y=element_blank(),
                                          axis.ticks.y=element_blank()
          )
          
          braw.env$labelSize<-4*fontScale
          
          braw.env$layout<-layout
          
          ##########################
          # NHST constants
          
          braw.env$alphaSig<-alphaSig
          braw.env$alphaLLR<-0.5*qnorm(1-alphaSig/2)^2
          braw.env$STMethod<-"NHST"
          braw.env$lrRange<-10
          braw.env$anovaMethod<-"F"
          
          #########################
          # display choices
          
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
          
          braw.env$allScatter<-TRUE
          braw.env$showMedians<-FALSE
          braw.env$minN<-10
          braw.env$maxRandN<-5 # times mean sample size
          braw.env$reportGroupMeans<-TRUE
          braw.env$CdoLegendBars<-TRUE
          braw.env$doLegendPoints<-FALSE
          braw.env$simData<-TRUE
          
          braw.env$wPlotScale<-"log10"
          braw.env$pPlotScale<-"log10"
          braw.env$nPlotScale<-"linear"
          
          braw.env$useSignificanceCols<-TRUE
          braw.env$showInteractionOnly<-TRUE
          
          braw.env$includeSingle<-FALSE  # in "All" meta-analysis
          
          braw.env$alphaChar<-'\u03B1'
          
          ##################################
          # notation for worlds
          
          braw.env$rpLabel<-bquote(bold(r[p]))
          braw.env$rsLabel<-bquote(bold(r[s]))
          braw.env$zpLabel<-bquote(bold(z[p]))
          braw.env$zsLabel<-bquote(bold(z[s]))
          
          ###############################
          # notation for world
          #
          
          useLabels<-list(psig="psig",UD="D",P="0")
          
          Pchar<-"P"
          Zchar<-"Z"
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

braw.env<<-braw.env          
}
