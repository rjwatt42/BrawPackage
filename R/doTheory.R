
#' @export
doTheory<-function(doingTheory=NULL,showOutput=TRUE,showJamovi=TRUE,showHelp=FALSE,
                   showPlanOnly=FALSE,doHistory=TRUE,
                   IVtype="Interval",IV2type=NULL,DVtype="Interval",
                   DVmean=0,DVsd=1,DVskew=0,DVkurtosis=0,
                   residuals="normal",
                   rIV=0.3,rIV2=NULL,rIVIV2=NULL,rIVIV2DV=NULL,
                   heteroscedasticity=0,
                   sN=42,sMethod="Random",sDataFormat="long",
                   sOutliers=0, sDependence=0,
                   sIV1Use="Between",sIV2Use="Between",
                   analyse="Main1", 
                   allScatter=NULL,fullWithinNames=NULL,
                   nreps=200
) {
  
  if (is.logical(analyse) && length(analyse)<4) analyse<-c(analyse,rep(FALSE,4-length(analyse)))
  if (is.logical(sOutliers) && sOutliers) sOutliers<-0.1
  if (is.logical(sDependence) && sDependence) sDependence<-0.25
  
  oldHypothesis<-braw.def$hypothesis
  oldDesign<-braw.def$design
  oldEvidence<-braw.def$evidence
  oldAllScatter<-braw.env$allScatter
  setHTML()
  
  showNow<-"None"
  
  if (!is.null(doingTheory)) {
    stepBS<-stepBS(doingTheory)
    partBS<-partBS(doingTheory)
    if (singleBS(doingTheory)) process<-"single" else process<-"multiple"
    rootBS<-paste0("Step",stepBS,partBS)
    
    IV<-makeVariable("IV",IVtype)
    if (!is.null(IV2type)) IV2<-makeVariable("IV2",IV2type) else IV2<-NULL
    DV<-makeVariable("DV",DVtype,mu=DVmean,sd=DVsd,skew=DVskew,kurtosis=DVkurtosis)
    variables=list(IV=IV,IV2=IV2,DV=DV)
    world<-makeWorld(FALSE)
    
    marginalsStyle<-"all"
    hideReport<-FALSE
    makeData<-TRUE
    whichEffect="Main1"
    
    switch(stepBS,
           "0"={
             showNow<-"Plan"
           },
           "1"={ # sampling error
             switch(partBS,
                    "A"={showNow<-"Mean"},
                    "B"={showNow<-"Kurt"},
                    "C"={showNow<-"Effect"}
             )
           },
           "2"={ # NHST
             if (is.null(rIV)) rIV<-0.3
             world<-makeWorld(TRUE,"Single","r",rIV)
             switch(partBS,
                    "A"={world$pRplus<-1},
                    "B"={world$pRplus<-0},
                    "C"={world$pRplus<-0.5},
                    {}
             )
             showNow<-"NHST"
           },
           "3"={ # 
             if (is.null(rIV)) rIV<-0.3
             world<-makeWorld(TRUE,"Single","r",rIV,pRplus=0.5)
             showNow<-"NHST"
           },
           "4"={ # 
           },
           "5"={ # 
           },
           "9"={ # Moderation
             variables$IV<-"Anxiety"
             variables$IV2<-"Smoker?"
             variables$DV<-"ExamGrade"
             if (is.null(rIVIV2DV)) rIVIV2DV <- -0.3
             if (is.null(rIV2)) rIV2 <- 0
             rIVIV2 <- 0
             switch(partBS,
                    "A"={if (is.null(rIV)) rIV<-0},
                    "B"={if (is.null(rIV)) rIV<- -rIVIV2DV},
                    "C"={
                      variables$IV<-"Sessions"
                      variables$IV2<-"Smoker?"
                      variables$DV<-"Happiness"
                      rIV<- -rIVIV2DV
                      rIV2 <- rIVIV2DV+sign(rIVIV2DV)*(1-abs(rIVIV2DV))/2
                    }
             )
             if (is.null(sN)) sN<-500
             analyse<-"Main1x2"
             showNow<-"Effect"
           },
           "10"={ # Mediation
             variables$IV<-"Anxiety"
             variables$IV2<-"HoursSleep"
             variables$DV<-"ExamGrade"
             if (is.null(rIVIV2DV)) rIVIV2DV<-0
             switch(partBS,
                    "A"={ # full mediation
                      if (is.null(rIV)) rIV<-0
                      if (is.null(rIVIV2)) rIVIV2<-0.6
                      if (is.null(rIV2)) rIV2<-0.6
                    },
                    "B"={ # no mediation
                      if (is.null(rIV)) rIV<-0.36
                      if (is.null(rIVIV2)) rIVIV2<-0
                      if (is.null(rIV2)) rIV2<-0.6
                    },
                    "C"={ # partial mediation
                      if (is.null(rIV)) rIV<-0.18
                      if (is.null(rIVIV2)) rIVIV2<-0.3
                      if (is.null(rIV2)) rIV2<-0.6
                    }
             )
             if (is.null(sN)) sN<-500
             analyse<-"Covariation"
             showNow<-"SchematicSEM"
           }
    )
    
    if (is.character(analyse))
      switch(analyse,
             "Main1"={analyse<-c(TRUE,FALSE,FALSE,FALSE)},
             "Main2"={analyse<-c(FALSE,TRUE,FALSE,FALSE)},
             "Main12"={analyse<-c(TRUE,TRUE,FALSE,FALSE)},
             "Main1x2"={analyse<-c(TRUE,TRUE,TRUE,FALSE)},
             "InteractionOnly"={analyse<-c(FALSE,FALSE,TRUE,FALSE)},
             "Covariation"={analyse<-c(TRUE,TRUE,FALSE,TRUE)}
      )
    setEvidence(AnalysisTerms=analyse)
    
    hypothesis<-makeHypothesis(IV=variables$IV,IV2=variables$IV2,DV=variables$DV,
                               effect=makeEffect(rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV,
                                                 world=world,
                                                 Heteroscedasticity=heteroscedasticity,ResidDistr=residuals
                                                 )
    )
    hypothesis$layout<-"simple"
    if (stepBS=="6") hypothesis$layout<-"noInteraction"
    if (stepBS=="9") hypothesis$layout<-"moderation"
    if (stepBS=="10") hypothesis$layout<-"mediation"
    
    design<-makeDesign(sN=sN,sMethod=makeSampling(sMethod),sDataFormat=sDataFormat,
                       sOutliers=sOutliers, sDependence=sDependence,
                       sIV1Use=sIV1Use,sIV2Use=sIV2Use)
    setBrawDef("hypothesis",hypothesis)
    setBrawDef("design",design)
    
    if (makeData) {
      if (process=="single") {
        setBrawRes("result",NULL)
        doSingle()
      } 
      if (process=="analysis") {
        doAnalysis(sample=braw.res$result)
      }      
      if (process=="multiple") {
        doMultiple(nreps)
      }      
    }
    
    if(!is.null(allScatter)) setBrawEnv("allScatter",allScatter)
    if(!is.null(fullWithinNames)) setBrawEnv("fullWithinNames",fullWithinNames)
    # display the results
    svgBox(height=350,aspect=1.5)
    setBrawEnv("graphicsType","HTML")
    # setBrawEnv("fontSize",0.75)
    
    mType="rs;p"
    if (showNow=="Mean") {mType<-"dv.mn;dv.sd"; showNow<-"Sample"}
    if (showNow=="Kurt") {mType<-"dv.sk;dv.kt"; showNow<-"Sample"}
    mrType<-mType
    if (showNow=="NHST") {mType<-"rse;pe"; mrType<-"NHST"; showNow<-"Schematic"}
    if ((process=="single" || process=="analysis") && showNow!="SchematicSEM") {
      schematic<-makePanel(showInference(showType=mType,effectType="direct"),reportInference())
    } 
    if (process=="multiple") {
      schematic<-makePanel(showMultiple(showType=mType,effectType="direct"),reportMultiple(showType=mrType))
      showNow<-"Schematic"
    }      
    if (process=="single" && showNow=="SchematicSEM") {
      schematic<-makePanel(showInference(effectType="direct"),plotSEMModel(braw.res$result$SEM))
      showNow<-"Schematic"
    }      
  }
  
  if (showNow=="None") {
    tabs<-c("Plan","Sample","Effect","Schematic")
    tabContents<-c(
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL)
    )
    tabLink=NULL
    tabLinkLabel=NULL
  } 
  
  if (showNow=="Plan") {
    tabs<-c("Plan","Sample","Effect","Schematic")
    tabContents<-c(
      makePanel(showPlan()),
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL)
    )
    tabLink=NULL
    tabLinkLabel=NULL
  } 
  if (!is.element(showNow,c("None","Plan"))) {
      tabs<-c("Plan","Sample","Effect","Schematic")
      tabContents<-c(
        makePanel(showPlan()),
        makePanel(showMarginals(style=marginalsStyle),reportSample()),
        makePanel(showDescription(whichEffect=whichEffect),
                  paste0(reportInference(),reportDescription(plain=TRUE))),
        schematic
      )
    tabLink=paste0('https://doingpsychstats.wordpress.com/theory-',stepBS,'#',partBS)
    tabLinkLabel=paste0('&#x24D8 ',rootBS)
  }
  if (showJamovi) {
    tabs<-c(tabs,"Jamovi")
    tabContents<-c(tabContents,JamoviInstructions())
  } else {
    tabs<-c(tabs,"Jamovi")
    tabContents<-c(tabContents,nullPlot())
  }
  
  if (showHelp) {
    tabs<-c(tabs,"Help")
    tabContents<-c(tabContents,brawTheoryHelp(open=c(0,0),indent=100,plainTabs=TRUE))
  }
  
  open<-which(showNow==tabs)
  if (isempty(open)) open<-0
  
  history<-braw.res$theoryHistory
  if (is.null(history)) {
    if (doHistory) history<-list(content='',place=1)
    else history<-list(content=NULL,sequence=c(),place=1)
  }
  
  theoryResults<-
    generate_tab(
      title="Theory:",
      plainTabs=TRUE,
      titleWidth=100,
      width=600,
      tabs=tabs,
      tabContents=tabContents,
      tabLink=tabLink,
      tabLinkLabel=tabLinkLabel,
      history=history$content,
      open=open
    )
  
  if (doHistory) {
    history$content<-theoryResults
    history$place<-length(history$content)
  } else {
    history$sequence<-c(history$sequence,theoryResults)
    history$place<-length(history$sequence)
  }
  setBrawRes("theoryResults",history)
  setBrawRes("theoryDone",c(stepBS,partBS))
  
  setBrawDef("hypothesis",oldHypothesis)
  setBrawDef("design",oldDesign)
  setBrawDef("evidence",oldEvidence)
  
  setBrawEnv("allScatter",oldAllScatter)
  
  if (showOutput) {
    showHTML(theoryResults)
    return(invisible(NULL))
  }
  
  return(theoryResults)
}
