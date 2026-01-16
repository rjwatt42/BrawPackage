
#' @export
doTheory<-function(doingTheory=NULL,showOutput=TRUE,showJamovi=TRUE,showHelp=FALSE,
                   showPlanOnly=FALSE,doHistory=TRUE,
                   IV="Perfectionism",IV2=NULL,DV="ExamGrade",
                   skew=0,kurtosis=0,
                   rIV=NULL,rIV2=NULL,rIVIV2=NULL,rIVIV2DV=NULL,
                   sN=NULL,sMethod=NULL,sDataFormat=NULL,
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
    
    variables=list(IV=IV,IV2=IV2,DV=DV)
    world<-NULL
    
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
                    "A"={showNow<-"Sample"},
                    "B"={showNow<-"Effect"}
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
             showNow<-"Effect"
           },
           "3"={ # 2 basic tests with Categorical DV
             variables$DV<-randomCatDV()
             switch(partBS,
                    "A"={variables$IV<-randomCat2IV(variables$DV)},
                    "B"={variables$IV<-randomOrdIV(variables$DV)},
                    "C"={variables$IV<-randomParIV(variables$DV)},
                    {}
             )
             showNow<-"Effect"
           },
           "31"={ # Revision of all basic tests with 2 variables
             variables$DV<-randomDV()
             variables$IV<-randomIV(variables$DV)
             
             switch(partBS,
                    "A"={hideReport<-TRUE;showJamovi<-FALSE;showNow<-"Sample"},
                    "B"={hideReport<-FALSE;makeData<-FALSE;showNow<-"Effect"},
                    {}
             )
             process<-"single"
           },
           "4"={ # Main effects in multiple IVs
             variables$DV<-"ExamGrade"
             switch(partBS,
                    "A"={variables$IV<-"BirthOrder";variables$IV2<-"Musician?"},
                    "B"={variables$IV<-"Smoker?";variables$IV2<-"Anxiety"},
                    "C"={variables$IV<-"Perfectionism";variables$IV2<-"HoursSleep"},
                    "D"={
                      IVs<-c("IQ","Musician?","Anxiety","RiskTaker?","SelfConfidence","Diligence","Coffee?")
                      variables$IV<-IVs[ceiling(runif(1)*length(IVs))]
                      IVs<-IVs[IVs!=variables$IV]
                      variables$IV2<-IVs[ceiling(runif(1)*length(IVs))]
                    }
             )
             if (is.null(rIV2)) rIV2<- -0.3
             rIVIV2<- 0
             rIVIV2DV<-0
             if (is.null(analyse)) analyse<-"Main12"
             showNow<-"Effect"
           },
           "41"={ # Revision of all basic tests with 3 variables
             variables$DV<-randomDV()
             variables$IV<-randomIV(variables$DV)
             while (1==1) {
               variables$IV2<-randomIV(variables$DV)
               if (variables$IV2$name!=variables$IV$name) break;
             }
             switch(partBS,
                    "A"={hideReport<-TRUE;showJamovi<-FALSE;showNow<-"Sample"},
                    "B"={hideReport<-FALSE;makeData<-FALSE;showNow<-"Effect"},
                    {}
             )
             if (runif(1)>0.5) rIV<-0.3 else rIV<-0
             if (runif(1)>0.5) rIV2<-0.3 else rIV2<-0
             if (runif(1)>0.5) rIVIV2<-0.3 else rIVIVIV2<-0
             if (runif(1)>0.5) rIVIV2DV<-0.3 else rIVIV2DV<-0
             process<-"single"
           },
           "5"={ # Interactions
             variables$DV<-"ExamGrade"
             variables$IV<-"Coffee?";variables$IV2<-"Musician?"
             if (is.null(rIVIV2DV)) rIVIV2DV<-0.5
             switch(partBS,
                    "A"={rIV<-0; rIV2<-0}, # +ve/-ve
                    "B"={rIV<-rIVIV2DV; rIV2<- rIVIV2DV}, # on/off
                    "C"={rIV<-0;rIV2<-rIVIV2DV} # diverge
             )
             rIVIV2<- 0
             if (is.null(analyse)) analyse<-"Main1x2"
             if (length(analyse)>1) {
               if (analyse[3]) analyse<-"Main1x2"
               else analyse<-"Main12"
             }
             if (is.null(sN)) sN<-450
             showNow<-"Effect"
             if (analyse=="Main1x2") whichEffect<-"Main1x2"
             else whichEffect<-"Main1"
           },
           "6"={ # Covariation
             variables$IV<-"Anxiety"
             variables$DV<-"ExamGrade"
             rIVIV2DV<- 0
             switch(partBS,
                    "A"={
                      variables$IV2<-"HoursSleep"
                      if (is.null(rIV)) rIV<- -0.05
                      if (is.null(rIV2)) rIV2<- 0.5
                      if (is.null(rIVIV2)) rIVIV2<- -0.7
                    },
                    "B"={
                      variables$IV2<-"Perfectionism"
                      if (is.null(rIV)) rIV<- -0.35
                      if (is.null(rIV2)) rIV2<- 0.5
                      if (is.null(rIVIV2)) rIVIV2<- 0.7
                    },
                    "C"={
                      variables$IV2<-"Diligence"
                      if (is.null(rIV)) rIV<- -0.2
                      if (is.null(rIV2)) rIV2<- 0.57
                      if (is.null(rIVIV2)) rIVIV2<- 0.7
                    }
             )
             if (length(analyse)>1) {
               if (analyse[2]) analyse<-"Main12"
               else analyse<-"Main1"
             }
             if (is.null(sN)) sN<-450
             showNow<-"Effect"
             whichEffect<-"Main1+2"
           },
           "7"={ # Experimental 1 IV
             variables$IV<-"Condition"
             variables$DV<-"Response"
             switch(partBS,
                    "A"={ sIV1Use<-"Between" },
                    "B"={ sIV1Use<-"Within"  }
             )
             if (is.null(sN))  sN<-50
             showNow<-"Effect"
           },
           "8"={ # Experimental 2 IV,
             variables$IV<-"Condition"
             variables$IV2<-"Group"
             variables$DV<-"Response"
             switch(partBS,
                    "A"={ sIV1Use<-sIV2Use<-"Between" },
                    "B"={ sIV1Use<-"Within" ; sIV2Use<-"Between" },
                    "C"={ sIV1Use<-sIV2Use<-"Within"  }
             )
             if (is.null(rIVIV2DV)) rIVIV2DV<-0.3
             if (is.null(rIV)) rIV<-rIVIV2DV
             if (is.null(rIV2)) rIV2<-rIVIV2DV
             if (is.null(sDataFormat)) sDataFormat<-"wide"
             if (is.null(allScatter)) allScatter<-FALSE
             if (is.null(sN))  sN<-50
             analyse<-"Main1x2"
             showNow<-"Effect"
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
    
    if (is.null(rIV)) rIV<-0.3
    hypothesis<-makeHypothesis(IV=variables$IV,IV2=variables$IV2,DV=variables$DV,
                               effect=makeEffect(rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV
                                                 )
    )
    if (!is.null(world)) hypothesis$effect$world<-world
    if (stepBS=="1") hypothesis$DV$skew<-skew
    if (stepBS=="1") hypothesis$DV$kurtosis<-kurtosis
    if (stepBS=="4") hypothesis$layout<-"simple"
    if (stepBS=="5") hypothesis$layout<-"noCovariation"
    if (stepBS=="8") hypothesis$layout<-"noCovariation"
    if (stepBS=="6") hypothesis$layout<-"noInteraction"
    if (stepBS=="9") hypothesis$layout<-"moderation"
    if (stepBS=="10") hypothesis$layout<-"mediation"
    
    if (is.null(sN))  sN<-42
    if (is.null(sMethod)) sMethod<-"Random"
    if (is.null(sDataFormat)) sDataFormat<-"long"
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
    
    if (showNow=="Sample") mType<-"dv.mn;dv.sd" else mType="rs;p"
    if ((process=="single" || process=="analysis") && showNow!="SchematicSEM") {
      schematic<-makePanel(showInference(showType=mType,effectType="direct"),reportInference())
    } 
    if (process=="multiple") {
      schematic<-makePanel(showMultiple(showType=mType,effectType="direct"),reportMultiple())
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
    if (hideReport) {
      tabs<-c("Plan","Sample","Effect","Schematic")
      tabContents<-c(
        makePanel(showPlan()),
        makePanel(showMarginals(style="all"),NULL),
        makePanel(nullPlot(),NULL),
        makePanel(nullPlot(),NULL)
      )
    } else {
      tabs<-c("Plan","Sample","Effect","Schematic")
      tabContents<-c(
        makePanel(showPlan()),
        makePanel(showMarginals(style=marginalsStyle),reportSample()),
        makePanel(showDescription(whichEffect=whichEffect),
                  paste0(reportInference(),reportDescription(plain=TRUE))),
        schematic
      )
    }
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
