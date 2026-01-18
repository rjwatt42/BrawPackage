
#' @export
stepBS<-function(doing) gsub('[A-Za-z]*([0-9]*)[A-Da-d]*','\\1',doing)

#' @export
partBS<-function(doing) toupper(gsub('[A-Za-z]*[0-9]*([A-Da-d]*)','\\1',doing))

#' @export
singleBS<-function(doing) !grepl('m',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([RMrm]*)','\\1',doing)),fixed=TRUE)

#' @export
reanalyseBS<-function(doing) grepl('r',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([RMrm]*)','\\1',doing)),fixed=TRUE)

#' @export
randomDV<-function() {
  if (runif(1)<0.5)   DV<-randomParDV()
  else                DV<-randomCatDV()
  return(DV)
}

#' @export
randomIV<-function(DV="DV") {
  if (DV$type=="Interval") {
    switch(ceiling(runif(1)*3),
           {IV<-randomParIV(DV)},
           {IV<-randomCat2IV(DV)},
           {IV<-randomCat3IV(DV)}
    )
  } else {
    switch(ceiling(runif(1)*3),
           {IV<-randomParIV(DV)},
           {IV<-randomOrdIV(DV)},
           {IV<-randomCat2IV(DV)}
    )
  }
  return(IV)
}

#' @export
randomParDV<-function() {
  all<-c("Happiness","ExamGrade","ReactionTime","RiskTaking"
  )
  use<-ceiling(runif(1)*length(all))
  return(getVariable(all[use]))
}

#' @export
randomCatDV<-function() {
  all<-c("TrialOutcome","ExamPass?","RiskTaker?")
  use<-ceiling(runif(1)*length(all))
  return(getVariable(all[use]))
}

#' @export
randomParIV<-function(DV="DV") {
  switch(DV$name,
         "DV"={allvars<-c("IV")},
         "Happiness"={allvars<-c("Perfectionism","Diligence","Anxiety")},
         "ExamGrade"={allvars<-c("HoursSleep","SelfConfidence","Perfectionism","Diligence","IQ")},
         "RiskTaking"={allvars<-c("SelfConfidence","Perfectionism")},
         "ReactionTime"={allvars<-c("SelfConfidence","Perfectionism","InformationLevel")},
         "TrialOutcome"={allvars<-c("SelfConfidence","Perfectionism","InformationLevel")},
         "ExamPass?"={allvars<-c("Perfectionism","Diligence","Anxiety")},
         "RiskTaker?"={allvars<-c("SelfConfidence","Perfectionism")}
  )
  use<-ceiling(runif(1)*length(allvars))
  return(getVariable(allvars[use]))
}

#' @export
randomOrdIV<-function(DV="DV") {
  # only for Categorical DV
  switch(DV$name,
         "DV"={allvars<-c("IVOrd")},
         "TrialOutcome"={allvars<-c("Sessions","PracticeTrials")},
         "ExamPass?"={allvars<-c("SelfConfidenceOrd","PerfectionismOrd")},
         "RiskTaker?"={allvars<-c("SelfConfidenceOrd","PerfectionismOrd")}
  )
  use<-ceiling(runif(1)*length(allvars))
  return(getVariable(allvars[use]))
}

#' @export
randomCat2IV<-function(DV="DV") {
  switch(DV$name,
         "DV"={allvars<-c("IVCat")},
         "Happiness"={allvars<-c("NeuroType","Gender")},
         "ExamGrade"={allvars<-c("NeuroType","Coffee?","RiskTaker?")},
         "RiskTaking"={allvars<-c("NeuroType","Gender")},
         "ReactionTime"={allvars<-c("Condition","Group")},
         "TrialOutcome"={allvars<-c("Treatment?","TrialPhase")},
         "ExamPass?"={allvars<-c("NeuroType","Coffee?","RiskTaker?")},
         "RiskTaker?"={allvars<-c("NeuroType","Gender")}
  )
  use<-ceiling(runif(1)*length(allvars))
  return(getVariable(allvars[use]))
}

#' @export
randomCat3IV<-function(DV="DV") {
  switch(DV$name,
         "DV"={all<-c("IV3Cat")},
         "Happiness"={all<-c("Diagnosis","BirthOrder","StudySubject")},
         "ExamGrade"={all<-c("Diagnosis","BirthOrder")},
         "RiskTaking"={all<-c("Diagnosis","BirthOrder")},
         "ReactionTime"={all<-c("Condition3","Group3","MemoryCondition")},
         "TrialOutcome"={all<-c("Treatment3","TrialPhase3")},
         "ExamPass?"={all<-c("Diagnosis","BirthOrder")},
         "RiskTaker?"={all<-c("Diagnosis","BirthOrder")}
  )
  use<-ceiling(runif(1)*length(all))
  return(getVariable(all[use]))
}

#' @export
makePanel<-function(g,r=NULL) {
  paste0('<div style="display:inline-block;margin-bottom:10px;margin-top:10px;">',
         '<table>',
         '<tr><td>', g, '</td></tr>',
         '<tr><td>', r, '</td></tr>',
         # '<tr style="height:10px;"></tr>',
         # '<tr><td>', moreHTML(reportWorldDesign(),"see Plan","p1"), '</td></tr>',
         '</table>',
         '</div>'
  )
}

#' @export
doBasics<-function(doingBasics=NULL,showOutput=TRUE,showJamovi=TRUE,showHelp=FALSE,
                   showPlanOnly=FALSE,doHistory=TRUE,
                   IV="Perfectionism",IV2=NULL,DV="ExamGrade",
                   skew=0,kurtosis=0,
                   rIV=NULL,rIV2=NULL,rIVIV2=NULL,rIVIV2DV=NULL,
                   sN=NULL,sMethod=NULL,sDataFormat=NULL,
                   sOutliers=0, sDependence=0,
                   sIV1Use="Between",sIV2Use="Between",
                   analyse="Main12", 
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
  
  if (!is.null(doingBasics)) {
    stepBS<-stepBS(doingBasics)
    partBS<-partBS(doingBasics)
    if (singleBS(doingBasics)) process<-"single" else process<-"multiple"
    rootBS<-paste0("Step",stepBS,partBS)
    
    variables=list(IV=IV,IV2=IV2,DV=DV)
    
    marginalsStyle<-"all"
    hideReport<-FALSE
    makeData<-TRUE
    whichEffect="Main1+2"
    
    switch(stepBS,
           "0"={
             showNow<-"Plan"
           },
           "1"={ # making samples and analysing them in Jamovi
             switch(partBS,
                    "A"={showNow<-"Effect"},
                    "B"={showNow<-"Sample"
                    if (is.null(sMethod)) sMethod<-"Convenience"
                    },
                    "C"={showNow<-"Effect"
                    if (is.null(sN)) sN<-500
                    }
             )
           },
           "2"={ # 3 basic tests with Interval DV
             variables$DV<-randomParDV()
             switch(partBS,
                    "A"={variables$IV<-randomParIV(variables$DV)},
                    "B"={variables$IV<-randomCat2IV(variables$DV)},
                    "C"={variables$IV<-randomCat3IV(variables$DV)},
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
             switch(partBS,
                    "A"={hideReport<-TRUE;showJamovi<-FALSE;showNow<-"Sample"},
                    "B"={hideReport<-FALSE;makeData<-FALSE;showNow<-"Effect"},
                    {}
             )
             if (makeData) {
               variables$DV<-randomDV()
               variables$IV<-randomIV(variables$DV)
             }
             
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
             switch(partBS,
                    "A"={hideReport<-TRUE;showJamovi<-FALSE;showNow<-"Sample"},
                    "B"={hideReport<-FALSE;makeData<-FALSE;showNow<-"Effect"},
                    {}
             )
             
             if (makeData) {
               variables$DV<-randomDV()
               variables$IV<-randomIV(variables$DV)
               while (1==1) {
                 variables$IV2<-randomIV(variables$DV)
                 if (variables$IV2$name!=variables$IV$name) break;
               }
               if (runif(1)>0.5) rIV<-0.3 else rIV<-0
               if (runif(1)>0.5) rIV2<-0.3 else rIV2<-0
               if (runif(1)>0.5) rIVIV2<-0.3 else rIVIVIV2<-0
               if (runif(1)>0.5) rIVIV2DV<-0.3 else rIVIV2DV<-0
             }
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
             else whichEffect<-"Main1+2"
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
    
    if (makeData) {
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
                                 effect=makeEffect(rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV)
      )
      if (stepBS=="1") hypothesis$DV$skew<-skew
      if (stepBS=="1") hypothesis$DV$kurtosis<-kurtosis
      if (stepBS=="4") hypothesis$layout<-"simple"
      if (stepBS=="5") hypothesis$layout<-"noCovariation"
      if (stepBS=="8") hypothesis$layout<-"noCovariation"
      if (stepBS=="6") hypothesis$layout<-"noInteraction"
      if (stepBS=="9") hypothesis$layout<-"moderation"
      if (stepBS=="10") hypothesis$layout<-"mediation"
      
      if (is.null(sN))  sN<-100
      if (is.null(sMethod)) sMethod<-"Random"
      if (is.null(sDataFormat)) sDataFormat<-"long"
      design<-makeDesign(sN=sN,sMethod=makeSampling(sMethod),sDataFormat=sDataFormat,
                         sOutliers=sOutliers, sDependence=sDependence,
                         sIV1Use=sIV1Use,sIV2Use=sIV2Use)
      setBrawDef("hypothesis",hypothesis)
      setBrawDef("design",design)
      
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
    
    if ((process=="single" || process=="analysis") && showNow!="SchematicSEM") {
      schematic<-makePanel(showInference(effectType="direct"),reportInference())
    } 
    if (process=="multiple") {
      schematic<-makePanel(showMultiple(effectType="direct"),reportMultiple())
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
        makePanel(showMarginals(style=marginalsStyle),NULL),
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
    tabLink=paste0('https://doingpsychstats.wordpress.com/basics-',stepBS,'#',partBS)
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
    tabContents<-c(tabContents,brawBasicsHelp(open=c(0,0),indent=100,plainTabs=TRUE))
  }
  
  open<-which(showNow==tabs)
  if (isempty(open)) open<-0
  
  history<-braw.res$basicsHistory
  if (is.null(history)) {
    if (doHistory) history<-list(content='',place=1)
    else history<-list(content=NULL,sequence=c(),place=1)
  }
  
  basicsResults<-
    generate_tab(
      title="Basics:",
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
    history$content<-basicsResults
    history$place<-length(history$content)
  } else {
    history$sequence<-c(history$sequence,basicsResults)
    history$place<-length(history$sequence)
  }
  setBrawRes("basicsHistory",history)
  setBrawRes("basicsDone",c(stepBS,partBS))
  
  setBrawDef("hypothesis",oldHypothesis)
  setBrawDef("design",oldDesign)
  setBrawDef("evidence",oldEvidence)
  
  setBrawEnv("allScatter",oldAllScatter)
  
  if (showOutput) {
    showHTML(basicsResults)
    return(invisible(NULL))
  }
  
  return(basicsResults)
}

