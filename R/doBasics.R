
#' @export
stepBS<-function(doing) gsub('[A-Za-z]*([0-9]*)[A-Da-d]*','\\1',doing)

#' @export
partBS<-function(doing) toupper(gsub('[A-Za-z]*[0-9]*([A-Da-d]*)','\\1',doing))

#' @export
singleBS<-function(doing) !grepl('m',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([RMrm]*)','\\1',doing)),fixed=TRUE)

#' @export
reanalyseBS<-function(doing) grepl('r',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([RMrm]*)','\\1',doing)),fixed=TRUE)

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
doBasics<-function(doingBasics=NULL,showOutput=TRUE,showJamovi=TRUE,showHelp=TRUE,
                   showPlanOnly=FALSE,doHistory=TRUE,
                   IV="Perfectionism",IV2=NULL,DV="ExamGrade",
                   rIV=NULL,rIV2=NULL,rIVIV2=NULL,rIVIV2DV=NULL,
                   sN=NULL,sMethod=NULL,sDataFormat=NULL,
                   sOutliers=0, sDependence=0,
                   sIV1Use="Between",sIV2Use="Between",
                   analyse="Main1", 
                   allScatter=NULL,fullWithinNames=NULL,
                   nreps=200
) {
  
  oldHypothesis<-braw.def$hypothesis
  oldDesign<-braw.def$design
  oldEvidence<-braw.def$evidence
  setHTML()
  
  if (is.null(doingBasics)) doingBasics<-"0A"

  if (reanalyseBS(doingBasics)) {
    stepBS<-braw.res$basicsDone[1]
    partBS<-braw.res$basicsDone[2]
    reanalyse<-TRUE
    process<-"analysis"
  } else {
    stepBS<-stepBS(doingBasics)
    partBS<-partBS(doingBasics)
    if (singleBS(doingBasics)) process<-"single" else process<-"multiple"
  }  
  rootBS<-paste0("Step",stepBS,partBS)
  variables=list(IV=IV,IV2=IV2,DV=DV)
  
  if (is.null(sN)) {
    if (paste0(stepBS,partBS)=="1C") sN<-500
    if (paste0(stepBS)=="2") sN<-100
    if (paste0(stepBS)=="3") sN<-100
    if (paste0(stepBS)=="31") sN<-100
    if (paste0(stepBS)=="4") sN<-100
    if (paste0(stepBS)=="5") sN<-150
    if (paste0(stepBS)=="6") sN<-150
    if (paste0(stepBS)=="7") sN<-50
    if (paste0(stepBS)=="8") sN<-50
    if (paste0(stepBS)=="9") sN<-500
    if (paste0(stepBS)=="10") sN<-500
    if (is.null(sN)) sN<-42
  }
  if (is.null(sMethod)) {
    if (is.element(paste0(stepBS,partBS),c("1B"))) sMethod<-"Convenience"
    if (is.null(sMethod)) sMethod<-"Random"
  }
  
  marginalsStyle<-"all"
  hideReport<-FALSE
  makeData<-TRUE
  switch(stepBS,
         "0"={
           showNow<-"Plan"
         },
         "1"={ # making samples and analysing them in Jamovi
           switch(partBS,
                  "A"={showNow<-"Effect"},
                  "B"={showNow<-"Sample"},
                  "C"={showNow<-"Effect"}
           )
         },
         "2"={ # 3 basic tests with Interval DV
           variables$DV<-"ExamGrade"
           switch(partBS,
                  "A"={variables$IV<-"Perfectionism"},
                  "B"={variables$IV<-"Smoker?"},
                  "C"={variables$IV<-"BirthOrder"},
                  {}
           )
           showNow<-"Effect"
         },
         "3"={ # 2 basic tests with Categorical DV
           variables$DV<-"TrialOutcome"
           switch(partBS,
                  "A"={variables$IV<-"Treatment?"},
                  "B"={variables$IV<-"Sessions"},
                  "C"={variables$IV<-"Diligence"},
                  {}
           )
           showNow<-"Effect"
         },
         "31"={ # Revision of all basic tests with 2 variables
           DVs<-c("ExamGrade","ExamPass?","TrialOutcome","Happiness")
           variables$DV<-DVs[ceiling(runif(1)*length(DVs))]
           
           if (is.element(variables$DV,c("ExamGrade","ExamPass?")))
                 IVs<-c("Perfectionism","Musician?","RiskTaking","RiskTaker?")
           else  IVs<-c("Sessions","Treatment?","Smoker?","Diligence")
           variables$IV<-IVs[ceiling(runif(1)*length(IVs))]

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
         "5"={ # Interactions
           variables$DV<-"ExamGrade"
           switch(partBS,
                  "A"={variables$IV<-"Coffee?";variables$IV2<-"Musician?"},
                  "B"={variables$IV<-"Anxiety";variables$IV2<-"Smoker?"},
                  "C"={variables$IV<-"Perfectionism";variables$IV2<-"HoursSleep"},
                  "D"={
                    IVs<-c("IQ","Musician?","Anxiety","RiskTaker?","SelfConfidence","Diligence","Coffee?")
                    variables$IV<-IVs[ceiling(runif(1)*length(IVs))]
                    IVs<-IVs[IVs!=variables$IV]
                    variables$IV2<-IVs[ceiling(runif(1)*length(IVs))]
                  }
           )
           if (is.null(rIV2)) rIV2<- -0.3
           if (is.null(rIVIV2DV)) rIVIV2DV<-0.3
           rIVIV2<- 0
           if (is.null(analyse)) analyse<-"Main1x2"
           showNow<-"Effect"
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
           showNow<-"Effect"
         },
         "7"={ # Experimental 1 IV
           variables$IV<-"Condition"
           variables$DV<-"Response"
           switch(partBS,
                  "A"={ sIV1Use<-"Between" },
                  "B"={ sIV1Use<-"Within"  }
                  )
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
                             effect=makeEffect(rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV)
                             )
  if (stepBS=="4") hypothesis$layout<-"simple"
  if (stepBS=="5") hypothesis$layout<-"noCovariation"
  if (stepBS=="8") hypothesis$layout<-"noCovariation"
  if (stepBS=="6") hypothesis$layout<-"noInteraction"
  if (stepBS=="9") hypothesis$layout<-"moderation"
  if (stepBS=="10") hypothesis$layout<-"mediation"
  
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
  setBrawEnv("fontSize",0.75)

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
  
  if (showNow=="Plan") {
    tabs<-c("Plan","Sample","Effect","Schematic")
    tabContents<-c(
      makePanel(showPlan()),
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL),
      makePanel(nullPlot(),NULL)
    )
    
  } else {
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
      makePanel(showDescription(),
                paste0(reportInference(),reportDescription(plain=TRUE))),
      schematic
    )
  }
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
  
  history<-braw.res$basicsHistory
  if (is.null(history)) history<-list(content='')
  if (!doHistory) history$content<-NULL
  
  linkLabel<-paste0(rootBS)
  basicsResults<-
    generate_tab(
      title="Basics:",
      plainTabs=TRUE,
      titleWidth=100,
      width=550,
      tabs=tabs,
      tabContents=tabContents,
      tabLink=paste0('https://doingpsychstats.wordpress.com/basics-',partBS,'#','A'),
      tabLinkLabel=paste0('&#x24D8 ',linkLabel),
      history=history$content,
      open=open
    )
  
  if (doHistory) {
    history$content<-basicsResults
    history$place<-length(history$content)
    setBrawRes("basicsHistory",history)
  }
  setBrawRes("basicsDone",c(stepBS,partBS))
  
  setBrawDef("hypothesis",oldHypothesis)
  setBrawDef("design",oldDesign)
  setBrawDef("evidence",oldEvidence)

  if (showOutput) {
    showHTML(basicsResults)
    return(invisible(NULL))
  }
  
  return(basicsResults)
}
