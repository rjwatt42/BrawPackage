
# This file is a generated template, your changes will not be overwritten

BrawSimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawSimClass",
  inherit = BrawSimBase,
  private = list(
    .init = function() {
    },
    .run = function() {
      # debug information
      # self$results$debug$setVisible(TRUE)
      # self$results$debug$setContent(c(self$options$showExploreBtn,is.null(dataStore$exploreResult)))

      # initialization code 
      if (!exists("braw.env")) {
        BrawOpts(fontScale = 1.5,graphC="white",graphicsSize=17)
        braw.env$graphicsSize<<-braw.env$graphicsSize*0.6
        statusStore<-list(lastOutput="System",
                          showSampleType="Sample",
                          showInferParam="Basic",
                          showInferDimension="1D",
                          showMultipleParam="Basic",
                          showMultipleDimension="1D",
                          showExploreParam="Basic",
                          showExploreDimension="1D"
        )
        braw.env$statusStore<<-statusStore
        braw.env$table<<-NULL
        braw.res$lm<<-NULL
      }
      
      if (is.null(braw.env$statusStore)) {
        statusStore<-list(lastOutput="System",
                          showSampleType="Sample",
                          showInferParam="Basic",
                          showInferDimension="1D",
                          showMultipleParam="Basic",
                          showMultipleDimension="1D",
                          showExploreParam="Basic",
                          showExploreDimension="1D"
        )
        braw.env$statusStore<<-statusStore
      } else       statusStore<-braw.env$statusStore
      
      # get some display parameters for later
      makeSampleNow<-self$options$makeSampleBtn
      showSampleType<-self$options$showSampleType
      showInferParam<-self$options$showInferParam
      showInferDimension<-self$options$showInferDimension
      
      makeMultipleNow<-self$options$makeMultipleBtn
      showMultipleParam<-self$options$showMultipleParam
      showMultipleDimension<-self$options$showMultipleDimension
      whichShowMultipleOut<-self$options$whichShowMultiple
      
      makeExploreNow<-self$options$makeExploreBtn
      switch(self$options$exploreMode,
             "hypothesisExplore"={
               typeExplore<-self$options$hypothesisExploreList
               minV<-as.numeric(self$options$exploreMinValH)
               maxV<-as.numeric(self$options$exploreMaxValH)
               xlog<-self$options$exploreXLogH
               exploreNPoints<-as.numeric(self$options$exploreNPointsH)
             },
             "designExplore"={
               typeExplore<-self$options$designExploreList
               minV<-as.numeric(self$options$exploreMinValD)
               maxV<-as.numeric(self$options$exploreMaxValD)
               xlog<-self$options$exploreXLogD
               exploreNPoints<-as.numeric(self$options$exploreNPointsD)
             },
             "analysisExplore"={
               typeExplore<-self$options$analysisExploreList
               minV<-as.numeric(self$options$exploreMinValA)
               maxV<-as.numeric(self$options$exploreMaxValA)
               xlog<-self$options$exploreXLogA
               exploreNPoints<-as.numeric(self$options$exploreNPointsA)
             },
             "moreExplore"={
               typeExplore<-self$options$moreExploreList
               minV<-as.numeric(self$options$exploreMinValM)
               maxV<-as.numeric(self$options$exploreMaxValM)
               xlog<-self$options$exploreXLogM
               exploreNPoints<-as.numeric(self$options$exploreNPointsM)
             }
      )
      showExploreParam<-self$options$showExploreParam
      showExploreDimension<-self$options$showExploreDimension
      whichShowExploreOut<-self$options$whichShowMultiple
      
      if (showExploreParam=="Custom") {
        showExploreParam<-paste0(self$options$exploreVar1,";",self$options$exploreVar2)
      } 
      if (showMultipleParam=="Custom") {
        showMultipleParam<-paste0(self$options$multipleVar1,";",self$options$multipleVar2)
      } 
      if (showInferParam=="Custom") {
        showInferParam<-paste0(self$options$singleVar1,";",self$options$singleVar2)
      } 
      
      # unless we have done something, we will make the same output as last time
      outputNow<-statusStore$lastOutput
      
      # we pressed the "show" hypothesis button
      if (self$options$showHypothesisBtn) outputNow<-"System"

      # are we asking for a different display of the current explore?
      if (!is.null(braw.res$explore)) {
        if (showExploreParam != statusStore$showExploreParam ||
            showExploreDimension != statusStore$showExploreDimension)
          outputNow<-"Explore"
      }
      # or expected?
      if (!is.null(braw.res$expected)) {
        if (showMultipleParam != statusStore$showMultipleParam ||
            showMultipleDimension != statusStore$showMultipleDimension)
          outputNow<-"Multiple"
      }
      # or result?
      if (!is.null(braw.res$result)) {
        if (showInferParam != statusStore$showInferParam ||
            showInferDimension != statusStore$showInferDimension)
          outputNow<-"Infer"
        if (showSampleType != statusStore$showSampleType)
          outputNow<-showSampleType
      }
      
      # make all the standard things we need
      # the hypothesis has IV, DV and effect
      DV<-makeVariable(self$options$DVname,self$options$DVtype,
                       mu=self$options$DVmu,sd=self$options$DVsd,skew=self$options$DVskew,kurtosis=self$options$DVkurt,
                       ncats=self$options$DVncats,cases=self$options$DVcases,proportions=self$options$DVprops,
                       nlevs=self$options$DVnlevs,iqr=self$options$DViqr)
      IV<-makeVariable(self$options$IVname,self$options$IVtype,
                       mu=self$options$IVmu,sd=self$options$IVsd,skew=self$options$IVskew,kurtosis=self$options$IVkurt,
                       ncats=self$options$IVncats,proportions=self$options$IVprops,
                       nlevs=self$options$IVnlevs,iqr=self$options$IViqr)
      if (self$options$IV2on) {
        IV2<-makeVariable(self$options$IV2name,self$options$IV2type,
                          mu=self$options$IV2mu,sd=self$options$IV2sd,skew=self$options$IV2skew,kurtosis=self$options$IV2kurt,
                          ncats=self$options$IV2ncats,proportions=self$options$IV2props,
                          nlevs=self$options$IV2nlevs,iqr=self$options$IV2iqr)
      } else {
        IV2<-NULL
      }
      
      effect<-makeEffect(rIV=self$options$EffectSize1,
                         rIV2=self$options$EffectSize2,
                         rIVIV2<-self$options$EffectSize3,
                         rIVIV2DV<-self$options$EffectSize12,
                         Heteroscedasticity=self$options$Heteroscedasticity,
                         ResidDistr=self$options$Residuals,
                         world=makeWorld(worldOn=self$options$WorldOn,
                                         populationPDF=self$options$WorldPDF,
                                         populationRZ = self$options$WorldRZ,
                                         populationPDFk = self$options$WorldLambda,
                                         populationNullp = self$options$WorldNullP
                         )
      )
      
      oldH<-braw.def$hypothesis
      hypothesis<-makeHypothesis(IV,IV2,DV,effect)
      changedH<- !identical(oldH,hypothesis)

      # set up the design variable
      oldD<-braw.def$design
      design<-makeDesign(sN=self$options$SampleSize,
                         sNRand=self$options$SampleSpread=="yes",sNRandK=self$options$SampleGamma,
                         sMethod=makeSampling(self$options$SampleMethod),
                                sIV1Use=self$options$SampleUsage1,
                                sIV2Use=self$options$SampleUsage2,
                                sDependence=self$options$Dependence,
                                sOutliers=self$options$Outliers,
                                sCheating=self$options$Cheating,sCheatingAttempts=self$options$CheatingAttempts,
                         Replication=makeReplication(On=self$options$ReplicationOn,
                                                     Power=self$options$ReplicationPower,
                                                     Repeats=self$options$ReplicationAttempts,
                                                     Keep=self$options$ReplicationDecision,
                                                     forceSigOriginal=FALSE,forceSign=(self$options$ReplicationSign=="yes"),
                                                     RepAlpha=self$options$ReplicationAlpha,
                                                     PowerPrior=self$options$ReplicationPrior
                                                     )
                         )
      changedD<- !identical(oldD,design)
      
      # set up the evidence variable
      oldE<-braw.def$evidence
      evidence<-makeEvidence(rInteractionOn=self$options$interaction=="yes",
                             ssqType=self$options$ssq,
                             Welch=self$options$equalVar=="no",
                             Transform=self$options$Transform
      )
      changedE<- !identical(oldE,evidence)
      braw.env$alphaSig<<-self$options$alphaSig
      
      # set up the explore variable
      oldX<-braw.def$explore
      explore<-makeExplore(exploreType=typeExplore,
                           exploreNPoints=exploreNPoints,
                           minVal=minV,maxVal=maxV,
                           xlog=xlog)
      changedX<- !identical(oldX,explore)
      
      oldM<-braw.def$metaAnalysis
      metaAnalysis<-makeMetaAnalysis(nstudies=self$options$MetaAnalysisNStudies,
                                 analysisType=self$options$MetaAnalysisType,
                                 modelPDF="All",
                                 sigOnlySource=self$options$MetaAnalysisStudiesSig=="sigOnly",
                                 includeNulls=self$options$MetaAnalysisNulls=="yes",
                                 includeBias=self$options$MetaAnalysisBias=="yes")
      changedM<- !identical(oldM,metaAnalysis)
      
      oldP<-braw.def$possible
      possible<-makePossible(typePossible=self$options$likelihoodType,
                             UsePrior=self$options$likelihoodUsePrior,
                             prior=makeWorld(worldOn=TRUE,
                                             populationPDF=self$options$priorPDF,
                                             populationRZ=self$options$priorRZ,
                                             populationPDFk=self$options$priorLambda,
                                             populationNullp=self$options$priorNullP)
                             )
      changedP<- !identical(oldP,possible)
      
      
      # store the option variables inside the braw package
      braw.def$hypothesis<<-hypothesis
      braw.def$design<<-design
      braw.def$evidence<<-evidence
      braw.def$explore<<-explore
      braw.def$metaAnalysis<<-metaAnalysis
      braw.def$possible<<-possible
      
      # are any of the existing stored results now invalid?
      if (changedH || changedD) {
        braw.res$result<<-NULL
        braw.res$expected<<-NULL
        braw.res$explore<<-NULL
        braw.res$metaAnalysis<<-NULL
        outputNow<-"System"
      }
      if (changedE) {
        braw.res$result<<-doAnalysis(sample=braw.res$result)
        braw.res$expected<<-NULL
        braw.res$explore<<-NULL
        braw.res$metaAnalysis<<-NULL
        outputNow<-showSampleType
      }
      if (changedX) {
        braw.res$explore<<-NULL
        outputNow<-"System"
      }
      if (changedM) {
        braw.res$metaAnalysis<<-NULL
        outputNow<-"System"
      }
      
      # now we start doing things
      # did we ask for a new sample?
      if (makeSampleNow) {
        if (self$options$MetaAnalysisOn) {
          metaResult<-doMetaAnalysis(1,NULL)
          outputNow<-"MetaSingle"
        } else {
          # make a sample
          result<-doResult()
          outputNow<-showSampleType
        }
      }
      
      # did we ask for new multiples?
      if (makeMultipleNow) {
        numberSamples<-self$options$numberSamples
        if (self$options$MetaAnalysisOn) {
          metaResult<-doMetaAnalysis(numberSamples,braw.res$metaAnalysis)
          outputNow<-"MetaMultiple"
        } else {
          expectedResult<-doExpected(nsims=numberSamples,expectedResult=braw.res$expected)
          outputNow<-"Multiple"
        }
      }
      
      # did we ask for new explore?
      if (makeExploreNow) {
        numberExplores<-self$options$numberExplores
        exploreResult<-doExplore(nsims=numberExplores,exploreResult=braw.res$explore)
        outputNow<-"Explore"
      }
      
      likelihoodCutaway<-(self$options$likelihoodCutaway=="cutaway")
      # self$results$debug$setVisible(TRUE)
      # self$results$debug$setContent(c(is.logical(likelihoodCutaway),likelihoodCutaway,self$options$likelihoodCutaway))
      
      # save everything for the next round      
      statusStore$showSampleType<-showSampleType
      statusStore$showInferParam<-showInferParam
      statusStore$showInferDimension<-showInferDimension
      statusStore$showMultipleParam<-showMultipleParam
      statusStore$showMultipleDimension<-showMultipleDimension
      statusStore$showExploreParam<-showExploreParam
      statusStore$showExploreDimension<-showExploreDimension
      statusStore$lastOutput<-outputNow
      
      braw.env$statusStore<<-statusStore
      
      # what are we showing?
      # main results graphs/reports
      if (!is.null(outputNow))     
        switch(outputNow,
               "System"={
                 self$results$graphPlot$setState(outputNow)
                 self$results$reportPlot$setState(NULL)
               },
               "Sample"={
                 self$results$graphPlot$setState(outputNow)
                 self$results$reportPlot$setState(outputNow)
               },
               "Describe"={
                 self$results$graphPlot$setState(outputNow)
                 self$results$reportPlot$setState(outputNow)
               },
               "Infer"={
                 self$results$graphPlot$setState(c(outputNow,showInferParam,showInferDimension))
                 self$results$reportPlot$setState(c(outputNow,showInferParam))
               },
               "Likelihood"={
                 possibleResult<-doPossible()
                 self$results$graphPlot$setState(c(outputNow,likelihoodCutaway))
               },
               "Multiple"={
                 self$results$graphPlot$setState(c(outputNow,showMultipleParam,showMultipleDimension,whichShowMultipleOut))
                 self$results$reportPlot$setState(c(outputNow,showMultipleParam))
               },
               "Explore"={
                 self$results$graphPlot$setState(c(outputNow,showExploreParam,showExploreDimension,whichShowExploreOut))
                 # self$results$reportPlot$setState(c(outputNow,showExploreParam))
               },
               {
                 self$results$graphPlot$setState(outputNow)
                 self$results$reportPlot$setState(outputNow)
               }
        )
      
      # now we save any results to the Jamovi spreadsheet
      # single result first
      if (!is.null(braw.res$result)) {
        if (is.null(IV2)) {
          newVariables<-data.frame(braw.res$result$dv,braw.res$result$iv,braw.res$result$dv+NA)
          names(newVariables)<-c(DV$name,IV$name,"-")
        } else {
          newVariables<-data.frame(braw.res$result$dv,braw.res$result$iv,braw.res$result$iv2)
          names(newVariables)<-c(DV$name,IV$name,IV2$name)
        }

        keys<-1:length(newVariables)
        measureTypes<-sapply(newVariables,function(x) { if (is.character(x)) "Nominal" else "Continuous"})
        
        self$results$sendSample$set(keys=keys,titles=names(newVariables),
                                    descriptions=rep("simulated",length(newVariables)),
                                    measureTypes=measureTypes
        )
        self$results$sendSample$setValues(newVariables)
      }
      # then multiple result
      if (!is.null(braw.res$expected)) {
        q<-mergeExpected(braw.res$expected$result,braw.res$expected$nullresult)
          newMultiple<-data.frame(q$rIV,q$nval,q$pIV)
          newMultiple<-newMultiple[!is.na(newMultiple$q.rIV),]
          names(newMultiple)<-c("rs","n","p")
        nvars<-length(newMultiple)
        
        keys<-1:nvars
        self$results$sendMultiple$set(keys=keys,titles=names(newMultiple),
                                    descriptions=rep("simulated",nvars),
                                    measureTypes=rep("Continuous",nvars)
        )
        self$results$sendMultiple$setValues(newMultiple)
      }
      
      # end of .run()
    },
    
    .plotGraph=function(image, ...) {
      outputGraph <- image$state[1]
      if (!is.null(outputGraph)) {
        switch(outputGraph,
               "System"    =outputGraph<-showSystem(),
               "Hypothesis"=outputGraph<-showHypothesis(),
               "Design"    =outputGraph<-showDesign(),
               "Population"=outputGraph<-showPopulation(),
               "Prediction"=outputGraph<-showPrediction(),
               "Sample"    =outputGraph<-showSample(),
               "Describe"  =outputGraph<-showDescription(),
               "Infer"     =outputGraph<-showInference(showType=image$state[2],dimension=image$state[3]),
               "Likelihood"=outputGraph<-showPossible(cutaway=as.logical(image$state[2])),
               "Multiple"  =outputGraph<-showExpected(showType=image$state[2],dimension=image$state[3],effectType=image$state[4]),
               "MetaSingle"  =outputGraph<-showMetaSingle(),
               "MetaMultiple"  =outputGraph<-showMetaMultiple(),
               "Explore"   =outputGraph<-showExplore(showType=image$state[2],dimension=image$state[3],effectType=image$state[4])
        )
        print(outputGraph)
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    .plotReport=function(image, ...) {
      outputGraph <- image$state[1]
      if (!is.null(outputGraph)) {
        switch(outputGraph,
               "System"    =outputGraph<-reportTerms(),
               "Sample"    =outputGraph<-reportSample(),
               "Describe"  =outputGraph<-reportDescription(),
               "Infer"     =outputGraph<-reportInference(),
               "MetaSingle"  =outputGraph<-reportMetaAnalysis(),
               "MetaMultiple"  =outputGraph<-reportMetaAnalysis(),
               "Multiple"  =outputGraph<-reportExpected(showType=image$state[2]),
               "Explore"   =outputGraph<-reportExplore(showType=image$state[2])
        )
        print(outputGraph)
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
)
