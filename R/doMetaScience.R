# stepMS<-function(doing) substr(doing,5,5)
# partMS<-function(doing) substr(doing,6,6)
# singleMS<-function(doing) substr(doing,7,7)!='m'

stepMS<-function(doing) gsub('[A-Za-z]*([0-9]*)[A-Da-d]*','\\1',doing)
partMS<-function(doing) toupper(gsub('[A-Za-z]*[0-9]*([A-Da-d]*)','\\1',doing))
singleMS<-function(doing) !grepl('m',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([rm]*)','\\1',doing)),fixed=TRUE)
replicateMS<-function(doing) grepl('r',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([rm]*)','\\1',doing)),fixed=TRUE)
replicateFirstMS<-function(doing) grepl('rm',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-d]*([rm]*)','\\1',doing)),fixed=TRUE)

#' @export
prepareMetaScience<-function(doingMetaScience,world="Psych50",rp=0.3,pNull=0.5,metaPublicationBias=FALSE,
                             alt4B=FALSE,
                          sN=42,sMethod="Convenience",
                          sBudget=320,sSplits=16,sCheating="Replace",sCheatingProportion=0.05,
                          sReplicationKeep="Cautious",sReplicationPower=0.9,
                          sReplicationAll=FALSE,sReplicationSigOriginal=TRUE,
                          sReplicationOriginalAnomaly="Random",sReplicationUseLikelihood=FALSE,
                          differenceSource="Interaction",range=NULL,rangeWidth=0,
                          rangeVar=NULL,rangeP=NULL,analysisTerms=c(TRUE,FALSE,FALSE)
                        ) {

  stepMetaSci<-stepMS(doingMetaScience)
  partMetaSci<-partMS(doingMetaScience)
  steppartMetaSci<-paste0(stepMetaSci,partMetaSci)
  replicate<-replicateMS(doingMetaScience)
  
  switch(stepMetaSci,
         "0"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain")))
           design<-makeDesign(sN=42)
           evidence<-makeEvidence(sigOnly=FALSE)
         },
         "1"={
           switch(partMetaSci,
                  "I"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain",rp=rp))),
                  "A"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Binary",rp=rp))),
                  "B"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Psych50",rp=rp)))
           )
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           design<-makeDesign(sN=42)
           evidence<-makeEvidence(sigOnly=metaPublicationBias)
         },
         "2"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world,rp=rp)))
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           design<-makeDesign(sN=sN)
           switch(partMetaSci,
                  "A"=design$sMethod<-makeSampling(sMethod),
                  "B"={
                    design$sCheating<-sCheating
                    design$sCheatingLimit<-"Budget"
                    design$sCheatingBudget<-floor(sN*sCheatingProportion)
                    design$sCheatingAttempts<-floor(sN*sCheatingProportion)
                  }
           )
           # if (partMetaSci=="B") metaPublicationBias<-FALSE
           evidence<-makeEvidence(sigOnly=metaPublicationBias)
         },
         "3"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world,rp=rp)))
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           
           switch(partMetaSci,
                  "A"=design<-makeDesign(sN=sN),
                  "B"={
                    n<-round(sBudget/sSplits)
                    design<-makeDesign(sN=n,
                                       sCheating="Retry",
                                       sCheatingLimit="Budget",sCheatingBudget=sBudget-n,
                                       sCheatingFixedPop=FALSE)
                  }
           )
           # if (partMetaSci=="B") metaPublicationBias<-FALSE
           evidence<-makeEvidence(sigOnly=metaPublicationBias)
         },
         "4"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world,rp=rp)))
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           design<-makeDesign(sN=sN)
           if (!alt4B)
             if (partMetaSci=="B") {
               switch (sReplicationOriginalAnomaly,
                       "Random"={},
                       "Convenience"={
                         design$sMethod<-makeSampling("Convenience")
                       },
                       "Cheating"={
                         design$sCheating<-"Replace"
                       },
                       "Retry"={
                         design$sCheating<-"Retry"
                         design$sCheatingLimit="Budget"
                         design$sCheatingBudget=sN*4-sN
                         design$sCheatingFixedPop=FALSE
  
                       })
             }
           evidence<-makeEvidence(sigOnly=sReplicationSigOriginal)
         },
         "5"={
           switch (partMetaSci,
                   "A"={
                     if (is.null(rangeP)) rangeP<-0.5
                     if (is.null(rangeVar)) rangeVar<-0
                     if (is.null(rangeWidth)) rangeWidth<-0
                   },
                   "B"={
                     if (is.null(rangeP)) rangeP<-1
                     if (is.null(rangeVar)) rangeVar<-0.75
                     if (is.null(rangeWidth)) rangeWidth<-1
                   }
           )
           sN<-100
           switch(differenceSource,
                  "None"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),
                                               effect=makeEffect(rIV=0.3,rIV2=-sqrt(0.3),rIVIV2=sqrt(0.3),world=makeWorld(FALSE)))
                    design<-makeDesign(sN=sN,sIV2RangeOn=FALSE)
                  },
                  "Interaction"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),
                                               effect=makeEffect(rIV=0.3,rIV2=0,rIVIV2DV=-0.3,world=makeWorld(FALSE)))
                    if (is.null(range)) range<-c(0,2)
                    design<-makeDesign(sN=sN,sIV2RangeOn=TRUE,sIV2Range=range)
                  },
                  "Covariation"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),
                                               effect=makeEffect(rIV=0.3,rIV2=-sqrt(0.3),rIVIV2=sqrt(0.3),world=makeWorld(FALSE)))
                    if (is.null(range)) range<-0+c(-1,1)*rangeWidth/2
                    design<-makeDesign(sN=sN,sIV2RangeOn=TRUE,sIV2Range=range)
                  })
           design$sRangeProb<-rangeP
           design$sRangeVary<-rangeVar
           evidence<-makeEvidence(AnalysisTerms=analysisTerms,sigOnly=FALSE)
         }
  )
  if (replicate) {
    design$Replication<-makeReplication(TRUE,Keep=sReplicationKeep,
                                        Power=sReplicationPower,
                                        replicateAll=sReplicationAll,
                                        UseLikelihood=sReplicationUseLikelihood)
    if (alt4B && steppartMetaSci=="4B") design$Replication$Keep<-"MetaAnalysis"
  }
  
  return(list(step=doingMetaScience,hypothesis=hypothesis,design=design,evidence=evidence))
}

#' @export
doMetaScience<-function(metaScience,nreps=200,alt4B=FALSE,
                        world="Psych50",rp=0.3,pNull=0.5,metaPublicationBias=FALSE,
                        sN=42,
                        sMethod="Convenience",sBudget=320,sSplits=16,
                        sCheating="Replace",sCheatingProportion=0.05,
                        sReplicationKeep="Cautious",sReplicationPower=0.9,
                        sReplicationAll=FALSE,sReplicationSigOriginal=TRUE,
                        sReplicationOriginalAnomaly="Random",sReplicationUseLikelihood=FALSE,
                        differenceSource="Interaction",range=NULL,rangeWidth=0,
                        rangeVar=NULL,rangeP=NULL,analysisTerms=c(TRUE,FALSE,FALSE)
) {
  
  if (is.character(metaScience)) 
    metaScience<-prepareMetaScience(metaScience,alt4B=alt4B,
                                    world=world,rp=rp,pNull=pNull,metaPublicationBias=metaPublicationBias,
                                    sN=sN,sMethod=sMethod,sBudget=sBudget,sSplits=sSplits,
                                    sCheating=sCheating,sCheatingProportion=sCheatingProportion,
                                    sReplicationKeep=sReplicationKeep,sReplicationPower=sReplicationPower,
                                    sReplicationAll=sReplicationAll,sReplicationSigOriginal=sReplicationSigOriginal,
                                    sReplicationOriginalAnomaly=sReplicationOriginalAnomaly,sReplicationUseLikelihood=sReplicationUseLikelihood,
                                    differenceSource=differenceSource,range=range,rangeWidth=rangeWidth,
                                    rangeVar=rangeVar,rangeP=rangeP,analysisTerms=analysisTerms
    )

  setBrawDef("hypothesis",metaScience$hypothesis)
  setBrawDef("design",metaScience$design)
  setBrawDef("evidence",metaScience$evidence)
  
  setHTML()
  doingMetaScience<-metaScience$step
  stepMetaSci<-stepMS(doingMetaScience)
  partMetaSci<-partMS(doingMetaScience)
  steppartMetaSci<-paste0(stepMetaSci,partMetaSci)
  rootMetaSci<-paste0("Step",stepMetaSci,partMetaSci)
  single<-singleMS(doingMetaScience)
  replicate<-replicateMS(doingMetaScience)
  
  if (single) {
    if (replicate) doSingle(onlyReplication=TRUE)    
    else           doSingle()
    if (stepMetaSci=="5") {
      result<-braw.res$result
      result$hypothesis$IV2<-NULL
      result<-doAnalysis(result)
      setBrawRes("result",result)
      }
    outputNow<-"Description"
    if (steppartMetaSci=="3B")   setBrawRes("multiple",braw.res$result)
  } else {
    if (steppartMetaSci=="2B" && single) nreps<-nreps/4
    doMultiple(nreps,onlyReplication=replicateFirstMS(doingMetaScience))
    outputNow<-"Multiple"
  }
  
  
  # display the results
  svgBox(height=350,aspect=1.5,fontScale=1.2)
  setBrawEnv("graphicsType","HTML")
  
  # if (stepMetaSci=="0") setBrawEnv("fullOutput",0)
  # else 
    setBrawEnv("fullOutput",1)
  if (steppartMetaSci=="3B" && single) setBrawEnv("reportCounts",TRUE)
  else setBrawEnv("reportCounts",FALSE)
  
  investgD<-braw.res$investgD
  investgS<-braw.res$investgS
  investgR<-braw.res$investgR
  showTheory=TRUE
  # if (stepMetaSci=="5") showTheory=FALSE
  if (single) {
    investgD<-showDescription()
    investgS<-showInference(showType="rse",orientation="horz",dimension=1,showTheory=showTheory)
    if (is.element(steppartMetaSci,c("3B")))
      investgR<-reportMultiple(showType="NHST",compact=TRUE)
    else     investgR<-reportInference(compact=TRUE)
    if (is.element(steppartMetaSci,c("2B","3B","4A","4B")))
      open<-2                   
    else open<-1
  } else {
    investgS<-showMultiple(showType="rse",dimension=1,orientation="horz",whichEffect = "Main 1",effectType="direct",showTheory=showTheory)
    if (stepMetaSci=="5") {
        investgR<-reportMultiple(showType="rs",compact=TRUE,whichEffect = "Main 1",effectType="direct")
      } else {
        investgR<-reportMultiple(showType="NHST",compact=TRUE)
      }
      open<-2
    }
  setBrawRes("investgD",investgD)
  setBrawRes("investgS",investgS)
  setBrawRes("investgR",investgR)
  
  show1<-paste0('<div style="display:inline-block;margin-bottom:10px;margin-top:10px;">',
                '<table>',
                '<tr><td>', braw.res$investgD, '</td></tr>',
                '<tr><td>', braw.res$investgR, '</td></tr>',
                '<tr style="height:10px;"></tr>',
                '<tr><td>', moreHTML(reportWorldDesign(),title="see Plan",ID="p1"), '</td></tr>',
                '</table>',
                '</div>'
  )
  show2<-paste0('<div style="display:inline-block;margin-bottom:10px;margin-top:10px;">',
                '<table>',
                '<tr><td>', braw.res$investgS, '</td></tr>',
                '<tr><td>', braw.res$investgR, '</td></tr>',
                '<tr style="height:10px;"></tr>',
                '<tr><td>', moreHTML(reportWorldDesign(),title="see Plan",ID="p2"), '</td></tr>',
                '</table>',
                '</div>'
  )
  linkLabel<-paste0(rootMetaSci)
  investgResults<-
    generate_tab(
      title="MetaScience:",
      plainTabs=FALSE,
      titleWidth=100,
      width=550,
      tabs=c("Data","Schematic"),
      tabContents=c(show1,show2),
      tabLink=paste0('https://doingpsychstats.wordpress.com/metascience-',stepMetaSci,'#','Part',stepMetaSci,partMetaSci),
      tabLinkLabel=paste0('&#x24D8 ',linkLabel),
      open=open
    )
  
  return(investgResults)
}
