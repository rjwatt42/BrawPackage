# stepMS<-function(doing) substr(doing,5,5)
# partMS<-function(doing) substr(doing,6,6)
# singleMS<-function(doing) substr(doing,7,7)!='m'

#' @export
stepMS<-function(doing) gsub('[A-Za-z]*([0-9]*)[A-Da-d]*','\\1',doing)
#' @export
partMS<-function(doing) toupper(gsub('[A-Za-z]*[0-9]*([A-Da-b]*)','\\1',doing))
#' @export
singleMS<-function(doing) !grepl('m',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)
#' @export
replicateMS<-function(doing) grepl('r',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)
#' @export
replicateFirstMS<-function(doing) grepl('rm',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)
#' @export
combineMS<-function(doing) grepl('c',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)
#' @export
combineFirstMS<-function(doing) grepl('cm',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)

#' @export
prepareMetaScience<-function(doingMetaScience,world="Psych50",rp=0.3,pRplus=0.5,metaPublicationBias=FALSE,
                             alt4B=FALSE,
                          sN=NULL,sMethod="Convenience",sMethodSeverity=0.1,
                          sBudget=100,sSplits=5,sCheating="Replace",sCheatingProportion=0.05,
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
  combine<-combineMS(doingMetaScience)
  
  if (is.null(sN)) {
    if (stepMetaSci=="2") sN<-100
    else sN<-50
  }
  
  switch(stepMetaSci,
         "0"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain")))
           design<-makeDesign(sN=42)
           evidence<-makeEvidence(sigOnly=0)
         },
         "1"={
           switch(partMetaSci,
                  "I"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain",rp=rp))),
                  "A"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Binary",rp=rp))),
                  "B"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Psych50",rp=rp)))
           )
           if (world!="Plain") hypothesis$effect$world$pRplus<-pRplus
           design<-makeDesign(sN=sN)
           evidence<-makeEvidence(sigOnly=metaPublicationBias)
         },
         "2"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world,rp=rp)))
           if (world!="Plain") hypothesis$effect$world$pRplus<-pRplus
           
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
         "3"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world,rp=rp)))
           if (world!="Plain") hypothesis$effect$world$pRplus<-pRplus
           design<-makeDesign(sN=sN)
           switch(partMetaSci,
                  "A"={
                    design$sMethod<-makeSampling(sMethod)
                    design$sMethodSeverity<-sMethodSeverity
                    },
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
         "4"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world,rp=rp)))
           if (world!="Plain") hypothesis$effect$world$pRplus<-pRplus
           design<-makeDesign(sN=sN)
           if (!alt4B)
             if (partMetaSci=="B") {
               switch (sReplicationOriginalAnomaly,
                       "Random"={},
                       "Convenience"={
                         design$sMethod<-makeSampling("Convenience")
                         design$sMethodSeverity<-sMethodSeverity
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
           sN<-200
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
           evidence<-makeEvidence(AnalysisTerms=analysisTerms,sigOnly=0)
           if (replicateMS(doingMetaScience))  evidence$sigOnly<-sReplicationSigOriginal
         }
  )
  if (replicate) {
    design$Replication<-makeReplication(TRUE,Keep=sReplicationKeep,
                                        Power=sReplicationPower,
                                        replicateAll=sReplicationAll,
                                        UseLikelihood=sReplicationUseLikelihood)
    if (alt4B && steppartMetaSci=="4B") design$Replication$Keep<-"MetaAnalysis"
  }
  if (combine) {
    design$Replication<-makeReplication(TRUE,Keep="MetaAnalysis",
                                        Power=sReplicationPower,
                                        replicateAll=sReplicationAll,
                                        UseLikelihood=sReplicationUseLikelihood)
  }
  
  return(list(step=doingMetaScience,hypothesis=hypothesis,design=design,evidence=evidence))
}

#' @export
doMetaScience<-function(metaScience,nreps=200,alt4B=FALSE,showOutput=TRUE,doHistory=TRUE,
                        world="Psych50",rp=0.3,pRplus=0.5,metaPublicationBias=FALSE,
                        sN=NULL,
                        sMethod="Convenience",sMethodSeverity=0.1,sBudget=100,sSplits=5,
                        sCheating="Replace",sCheatingProportion=0.05,
                        sReplicationKeep="Cautious",sReplicationPower=0.9,
                        sReplicationAll=FALSE,sReplicationSigOriginal=TRUE,
                        sReplicationOriginalAnomaly="Random",sReplicationUseLikelihood=FALSE,
                        differenceSource="Interaction",range=NULL,rangeWidth=0,
                        rangeVar=NULL,rangeP=NULL,analysisTerms=c(TRUE,FALSE,FALSE,FALSE)
) {
  
  pC<-braw.env$plotColours
  setColours("meta")
  if (is.character(metaScience)) 
    metaScience<-prepareMetaScience(metaScience,alt4B=alt4B,
                                    world=world,rp=rp,pRplus=pRplus,metaPublicationBias=metaPublicationBias,
                                    sN=sN,sMethod=sMethod,sMethodSeverity=sMethodSeverity,sBudget=sBudget,sSplits=sSplits,
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
  combine<-combineMS(doingMetaScience)
  
  doingMore<-identical(braw.def$hypothesis,braw.res$multiple$hypothesis) &&
    identical(braw.def$design,braw.res$multiple$design) &&
    identical(braw.def$evidence,braw.res$multiple$evidence) && !single
  
  if (single) {
    if (replicate || combine) 
            doSingle(onlyReplication=TRUE)    
    else    doSingle()
    if (stepMetaSci=="5") {
      result<-braw.res$result
      result$hypothesis$IV2<-NULL
      result<-doAnalysis(result)
      setBrawRes("result",result)
      }
    outputNow<-"Description"
    if (steppartMetaSci=="2B")   setBrawRes("multiple",braw.res$result)
  } else {
    if (steppartMetaSci=="3B" && single) nreps<-nreps/4
    doMultiple(nreps,onlyReplication=replicateFirstMS(doingMetaScience))
    outputNow<-"Multiple"
  }
  
  
  # display the results
  svgBox(height=350,aspect=1.5,fontScale=1.2)
  setBrawEnv("graphicsType","HTML")
  
  # if (stepMetaSci=="0") setBrawEnv("fullOutput",0)
  # else 
    setBrawEnv("fullOutput",1)
  if (steppartMetaSci=="2B" && single) setBrawEnv("reportCounts",TRUE)
  else setBrawEnv("reportCounts",FALSE)
  
  investgD<-braw.res$investgD
  investgS<-braw.res$investgS
  investgR<-braw.res$investgR
  showTheory=TRUE
  # if (stepMetaSci=="5") showTheory=FALSE
  if (single) {
    investgD<-showDescription()
    investgS<-showInference(showType="rse",orientation="horz",dimension=1,showTheory=showTheory)
    if (is.element(steppartMetaSci,c("2B")))
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
                '</table>',
                '</div>'
  )
  show2<-paste0('<div style="display:inline-block;margin-bottom:10px;margin-top:10px;">',
                '<table>',
                '<tr><td>', braw.res$investgS, '</td></tr>',
                '<tr><td>', braw.res$investgR, '</td></tr>',
                '</table>',
                '</div>'
  )
  showPlan<-reportWorldDesign()
  linkLabel<-paste0(rootMetaSci)
  
  history<-braw.res$investgHistory
  if (is.null(history)) history<-list(content='')
  if (!doHistory) history$content<-NULL
  
  # if (doingMore && !is.null(history$content)) {
  #   h<-unlist(strsplit(history$content,'<div class=\"history\"',fixed=TRUE))
  #   h<-paste0('<div class=\"history\"',paste0(h[3:length(h)]))
  #   history$content<-h
  # }
  investgResults<-
    generate_tab(
      title="MetaScience:",
      plainTabs=FALSE,
      titleWidth=100,
      width=550,
      tabs=c("Plan","Data","Schematic"),
      tabContents=c(showPlan,show1,show2),
      tabLink=paste0('https://doingpsychstats.wordpress.com/metascience-',stepMetaSci,'#','Part',stepMetaSci,partMetaSci),
      tabLinkLabel=paste0('&#x24D8 ',linkLabel),
      # tabLinkLabel=paste0(linkLabel),
      history=history$content,
      open=open+1
    )
  
  if (doHistory) {
    history$content<-investgResults
    history$place<-length(history$content)
    setBrawRes("investgHistory",history)
  }
  if (showOutput) {
    showHTML(investgResults)
    return(invisible(NULL))
  }
  
  setBrawEnv("plotColours",pC)
  return(invisible(investgResults))
}
