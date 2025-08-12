rootMS<-function(doing) substr(doing,1,5)
stepMS<-function(doing) substr(doing,5,5)
partMS<-function(doing) substr(doing,6,6)
singleMS<-function(doing) substr(doing,7,7)!='m'


#' @export
prepareMetaScience<-function(doingMetaScience,world="Binary",rp=0.3,pNull=0.5,
                        sN=42,sMethod="Convenience",sBudget=320,sSplits=16,sCheating="Grow",
                        sReplicationPower=0.9,sReplicationSigOriginal=TRUE,
                        differenceSource="Interaction"
                        ) {

  stepInv<-stepMS(doingMetaScience)
  partInv<-partMS(doingMetaScience)

  switch(stepInv,
         "0"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain")))
           design<-makeDesign(sN=42)
           evidence<-makeEvidence()
         },
         "1"={
           switch(partInv,
                  "I"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain"))),
                  "A"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Binary"))),
                  "B"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Psych50")))
           )
           design<-makeDesign(sN=42)
           evidence<-makeEvidence()
         },
         "2"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world)))
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           design<-makeDesign(sN=sN)
           switch(partInv,
                  "A"=design$sMethod<-makeSampling(sMethod),
                  "B"={
                    design$sCheating<-sCheating
                    design$sCheatingLimit<-"Budget"
                    design$sCheatingBudget<-sN*0.5
                  }
           )
           evidence<-makeEvidence()
         },
         "3"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world)))
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           
           switch(partInv,
                  "A"=design<-makeDesign(sN=sN),
                  "B"={
                    n<-round(sBudget/sSplits)
                    design<-makeDesign(sN=n,
                                       sCheating="Retry",
                                       sCheatingLimit="Budget",sCheatingBudget=sBudget-n,
                                       sCheatingFixedPop=FALSE)
                  }
           )
           evidence<-makeEvidence()
         },
         "4"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world)))
           if (world!="Plain") hypothesis$effect$world$populationNullp<-pNull
           design<-makeDesign(sN=sN)
           switch(partInv,
                  "A"= {
                    design$Replication<-makeReplication(FALSE)
                  },
                  "B"={
                    design$Replication<-makeReplication(TRUE,
                                                        forceSigOriginal=TRUE,Power=sReplicationPower)
                  }
           )
           evidence<-makeEvidence(sigOnly=TRUE)
         },
         "5"={
           switch(differenceSource,
                  "Interaction"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),
                                               effect=makeEffect(rIV=0.3,rIV2=0,rIVIV2DV=-0.3,world=makeWorld(FALSE)))
                    design<-makeDesign(sN=1000,sIV2RangeOn=TRUE,sIV2Range=c(1,1),sRangeP=0.5)
                  },
                  "Covariation"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),
                                               effect=makeEffect(rIV=0.3,rIV2=-sqrt(0.3),rIVIV2=sqrt(0.3),world=makeWorld(FALSE)))
                    design<-makeDesign(sN=1000,sIV2RangeOn=TRUE,sIV2Range=range<-c(0,0),sRangeP=0.5)
                  })
           switch(partInv,
                  "A"={
                    design$sRangeP<-0.5
                    design$sRangeV<-0
                  },
                  "B"={
                    design$sRangeP<-1
                    design$sRangeV<-1
                  }
           )
           evidence<-makeEvidence(AnalysisTerms=1)
         }
  )
  hypothesis$effect$world$populationPDFk<-rp

  return(list(hypothesis=hypothesis,design=design,evidence=evidence))
}

#' @export
doMetaScience<-function(doingInvestg,metaScience=prepareMetaScience(),nreps=200) {
  
  setBrawDef("hypothesis",metaScience$hypothesis)
  setBrawDef("design",metaScience$design)
  setBrawDef("evidence",metaScience$evidence)
  
  if (nchar(doingInvestg)<7) doingInvestg<-paste0(doingInvestg,'s')
  
  setHTML()
  rootInv<-rootMS(doingInvestg)
  stepInv<-stepMS(doingInvestg)
  partInv<-partMS(doingInvestg)
  steppartInv<-paste0(stepInv,partInv)
  single<-singleMS(doingInvestg)
  
  if (single) {
    doSingle()
    outputNow<-"Description"
    if (steppartInv=="3B")   setBrawRes("multiple",braw.res$result)
  } else {
    if (steppartInv=="2B" && single) nreps<-nreps/4
      doMultiple(nreps)
    outputNow<-"Multiple"
  }
    
  if (stepInv=="5") {
    if (single) {
      oldSingle<-braw.res$result
      result<-braw.res$result
      result$hypothesis$IV2<-NULL
      result$hypothesis$effect$world<-makeWorld(TRUE,"Single","r",0.3,populationNullp=0.5)
      setBrawRes("result",result)
    } else {
      oldMultiple<-braw.res$multiple
      multiple<-braw.res$multiple
      multiple$hypothesis$IV2<-NULL
      multiple$result$hypothesis$IV2<-NULL
      # multiple$hypothesis$effect$world<-makeWorld(TRUE,"Single","r",0.3,populationNullp=0.5)
      # multiple$result$hypothesis$effect$world<-makeWorld(TRUE,"Single","r",0.3,populationNullp=0.5)
      setBrawRes("multiple",multiple)
    } 
  }
  
  # display the results
  svgBox(height=350,aspect=1.5,fontScale=1.2)
  setBrawEnv("graphicsType","HTML")
  
  if (stepInv=="0") setBrawEnv("fullOutput",0)
  else setBrawEnv("fullOutput",1)
  if (steppartInv=="2B") setBrawEnv("reportCounts",TRUE)
  else setBrawEnv("reportCounts",FALSE)
  
  investgD<-braw.res$investgD
  investgS<-braw.res$investgS
  investgR<-braw.res$investgR
  if (single) {
    investgD<-showDescription()
    investgS<-showInference(showType="rse",orientation="horz",dimension=1)
    if (is.element(steppartInv,c("3B")))
      investgR<-reportMultiple(showType="NHST",compact=TRUE)
    else     investgR<-reportInference(compact=TRUE)
    if (is.element(steppartInv,c("2B","3B","4A","4B")))
      open<-2                   
    else open<-1
  } else {
    if (stepInv=="5") {
        investgS<-showMultiple(showType="rs",dimension=1,orientation="horz")
        investgR<-reportMultiple(showType="rs",compact=TRUE)
      } else {
        investgS<-showMultiple(showType="rse",dimension=1,orientation="horz")
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
  linkLabel<-paste0(rootInv,partInv)
  investgResults<-
    generate_tab(
      title="MetaScience:",
      plainTabs=FALSE,
      titleWidth=100,
      width=550,
      tabs=c("Data","Schematic"),
      tabContents=c(show1,show2),
      tabLink=paste0('https://doingpsychstats.wordpress.com/metascience-',stepInv,'#','Part',stepInv,partInv),
      tabLinkLabel=paste0('&#x24D8 ',linkLabel),
      open=open
    )
  
  if (stepInv=="5") {
    if (single) braw.res$result<-oldSingle
      else braw.res$multiple<-oldMultiple
  }
  
  return(investgResults)
}
