
#' @export
doDemonstration<-function(doingDemo,world="Binary",pRPlus=0.5,
                          sN=42,sMethod="Convenience",sBudget=320,sSplits=16,sCheating="grow",
                          sReplicationPower=0.9,sReplicationSigOriginal=TRUE,
                          group="a",
                          nreps=200) {
  
  setHTML()
  rootInv<-substr(doingInvestg,1,4)
  partInv<-substr(doingInvestg,5,5)
  single<-nchar(substr(doingInvestg,6,6))==0
  
  switch(rootInv,
         "Inv0"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain")))
           design<-makeDesign(sN=42)
           
         },
         "Inv1"={
           switch(partInv,
                  "I"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Plain"))),
                  "A"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Binary"))),
                  "B"=hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld("Psych50")))
           )
           design<-makeDesign(sN=42)
           
         },
         "Inv2"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world)))
           if (world!="Plain") hypothesis$effect$world$pRPlus<-pRPlus
           
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
           
         },
         "Inv3"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world)))
           if (world!="Plain") hypothesis$effect$world$pRPlus<-pRPlus
           design<-makeDesign(sN=sN)
           switch(partInv,
                  "A"=design$sMethod<-makeSampling(sMethod),
                  "B"={
                    design$sCheating<-sCheating
                    design$sCheatingLimit<-"Budget"
                    design$sCheatingBudget<-sN*0.5
                  }
           )
           
         },
         "Inv4"={
           hypothesis<-makeHypothesis(effect=makeEffect(world=getWorld(world)))
           if (world!="Plain") hypothesis$effect$world$pRPlus<-pRPlus
           design<-makeDesign(sN=sN)
           switch(partInv,
                  "A"= {
                    design$Replication<-makeReplication(TRUE,Keep="Cautious",
                                                        forceSigOriginal=sReplicationSigOriginal,Power=sReplicationPower)
                  },
                  "B"={
                    design$Replication<-makeReplication(TRUE,Keep="MetaAnalysis",
                                                        forceSigOriginal=sReplicationSigOriginal,Power=sReplicationPower)
                  }
           )
           
         },
         "Inv5"={
           switch(partInv,
                  "A"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),effect=makeEffect(rIV=0.3/2,rIV2=0,rIVIV2DV=0.3/2))
                    if (group=="a") range<-c(1,1) else range<-c(-1,-1)
                    design<-makeDesign(sN=1000,sIV2RangeOn=TRUE,sIV2Range=range)
                    
                  },
                  "B"={
                    hypothesis<-makeHypothesis(IV2=makeVariable("IV2","Interval"),effect=makeEffect(rIV=0.3,rIV2=-sqrt(0.3),rIVIV2=sqrt(0.3)))
                    if (group=="a") range<-c(0,0) else range<-c(-4,4)
                    design<-makeDesign(sN=1000,sIV2RangeOn=TRUE,sIV2Range=range)
                    
                  }
           )
           evidence<-makeEvidence(AnalysisTerms=c(TRUE,FALSE,FALSE))
           setBrawDef("evidence",evidence)
           
         }
  )
  setBrawDef("hypothesis",hypothesis)
  setBrawDef("design",design)

  
  if (single) {
    doSingle()
    outputNow<-"Description"
    if (doingInvestg=="Step2B")   setBrawRes("multiple",braw.res$result)
  } else {
    if (doingInvestg=="Step3Bm") nreps<-nreps/4
    doMultiple(nreps)
    outputNow<-"Multiple"
  }
    
  if (rootInv=="Step5") 
    if (single) {
      result<-braw.res$result
      result$hypothesis$IV2<-NULL
      setBrawRes("result",result)
    } else {
      multiple<-braw.res$multiple
      multiple$hypothesis$IV2<-NULL
      multiple$result$hypothesis$IV2<-NULL
      switch(group,
             "a"={multiple$result$hypothesis$effect$rIV<-0.3},
             "b"={multiple$result$hypothesis$effect$rIV<-0}
      )
      setBrawRes("multiple",multiple)
    } 
  
  # display the results
  svgBox(height=350,aspect=1.5,fontScale=1.2)
  setBrawEnv("graphicsType","HTML")
  
  # if (rootInv=="Step0") setBrawEnv("fullOutput",0)
  # else 
    setBrawEnv("fullOutput",1)
  if (doingInvestg=="Step2B") setBrawEnv("reportCounts",TRUE)
  else setBrawEnv("reportCounts",FALSE)
  
  investgD<-braw.res$investgD
  investgS<-braw.res$investgS
  investgR<-braw.res$investgR
  if (single) {
    investgD<-showDescription()
    investgS<-showInference(showType="rse",orientation="horz",dimension=1)
    if (is.element(doingInvestg,c("Step2B")))
      investgR<-reportMultiple(showType="NHST",compact=TRUE)
    else     investgR<-reportInference(compact=TRUE)
    if (is.element(doingInvestg,c("Step2B","Step3B","Step4A","Step4B")))
      open<-2                   
    else open<-1
    } else {
      investgS<-showMultiple(showType="rse",dimension=1,orientation="horz")
      if (rootInv=="Step5") 
        investgR<-reportMultiple(showType="rse",compact=TRUE)
      else    investgR<-reportMultiple(showType="NHST",compact=TRUE)
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
                '<tr><td>', moreHTML(reportWorldDesign(),"see Plan","p1"), '</td></tr>',
                '</table>',
                '</div>'
  )
  show2<-paste0('<div style="display:inline-block;margin-bottom:10px;margin-top:10px;">',
                '<table>',
                '<tr><td>', braw.res$investgS, '</td></tr>',
                '<tr><td>', braw.res$investgR, '</td></tr>',
                '<tr style="height:10px;"></tr>',
                '<tr><td>', moreHTML(reportWorldDesign(),"see Plan","p2"), '</td></tr>',
                '</table>',
                '</div>'
  )
  linkLabel<-paste0(substr(doingInvestg,1,6))
  investgResults<-
    generate_tab(
      title="MetaScience:",
      plainTabs=FALSE,
      titleWidth=100,
      width=550,
      tabs=c("Data","Schematic"),
      tabContents=c(show1,show2),
      tabLink=paste0('https://doingpsychstats.wordpress.com/investigation-',substr(doingInvestg,4,4),'#',substr(doingInvestg,1,5)),
      tabLinkLabel=paste0('\U24D8',linkLabel),
      open=open
    )
  
  return(investgResults)
}
