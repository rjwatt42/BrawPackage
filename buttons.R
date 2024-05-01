
library(devtools)
load_all("/Users/rogerwatt/Documents/GitHub/BrawPack")
newBrawDev()

library(rpanel)

backC<-"#BFECFF"
backB<-"#005E86"
backP<-"#002D40"
hypothesisC<-"#F2B6BB"
designC<-"#F2DAB6"
evidenceC<-"#B6F2B6"
exploreC<-"#C2C7F2"


newDisplay<-function(panel) {
  newBrawDev()
}

singleResult<-NULL
multipleResult<-NULL
exploreResultH<-NULL
exploreTypeH<-"rIV"
exploreResultD<-NULL
exploreTypeD<-"n"
showing<-"none"

singleShow<-"data"
multipleShow<-"Basic"
exploreShow<-"r"
exploreDone<-"H"

IV<-"Interval"
DV<-"Interval"
rIV<-0.3
pNull<-0.0
sN<-42
sAlpha<-0.05
sMethod<-"Random"
replicationPower<-0.8
replicationRepeats<-1
replicationType<-"none"
replicationIgnoreNS<-TRUE
replicationSigOnly<-FALSE
nameHypothesis<-"Single"
setBrawDef("hypothesis",getHypothesis(nameHypothesis))

setIV<-function(panel) {
  IV<<-panel$IV
}
setDV<-function(panel) {
  DV<<-panel$DV
}

setHypExtras<-function(panel) {
  rIV<<-as.numeric(panel$hypothesisPars[1])
  pNull<<-as.numeric(panel$hypothesisPars[2])
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
  if (showing=="possible") doPossible(panel)
  if (showing=="hypothesis") doHypothesis(panel)
}

setDesignExtras<-function(panel) {
  sN<<-as.numeric(panel$designPars[1])
  sAlpha<<-as.numeric(panel$designPars[2])
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
  if (showing=="possible") doPossible(panel)
  if (showing=="design") doDesign(panel)
}

setDesignMethod<-function(panel) {
  sMethod<<-panel$sMethod
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
}

setReplication<-function(panel) {
  replicationType<<-panel$replicationType
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
}

setReplicationExtras<-function(panel) {
  replicationPower<<-as.numeric(panel$replPars[1])
  replicationRepeats<<-as.numeric(panel$replPars[2])
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
}

setSingle<-function(panel) {
  singleShow<<-panel$single
  if (!is.null(singleResult)) displaySingle()
}
setMultiple<-function(panel) {
  multipleShow<<-panel$multiple
  if (!is.null(multipleResult)) displayMultiple()
}
setExplore<-function(panel) {
  exploreShow<<-panel$exploreShow
  if (!is.null(exploreResultH)) displayExplore()
}
setExploreTypeH<-function(panel) {
  exploreTypeH<<-panel$exploreTypeH
  exploreResultH<<-NULL
}
setExploreTypeD<-function(panel) {
  exploreTypeD<<-panel$exploreTypeD
  exploreResultD<<-NULL
}

setHypothesis<-function(panel) {
  nameHypothesis<<-panel$hypothesis
  switch(panel$hypothesis,
         "null"= setBrawDef("hypothesis",getHypothesis("Null")),
         "single"= setBrawDef("hypothesis",getHypothesis("Single")),
         "double"= setBrawDef("hypothesis",getHypothesis("Double")),
         "exp"= setBrawDef("hypothesis",getHypothesis("PsychF"))
  )
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
  if (showing=="possible") doPossible(panel)
  if (showing=="hypothesis") doHypothesis(panel)
}
doHypothesis<-function(panel) {
  prepare()
  showing<<-"hypothesis"
  print(showHypothesis())
}

setDesign<-function(panel) {
  switch(panel$design,
         "simple"= setBrawDef("design",getDesign("simple")),
         "world"= setBrawDef("design",getDesign("Psych"))
  )
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
  if (showing=="possible") doPossible(panel)
  if (showing=="design") doDesign(panel)
}
doDesign<-function(panel) {
  prepare()
  showing<<-"design"
  print(showDesign())
}


prepare<-function() {
  setBrawEnv("alphaSig",sAlpha)
  
  hypothesis<-braw.def$hypothesis
  hypothesis$IV<-makeVariable(name="IV",type=IV)
  hypothesis$DV<-makeVariable(name="DV",type=DV)
  hypothesis$effect$world$worldOn<-TRUE
  if (nameHypothesis=="null") {
    hypothesis$effect$world$populationPDFk<-0
    hypothesis$effect$world$populationNullp<-0
  } else {
    hypothesis$effect$world$populationPDFk<-rIV
    hypothesis$effect$world$populationNullp<-pNull
  }
  setBrawDef("hypothesis",hypothesis)
  
  design<-braw.def$design
  design$sN<-sN
  design$sMethod<-makeSampling(sMethod)
    switch (replicationType,
            "none"=design$Replication<-makeReplication(On=FALSE),
            "all"=design$Replication<-makeReplication(On=TRUE,Power=replicationPower,ignoreNS=FALSE,Repeats=replicationRepeats),
            "sig only"=design$Replication<-makeReplication(On=TRUE,Power=replicationPower,ignoreNS=TRUE,Repeats=replicationRepeats)
            )
  setBrawDef("design",design)
  
}

displaySingle<-function() {
  showing<<-"single"
  if (singleShow=="2D") {
    print(showResult(singleResult,show="infer",showType="Basic",dimension="2D"))
  } else {
    print(showResult(singleResult,show=singleShow))
  }
}
doSingle<-function(panel) {
  prepare()
  singleResult<<-doResult(autoShow=FALSE)
  displaySingle()
}

displayMultiple<-function() {
  showing<<-"multiple"
  print(showExpected(multipleResult,showType=multipleShow))
}
doMultiple<-function(panel) {
  prepare()
  multipleResult<<-doExpected(50,expectedResult=multipleResult,autoShow=FALSE)
  displayMultiple()
}

displayExplore<-function() {
  showing<<-"explore"
  switch(exploreDone,
         "D"={
           print(showExplore(exploreResultD,showType=exploreShow))
         },
         "H"={
           print(showExplore(exploreResultH,showType=exploreShow))
         })
}
doExploreH<-function(panel) {
  prepare()
  exploreResultH<<-doExplore(10,exploreResult=exploreResultH,exploreType=exploreTypeH,autoShow=FALSE)
  exploreDone<<-"H"
  displayExplore()
}
doExploreD<-function(panel) {
  prepare()
  switch(exploreTypeD,
         "n"={
           exploreResultD<<-doExplore(10,exploreResult=exploreResultD,exploreType=exploreTypeD,max_n=1000,xlog=TRUE,autoShow=FALSE)
         },
         "method"={
           exploreResultD<<-doExplore(10,exploreResult=exploreResultD,exploreType="Method",autoShow=FALSE)
         },
         {
           exploreResultD<<-doExplore(10,exploreResult=exploreResultD,exploreType=exploreTypeD,autoShow=FALSE)
         }
  )
  exploreDone<<-"D"
  displayExplore()
}

doPossible<-function(panel) {
  prepare()
  showing<<-"possible"
  showPossible(makePossible("Samples"))
}

doMenu<-function(panel) {
  print(panel$menuItem)
}
# 
# draw<-function(panel) {
#   print(showSample())
#   panel
# }
# 
# redraw <- function(panel) {
#   rp.tkrreplot(panel, tkrp)
#   panel
# }

margin<-1
rowMargin<-10
panelWidth<-90
alignment<-"news"
textWidth<-12
panel<-rp.control("controls",background=backP,size=c(3*margin+3*panelWidth,800))
rp.menu(panel,menuItem,list(list("type1","type2","type3")),initval="type1",action=doMenu)

row<-0
rp.grid(panel, pos=list(row=row, column=0, sticky=alignment, width=margin, height=rowMargin),
        background=backP, name="b0")
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment,width=panelWidth),
        background=backP, name="b1")
rp.grid(panel, pos=list(row=row, column=2, sticky=alignment, width=margin/2),
        background=backP, name="b2")
rp.grid(panel, pos=list(row=row, column=3, sticky=alignment,width=panelWidth),
        background=backP, name="b3")
rp.grid(panel, pos=list(row=row, column=4, sticky=alignment, width=margin/2),
        background=backP,name="b4")
rp.grid(panel, pos=list(row=row, column=5, sticky=alignment,width=panelWidth),
        background=backP, name="b5")
rp.grid(panel, pos=list(row=row, column=6, sticky=alignment, width=margin),
        background=backP,name="b6")
row<-row+1
# Hypothesis
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment),
        background=backP, name="hypothesis")
rp.grid(panel, pos=list(row=row, column=3, sticky=alignment),
        background=backP, name="hypothesis2")
row<-row+1
# Gap
rp.grid(panel, pos=list(row=row, column=0, sticky=alignment,height=rowMargin),
        background=backP)
row<-row+1
# Design
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment),
        background=backP, name="design")
rp.grid(panel, pos=list(row=row, column=3, sticky=alignment),
        background=backP, name="design2")
rp.grid(panel, pos=list(row=row, column=5, sticky=alignment),
        background=backP, name="replication")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment,height=rowMargin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment),
        background=backP, name="single")
rp.grid(panel, pos=list(row=row, column=3, sticky=alignment),
        background=backP, name="multiple")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment,height=rowMargin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment),
        background=backP, name="exploreHypothesis")
rp.grid(panel, pos=list(row=row, column=3, sticky=alignment),
        background=backP, name="exploreDesign")
rp.grid(panel, pos=list(row=row, column=5, sticky=alignment),
        background=backP, name="explore3")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment,height=rowMargin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky=alignment),
        background=backP, name="possible")
rp.grid(panel, pos=list(row=row, column=3, sticky=alignment),
        background=backP, name="display")

# rp.grid(panel, pos=list(row=row+1,column=6,sticky=alignment,width=700,height=700),background=backC,
#         name="plot")

rp.radiogroup(panel,action=setIV,variable=IV,
              vals=c("Interval","Categorical"),initval="Interval",title="IV",
              background=hypothesisC,parentname="hypothesis")
rp.radiogroup(panel,action=setDV,variable=DV,
              vals=c("Interval","Categorical"),initval="Interval",title="DV",
              background=hypothesisC,parentname="hypothesis")
rp.button(panel,action=doHypothesis,title="Show",
          background=backB,foreground="white",parentname="hypothesis")
rp.radiogroup(panel,action=setHypothesis,variable=hypothesis,
              vals=c("null","single","double","exp"),initval="single",title="Effect",
              background=hypothesisC,parentname="hypothesis2")
rp.textentry(panel,hypothesisPars,action=setHypExtras,labels=c("rIV","pNull"),initval=c(rIV,pNull),width=textWidth,
             background=hypothesisC,parentname="hypothesis2")

rp.radiogroup(panel,action=setDesign,variable=design,
           vals=c("simple","world"),initval="simple",title="Sample Size",
           background=designC,parentname="design")
rp.button(panel,action=doDesign,title="Show",
          background=backB,foreground="white",parentname="design")
rp.radiogroup(panel,action=setDesignMethod,variable=sMethod,
              vals=c("Random","Convenience"),initval="Random",title="Sample Method",
              background=designC,parentname="design2")
rp.textentry(panel,designPars,action=setDesignExtras,labels=c("n","alpha"),initval=c(sN,sAlpha),width=textWidth,
             background=designC,parentname="design2")

rp.radiogroup(panel,variable=replicationType,action=setReplication,vals=c("none","sig only","all"),title="Replicate",initval=replicationType,
            background=designC,parentname="replication")
rp.textentry(panel,replPars,action=setReplicationExtras,labels=c("power","repeats"),initval=c(replicationPower,replicationRepeats),width=textWidth,
             background=designC,parentname="replication")

rp.radiogroup(panel,action=setSingle,variable=single,
           vals=c("data","describe","infer","2D"),initval=singleShow,title="Single",
           background=evidenceC,parentname="single")
rp.button(panel,action=doSingle,title="New Sample",
          background=backB,foreground="white",parentname="single")
rp.radiogroup(panel,action=setMultiple,variable=multiple,
           vals=c("Basic","NHST","Hits","Misses"),initval="Basic",title="Multiple",
           background=evidenceC,parentname="multiple")
rp.button(panel,action=doMultiple,title="Make",
          background=backB,foreground="white",parentname="multiple")

rp.radiogroup(panel,action=setExploreTypeH,variable=exploreTypeH,
              vals=c("rIV","pNull"),initval="rIV",title="Explore Hypothesis",
              background=exploreC,parentname="exploreHypothesis")
rp.button(panel,action=doExploreH,title="Explore",
          background=backB,foreground="white",parentname="exploreHypothesis")
rp.radiogroup(panel,action=setExploreTypeD,variable=exploreTypeD,
              vals=c("n","method","alpha"),initval="n",title="Explore Design",
              background=exploreC,parentname="exploreDesign")
rp.button(panel,action=doExploreD,title="Explore",
          background=backB,foreground="white",parentname="exploreDesign")
rp.radiogroup(panel,action=setExplore,variable=exploreShow,
           vals=c("r","p","p(sig)","NHST","Hits"),initval=exploreShow,title="Show",
           background=exploreC,parentname="explore3")

rp.button(panel,action=doPossible,title="Possible",
          background=backB,foreground="white",parentname="possible")

rp.button(panel,action=newDisplay,title="New Display",
          background=backB,foreground="white",parentname="display")

# rp.tkrplot(panel, tkrp, draw, parentname="plot")



