
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

singleShow<-"data"
multipleShow<-"Basic"
exploreShowH<-"p(sig)"
exploreShowD<-"p(sig)"
exploreDone<-"H"

IV<-"Interval"
DV<-"Interval"
rIV<-0.3
pNull<-0.0
sN<-42
sAlpha<-0.05
sMethod<-"Random"
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
}

setDesignExtras<-function(panel) {
  sN<<-as.numeric(panel$designPars[1])
  sAlpha<<-as.numeric(panel$designPars[2])
  multipleResult<<-NULL
  exploreResultH<<-NULL
  exploreResultD<<-NULL
}

setDesignMethod<-function(panel) {
  sMethod<<-panel$sMethod
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
setExploreH<-function(panel) {
  exploreShowH<<-panel$exploreShowH
  if (!is.null(exploreResultH)) displayExplore()
}
setExploreD<-function(panel) {
  exploreShowD<<-panel$exploreShowD
  if (!is.null(exploreResultD)) displayExplore()
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
}
doHypothesis<-function(panel) {
  prepare()
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
}
doDesign<-function(panel) {
  prepare()
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
  setBrawDef("design",design)
  
}

displaySingle<-function() {
  switch(singleShow,
         "data"={print(showSample(singleResult))},
         "describe"={print(showDescription(singleResult))},
         "infer"={print(showInference(singleResult))},
         "2D"={print(showInference(singleResult,dimension="2D"))}
  )
}
doSingle<-function(panel) {
  prepare()
  singleResult<<-makeAnalysis(autoShow=FALSE)
  displaySingle()
}

displayMultiple<-function() {
  print(showExpected(multipleResult,showType=multipleShow))
}
doMultiple<-function(panel) {
  prepare()
  multipleResult<<-makeExpected(50,expectedResult=multipleResult,autoShow=FALSE)
  displayMultiple()
}

displayExplore<-function() {
  switch(exploreDone,
         "D"={
           print(showExplore(exploreResultD,showType=exploreShowD))
         },
         "H"={
           print(showExplore(exploreResultH,showType=exploreShowH))
         })
}
doExploreH<-function(panel) {
  prepare()
  exploreResultH<<-makeExplore(10,exploreResult=exploreResultH,exploreType=exploreTypeH,autoShow=FALSE)
  exploreDone<<-"H"
  displayExplore()
}
doExploreD<-function(panel) {
  prepare()
  switch(exploreTypeD,
         "n"={
           exploreResultD<<-makeExplore(10,exploreResult=exploreResultD,exploreType=exploreTypeD,max_n=1000,xlog=TRUE,autoShow=FALSE)
         },
         "method"={
           exploreResultD<<-makeExplore(10,exploreResult=exploreResultD,exploreType="Method",autoShow=FALSE)
         },
         {
           exploreResultD<<-makeExplore(10,exploreResult=exploreResultD,exploreType=exploreTypeD,autoShow=FALSE)
         }
  )
  exploreDone<<-"D"
  displayExplore()
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

margin<-10
rowMargin<-20
panelWidth<-120

panel<-rp.control("controls",background=backP,size=c(2.5*margin+2*panelWidth,800))

row<-0
rp.grid(panel, pos=list(row=row, column=0, sticky="news", width=margin, height=rowMargin),
        background=backP, name="b0")
rp.grid(panel, pos=list(row=row, column=1, sticky="news",width=panelWidth),
        background=backP, name="b1")
rp.grid(panel, pos=list(row=row, column=2, sticky="news", width=margin/2),
        background=backP, name="b2")
rp.grid(panel, pos=list(row=row, column=3, sticky="news",width=panelWidth),
        background=backP, name="b3")
rp.grid(panel, pos=list(row=row, column=4, sticky="news", width=margin),
        background=backP,name="b4")
row<-row+1
# Hypothesis
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="hypothesis")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="hypothesis2")
row<-row+1
# Gap
rp.grid(panel, pos=list(row=row, column=0, sticky="news",height=rowMargin),
        background=backP)
row<-row+1
# Design
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="design")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="design2")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news",height=rowMargin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="single")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="multiple")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news",height=rowMargin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="exploreHypothesis")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="exploreDesign")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="explore")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="explore2")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news",height=rowMargin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="display")

# rp.grid(panel, pos=list(row=row+1,column=6,sticky="news",width=700,height=700),background=backC,
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
rp.textentry(panel,hypothesisPars,action=setHypExtras,labels=c("rIV","pNull"),initval=c(rIV,pNull),
             background=hypothesisC,parentname="hypothesis2",pos="bottom")

rp.radiogroup(panel,action=setDesign,variable=design,
           vals=c("simple","world"),initval="simple",title="Design",
           background=designC,parentname="design")
rp.button(panel,action=doDesign,title="Show",
          background=backB,foreground="white",parentname="design")
rp.radiogroup(panel,action=setDesignMethod,variable=sMethod,
              vals=c("Random","Convenience"),initval="Random",title="Method",
              background=designC,parentname="design2")
rp.textentry(panel,designPars,action=setDesignExtras,labels=c("n","alpha"),initval=c(sN,sAlpha),
             background=designC,parentname="design2",pos="bottom")

rp.radiogroup(panel,action=setSingle,variable=single,
           vals=c("data","describe","infer","2D"),initval=singleShow,title="Single",
           background=evidenceC,parentname="single")
rp.button(panel,action=doSingle,title="New Sample",
          background=backB,foreground="white",parentname="single")
rp.radiogroup(panel,action=setMultiple,variable=multiple,
           vals=c("Basic","NHST","fDR","fMR"),initval="Basic",title="Multiple",
           background=evidenceC,parentname="multiple")
rp.button(panel,action=doMultiple,title="Make",
          background=backB,foreground="white",parentname="multiple")

rp.radiogroup(panel,action=setExploreTypeH,variable=exploreTypeH,
              vals=c("rIV","pNull"),initval="rIV",title="Explore Hypothesis",
              background=exploreC,parentname="exploreHypothesis")
rp.radiogroup(panel,action=setExploreTypeD,variable=exploreTypeD,
              vals=c("n","method","alpha"),initval="n",title="Explore Design",
              background=exploreC,parentname="exploreDesign")
rp.radiogroup(panel,action=setExploreH,variable=exploreShowH,
           vals=c("r","p","p(sig)","NHST","fDR"),initval=exploreShowH,title="Show",
           background=exploreC,parentname="explore")
rp.button(panel,action=doExploreH,title="Explore",
          background=backB,foreground="white",parentname="explore")
rp.radiogroup(panel,action=setExploreD,variable=exploreShowD,
              vals=c("r","p","p(sig)","NHST","fDR"),initval=exploreShowD,title="Show",
              background=exploreC,parentname="explore2")
rp.button(panel,action=doExploreD,title="Explore",
          background=backB,foreground="white",parentname="explore2")

rp.button(panel,action=newDisplay,title="New Display",
          background=backB,foreground="white",parentname="display")

# rp.tkrplot(panel, tkrp, draw, parentname="plot")



