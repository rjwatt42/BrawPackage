
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
exploreResult<-NULL
exploreType<-"n"

singleShow<-"data"
multipleShow<-"Basic"
exploreShow<-"r"

IV<-"Interval"
DV<-"Interval"
rIV<-0.3
pNull<-0.0
sN<-42
setBrawDef("hypothesis",getHypothesis("Single"))

setIV<-function(panel) {
  IV<<-panel$IV
}
setDV<-function(panel) {
  DV<<-panel$DV
}

setHypExtras<-function(panel) {
  rIV<<-as.numeric(panel$hypothesisPars[1])
  pNull<<-as.numeric(panel$hypothesisPars[2])
}

setDesignExtras<-function(panel) {
  sN<<-as.numeric(panel$sN)
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
  if (!is.null(exploreResult)) displayExplore()
}
setExploreType<-function(panel) {
  exploreType<<-panel$exploreType
  exploreResult<<-NULL
}

setHypothesis<-function(panel) {
  switch(panel$hypothesis,
         "null"= setBrawDef("hypothesis",getHypothesis("Null")),
         "single"= setBrawDef("hypothesis",getHypothesis("Single")),
         "exp"= setBrawDef("hypothesis",getHypothesis("PsychF"))
  )
  multipleResult<<-NULL
  exploreResult<<-NULL
}

setDesign<-function(panel) {
  switch(panel$design,
         "simple"= setBrawDef("design",getDesign("simple")),
         "world"= setBrawDef("hypothesis",getDesign("Psych"))
  )
  multipleResult<<-NULL
  exploreResult<<-NULL
}


prepare<-function() {
  hypothesis<-braw.def$hypothesis
  hypothesis$IV<-makeVariable(name="IV",type=IV)
  hypothesis$DV<-makeVariable(name="DV",type=DV)
  hypothesis$effect$world$worldOn<-TRUE
  hypothesis$effect$world$populationPDFk<-rIV
  hypothesis$effect$world$populationNullp<-pNull
  setBrawDef("hypothesis",hypothesis)
  
  design<-braw.def$design
  design$sN<-sN
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
  multipleResult<<-makeExpected(50,expectedResult=multipleResult)
  displayMultiple()
}

displayExplore<-function() {
  print(showExplore(exploreResult,showType=exploreShow))
}
doExplore<-function(panel) {
  prepare()
  if (exploreType=="n") 
    exploreResult<<-makeExplore(10,exploreResult=exploreResult,exploreType=exploreType,max_n=1000,xlog=TRUE,autoShow=FALSE)
  else 
    exploreResult<<-makeExplore(10,exploreResult=exploreResult,exploreType=exploreType,autoShow=FALSE)
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
panelWidth<-120

panel<-rp.control("controls",background=backP,size=c(350,800))

row<-0
rp.grid(panel, pos=list(row=row, column=0, sticky="news", width=margin, height=margin),
        background=backP, name="b0")
row<-row+1
rp.grid(panel, pos=list(row=row, column=0, sticky="news", width=margin, height=margin),
        background=backP)
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="hypothesis")
rp.grid(panel, pos=list(row=row, column=2, sticky="news", width=margin/2, height=margin),
        background=backP)
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="hypothesis2")
rp.grid(panel, pos=list(row=row, column=4, sticky="news", width=margin, height=margin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news", width=panelWidth, height=margin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="design")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="design2")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news", width=panelWidth, height=margin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="single")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="multiple")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news", width=panelWidth, height=margin),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="exploreType")
# rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
#         background=exploreC, name="exploreShow")
# row<-row+1
# rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
#         background=exploreC, name="explore")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news", width=panelWidth, height=margin),
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
rp.radiogroup(panel,action=setHypothesis,variable=hypothesis,
              vals=c("null","single","exp"),initval="single",title="Effect",
              background=hypothesisC,parentname="hypothesis2")
rp.textentry(panel,hypothesisPars,action=setHypExtras,labels=c("rIV","pNull"),initval=c(rIV,pNull),
             background=hypothesisC,parentname="hypothesis2",pos="bottom")

rp.radiogroup(panel,action=setDesign,variable=design,
           vals=c("simple","world"),initval="simple",title="Design",
           background=designC,parentname="design")
rp.textentry(panel,sN,action=setDesignExtras,labels="n",initval=sN,
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

rp.radiogroup(panel,action=setExploreType,variable=exploreType,
           vals=c("n","alpha","pNull",""),initval="n",title="Explore Type",
           background=exploreC,parentname="exploreType")
rp.radiogroup(panel,action=setExplore,variable=exploreShow,
           vals=c("r","p(sig)","NHST","fDR"),initval="r",title="Show",
           background=exploreC,parentname="exploreType")
rp.button(panel,action=doExplore,title="Explore",
          background=backB,foreground="white",parentname="exploreType")

rp.button(panel,action=newDisplay,title="New Display",
          background=backB,foreground="white",parentname="display")

# rp.tkrplot(panel, tkrp, draw, parentname="plot")



