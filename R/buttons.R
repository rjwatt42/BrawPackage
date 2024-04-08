
library(rpanel)

backC<-"#BFECFF"
backB<-"#005E86"
backP<-"#002D40"

singleResult<-NULL
multipleResult<-NULL
exploreResult<-NULL
exploreType<-"n"

singleShow<-"data"
multipleShow<-"Basic"
exploreShow<-"r"

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

hypothesis<-function(panel) {
  switch(panel$hypothesis,
         "null"= setBrawDef("hypothesis",getHypothesis("null")),
         "simple"= setBrawDef("hypothesis",getHypothesis("simple")),
         "world"= setBrawDef("hypothesis",getHypothesis("Psych"))
  )
  multipleResult<<-NULL
}

design<-function(panel) {
  switch(panel$design,
         "simple"= setBrawDef("design",getDesign("simple")),
         "world"= setBrawDef("hypothesis",getDesign("Psych"))
  )
  multipleResult<<-NULL
}

displaySingle<-function() {
  switch(singleShow,
         "data"={print(showSample(singleResult))},
         "describe"={print(showDescription(singleResult))},
         "infer"={print(showInference(singleResult))}
  )
}
doSingle<-function(panel) {
  singleResult<<-makeAnalysis()
  displaySingle()
}
displayMultiple<-function() {
  print(showExpected(multipleResult,showType=multipleShow))
}
doMultiple<-function(panel) {
  multipleResult<<-makeExpected(50,multipleResult)
  displayMultiple()
}
displayExplore<-function() {
  print(showExplore(exploreResult,showType=exploreShow))
}
doExplore<-function(panel) {
  if (exploreType=="n") 
    exploreResult<<-makeExplore(10,exploreResult=exploreResult,exploreType=exploreType,max_n=1000,xlog=TRUE,autoShow=FALSE)
  else 
    exploreResult<<-makeExplore(10,exploreResult=exploreResult,exploreType=exploreType,autoShow=FALSE)
  displayExplore()
}

# close<-function(panel) {
#   rp.control.dispose(panel)
# }

panel<-rp.control("controls",background=backP,size=c(80,1000))

row<-0
rp.grid(panel, pos=list(row=row, column=0, sticky="news", width=10, height=10),
        background=backP, name="b0")
row<-row+1
rp.grid(panel, pos=list(row=row, column=0, sticky="news", width=10, height=10),
        background=backP)
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="hypothesis")
rp.grid(panel, pos=list(row=row, column=2, sticky="news", width=10, height=10),
        background=backP)
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="design")
rp.grid(panel, pos=list(row=row, column=4, sticky="news", width=10, height=10),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news", width=100, height=30),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="single")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="multiple")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news", width=100, height=20),
        background=backP)
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="exploreType")
rp.grid(panel, pos=list(row=row, column=3, sticky="news"),
        background=backP, name="exploreShow")
row<-row+1
rp.grid(panel, pos=list(row=row, column=1, sticky="news"),
        background=backP, name="explore")
# rp.grid(panel, pos=list(row=row, column=4, sticky="news", width=150, height=200),
#         background=backP, name="close")


rp.listbox(panel,action=hypothesis,variable=hypothesis,vals=c("null","simple","world"),initval="simple",title="Hypothesis",background=backC,parentname="hypothesis")
rp.listbox(panel,action=design,variable=design,vals=c("simple","world",""),initval="simple",title="Design",background=backC,parentname="design")

rp.listbox(panel,action=setSingle,variable=single,vals=c("data","describe","infer",""),initval="describe",title="Single",background=backC,parentname="single")
rp.button(panel,action=doSingle,title="New Sample",background=backB,foreground="white",parentname="single")
rp.listbox(panel,action=setMultiple,variable=multiple,vals=c("Basic","NHST","fDR","fMR"),initval="Basic",title="Multiple",background=backC,parentname="multiple")
rp.button(panel,action=doMultiple,title="Make",background=backB,foreground="white",parentname="multiple")

rp.listbox(panel,action=setExploreType,variable=exploreType,vals=c("n","alpha","pNull",""),initval="describe",title="Explore Type",background=backC,parentname="exploreType")
rp.listbox(panel,action=setExplore,variable=exploreShow,vals=c("r","p(sig)","NHST","fDR"),initval="describe",title="Show",background=backC,parentname="exploreType")
rp.button(panel,action=doExplore,title="Explore",background=backB,foreground="white",parentname="exploreType")

# rp.button(panel,action=close,title="Close",background=backB,foreground="white",parentname="close")


