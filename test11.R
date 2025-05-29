###################

h<-getHypothesis("PsychF")
d<-getDesign("Psych")
d$Replication<-makeReplication(TRUE)

showMultiple(doMultiple(400,hypothesis=h,design=d),showType="ws;wp",dimension="2D")


###################
n=42

h<-makeHypothesis(effect=makeEffect(0))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",
                   axisScale=c(-0.5,1),walls="none",
                   showSig=TRUE,doTextResult=FALSE))

###################
n=42

w<-makeWorld(TRUE,populationPDF="Single",populationPDFk = wn2r(0.5,n),populationNullp = 0.5)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",
                   axisScale=c(-0.5,1),walls="none",
                   showSig=TRUE,doTextResult=FALSE))

###################
n=42

w<-makeWorld(TRUE,populationPDF="Single",populationPDFk = wn2r(0.8,n),populationNullp = 0.5)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",
                   axisScale=c(-0.5,1),walls="none",
                   showSig=TRUE,doTextResult=FALSE))

###################
n=120

w<-makeWorld(TRUE,populationPDF="Single",populationPDFk = wn2r(0.5,42),populationNullp = 0.5)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",
                   axisScale=c(-0.5,1),walls="none",
                   showSig=TRUE,doTextResult=FALSE))

###################
n=42

h<-makeHypothesis(effect=makeEffect(0.3))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(NA,"Samples",view="3D",walls="none",showSig=FALSE))
print(showPossible(0.3,"Populations",walls="none",doTextResult=FALSE))

###################
n=42
axisScale=c(-1,1)

w<-makeWorld(TRUE,"Exp","z",populationPDFk = 0.3,populationNullp = 0)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
p<-doPossible(makePossible(NA,n))
print(showPossible(p,"Samples",axisScale=axisScale,walls="populations"))
print(showPossible(p,"Samples",axisScale=axisScale,walls="both"))
###################

p<-doPossible(makePossible(0.3,n,UsePrior = "world"))
print(showPossible(p,"Samples",axisScale=axisScale,walls="populations"))
###################

setBrawDef("hypothesis",h)
setBrawDef("design",d)
p<-doPossible(makePossible(0.3,n,UsePrior = "world"))
print(showPossible(p,"Samples",axisScale=axisScale,walls="populations",cutaway = TRUE))

##########################

w<-makeWorld(TRUE,"Exp","z",populationPDFk = 0.3,populationNullp = 0)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)

p<-doPossible(makePossible(0.3,n,UsePrior = "world"))
print(showPossible(p,"Populations",axisScale=axisScale,walls="populations"))
##########################

w<-makeWorld(TRUE,"Uniform","r",populationPDFk = 0.3,populationNullp = 0)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)

p<-doPossible(makePossible(0.3,n,UsePrior = "world"))
print(showPossible(p,"Populations",axisScale=axisScale,walls="populations"))

##########################
n=42

w<-getWorld("PsychF")
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
p<-doPossible(makePossible(NA,n))
print(showPossible(p,"Samples",walls="none"))

p<-doPossible(makePossible(0.3,n))
print(showPossible(p,"Samples",walls="none"))

p<-doPossible(makePossible(0.3,n))
print(showPossible(p,"Samples",walls="none",cutaway = TRUE))

###################
n=42

w<-makeWorld(TRUE,"Exp",populationPDFk = 0.3,populationNullp = 0)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
p<-doPossible(makePossible(wn2r(0.51,n),n,sigOnly=TRUE,sigOnlyCompensate = TRUE))
print(showPossible(p,"Populations",view="3D",axisScale=c(-0.5,1),walls="none"))
# p<-doPossible(makePossible(0.3,42,UseSource="prior",UsePrior="prior",prior=w))
# print(showPossible(p,"Populations",view="3D",axisScale=1.2,walls="populations"))

###################
n=42

w<-makeWorld(TRUE,"Single",populationPDFk = 0.3,populationNullp = 0)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
p<-doPossible(makePossible(wn2r(0.5,n),n,sigOnly=FALSE,sigOnlyCompensate = FALSE))
print(showPossible(p,"Populations",view="3D",axisScale=1.2,walls="none"))

##################

n=rw2n(0.3,0.8)

h<-makeHypothesis(effect=makeEffect(0.3))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)

showExplore(doExplore(500,explore=makeExplore('rIV',17,0,0.8)),"p(sig)")

########################

setBrawEnv("npoints",1001)
rs<-0.3
ps<-0.05
w<-0.8

n<-round(rp2n(rs,ps))
# n<-42
nn<-round(rw2n(rs,w))
# nn<-154

world<-makeWorld(TRUE,populationPDFsample=TRUE,populationSamplemn=rs,populationSamplesd=n,populationNullp=0)
h1<-makeHypothesis(effect=makeEffect(world=world))
d1<-makeDesign(nn)
showMultiple(doMultiple(0,NULL,hypothesis=h1,design=d1),showType = "wp",orientation="horz")

########################

world<-makeWorld(TRUE,"Exp","z",0.3,0)
h1<-makeHypothesis(effect=makeEffect(world=world))
d1<-makeDesign(42,Replication=makeReplication(TRUE,Keep="MetaAnalysis"))
d1<-makeDesign(42,Replication=makeReplication(TRUE,Keep="Cautious"))

explore<-makeExplore("n",minVal=10,maxVal=5000,xlog=TRUE)
d<-doExplore(1000,explore=explore,hypothesis=h1,design=d1)
showExplore(d,showType="p(w80)",autoYlim=FALSE)
showExplore(d,showType="p(sig)",autoYlim=FALSE)
########################

world<-makeWorld(TRUE,"Exp","z",0.3,0)
h1<-makeHypothesis(effect=makeEffect())
d1<-getDesign("Psych")
d1$Replication<-makeReplication(TRUE,Keep="Cautious",forceSigOriginal = TRUE)
d1$Replication<-makeReplication(TRUE,Keep="MetaAnalysis",forceSigOriginal = TRUE)
d1$Replication<-makeReplication(TRUE,Keep="MetaAnalysis",forceSigOriginal = FALSE)

explore<-makeExplore("rIV",minVal=0,maxVal=0.9,exploreNPoints=19)
d<-doExplore(500,explore=explore,hypothesis=h1,design=d1)

showExplore(d,showType="p(sig)",autoYlim=FALSE)

###################
n=42

world<-makeWorld(TRUE,populationPDFsample=TRUE,populationSamplemn = 0.3,populationSamplesd = 42)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
d<-doMultiple(0,NULL,hypothesis=h,design=d)
g<-showMultiple(d,showType="rp",orientation="horz")

gap<-0.025
x1<-wn2r(0.05+gap,85)
x2<- -x1
g<-addG(g,dataPolygon(data.frame(x=c(x1,x1,x2,x2),y=c(0,1,1,0)),colour=NA,fill="white",alpha=0.35))

x1<-wn2r(0.5,85)
x2<-wn2r(0.5+gap,85)
g<-addG(g,dataPolygon(data.frame(x=c(x1,x1,x2,x2),y=c(0,1,1,0)),colour=NA,fill="white",alpha=0.35))

x1<-wn2r(0.8,85)
x2<-wn2r(0.8+gap,85)
g<-addG(g,dataPolygon(data.frame(x=c(x1,x1,x2,x2),y=c(0,1,1,0)),colour=NA,fill="white",alpha=0.35))

x1<-wn2r(0.95,85)
x2<-wn2r(0.95+gap,85)
g<-addG(g,dataPolygon(data.frame(x=c(x1,x1,x2,x2),y=c(0,1,1,0)),colour=NA,fill="white",alpha=0.35))



print(g)
