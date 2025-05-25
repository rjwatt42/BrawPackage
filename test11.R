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
print(showPossible(wn2r(0.5,n),"Samples",view="3D",axisScale=c(-0.5,1),walls="none"))

###################
n=42

w<-makeWorld(TRUE,populationPDF="Single",populationPDFk = wn2r(0.5,n),populationNullp = 0.5)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",axisScale=c(-0.5,1),walls="none"))

###################
n=42

w<-makeWorld(TRUE,populationPDF="Single",populationPDFk = wn2r(0.8,n),populationNullp = 0.5)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",axisScale=c(-0.5,1),walls="none"))

###################
n=120

w<-makeWorld(TRUE,populationPDF="Single",populationPDFk = wn2r(0.5,42),populationNullp = 0.5)
h<-makeHypothesis(effect=makeEffect(world=w))
d<-makeDesign(n)

setBrawDef("hypothesis",h)
setBrawDef("design",d)
print(showPossible(wn2r(0.5,n),"Samples",view="3D",axisScale=c(-0.5,1),walls="none"))

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

showExplore(doExplore(400,explore=makeExplore('rIV',17,0,0.8)),"p(sig)")

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
