###################

h<-getHypothesis("PsychF")
d<-getDesign("Psych")

showMultiple(doMultiple(100,hypothesis=h,design=d),showType="ws;wp",dimension="2D")


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
print(showPossible(p,"Populations",view="3D",axisScale=1.2,walls="none"))
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

showExplore(doExplore(100,explore=makeExplore('rIV',17,0,0.8)),"p(sig)")

##################

world<-makeWorld(TRUE,"biasedsample",populationPDFmu = 0.3,populationPDFk = 42,populationNullp = 0.)
h<-makeHypothesis(effect=makeEffect(world=world))
showSystem("hypothesis",hypothesis=h)
