rp<-0.3039314
n<-42
w<-0.5
zp<-wn2z(w,n)

world<-makeWorld(TRUE,"Gauss","z",1/sqrt(n-3),zp,0)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(n)

doMultiple(8000,NA,hypothesis=h,design=d)
print(showMultiple(showType="wp",orientation="horz"))

#########################
no<-42
zp<-wn2z(0.5,no)
w<-0.8
n<-rw2n(tanh(zp),w)

world<-makeWorld(TRUE,"Gauss","z",1/sqrt(no-3),zp,0)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(n)

doMultiple(99000,NA,hypothesis=h,design=d)
print(showMultiple(showType="wp",orientation="horz"))

############################

no<-42
zp<-wn2z(0.5,no)
w<-0.8
n<-rw2n(tanh(zp),w)
sigOnly<-FALSE

world<-makeWorld(TRUE,"sample",populationPDFk=1/sqrt(no-3),populationPDFmu=tanh(zp),sigOnly=sigOnly)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(n)

doMultiple(1000,NA,hypothesis=h,design=d)
print(showMultiple(showType="wp",orientation="horz"))

