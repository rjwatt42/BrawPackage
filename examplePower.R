rps<-tanh(wn2z(0.5,42))
n<-42
pval<-0.05
w<-0.5


for (rp in rps) {
world<-makeWorld(TRUE,"Gauss","z",1/sqrt(rp2n(rp,pval)-3),atanh(rp),0)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(rw2n(rp,w))

doMultiple(100,NA,hypothesis=h,design=d)
print(showMultiple(showType="wp",orientation = "horz"))
}


world<-makeWorld(TRUE,"Gauss","z",1/sqrt(50-3),atanh(0.3),0)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(62)

doMultiple(100,NA,hypothesis=h,design=d)
print(showMultiple(showType="wp",orientation = "horz"))

