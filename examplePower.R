rps<-c(0.1,0.4)
pval<-1E-5

for (rp in rps) {
world<-makeWorld(TRUE,"Gauss","z",1/sqrt(rp2n(rp,pval)-3),atanh(rp),0)
h<-makeHypothesis(effect=makeEffect(world=world))
d<-makeDesign(rw2n(rp,0.8))

doMultiple(100,NA,hypothesis=h,design=d)
print(showMultiple(showType="wp"))
}
