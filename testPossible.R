
res<-c()
for (i in 1:10) {
  m<-makePossible(rr(8,0.1),sigOnly=FALSE,sigOnlyCompensate=FALSE,UseSource="world",UsePrior="world",prior=getWorld("PsychF"))
  res<-c(res,doPossible(m)$mle)  
}

print(c(mean(res),sd(res)))

