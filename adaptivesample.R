########################

braw.env$minRp<<-0.3

world<-makeWorld(TRUE,"Exp","z",0.3,0,0)
h0<-makeHypothesis(effect=makeEffect(world=world))
world<-makeWorld(TRUE,"Exp","z",0.3,0,0.5)
h1<-makeHypothesis(effect=makeEffect(world=world))
# d1<-getDesign("Psych")
showMultiple(doMultiple(500,hypothesis=h1,design=d1),showType = "wp")


########################

rp<-0.3
minN<-20
maxN<-250

h1<-makeHypothesis(effect=makeEffect(rp))
# d1<-getDesign("Psych")
d1<-makeDesign(42)

mainRes<-c()
for (isim in 1:1000) {
sample<-list(iv=c(),dv=c())
res<-data.frame(rs=c(),n=c(),w=c())
while (1==1) {
  s1<-doSample(hypothesis=h1,design=d1)
  n<-length(sample$iv)+length(s1$iv)
  sample<-list(participant=1:n,iv=c(sample$iv,s1$iv),dv=c(sample$dv,s1$dv),hypothesis=s1$hypothesis,design=s1$design,evidence=sample$evidence)
  rs<-doAnalysis(sample)$rIV
  w<-rn2w(rs,n)
  res<-rbind(res,data.frame(rs=rs,n=n,w=w))
  d1$sN<-max(minN,min(maxN,rw2n(rs,0.8)-n))
  s1$dv<-sample$dv
  if (nrow(res)>5 && ((w<0.15) || (w>0.9))) break
}
print(c(brawFormat(n),brawFormat(rs,digits=2),brawFormat(w,digits=2)))
# plot(1:length(res$w),res$w,"l")
mainRes<-c(mainRes,rs)
}

hist(mainRes,breaks=seq(-1,1,0.01))
