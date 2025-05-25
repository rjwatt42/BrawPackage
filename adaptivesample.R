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

setBrawEnv("npoints",1001)

world<-getWorld("PsychF")
h1<-makeHypothesis(effect=makeEffect(world=world))
d1<-getDesign("Psych")
d1$Replication$On<-TRUE
d1$Replication$PowerPrior<-"World"
showMultiple(doMultiple(0,NULL,hypothesis=h1,design=d1),showType = "wp",orientation="horz")


########################

setBrawEnv("npoints",1001)

world<-getWorld("PsychF")
h1<-makeHypothesis(effect=makeEffect(world=world))
d1<-getDesign("Psych")
d1$Replication$On<-FALSE
d1$Replication$PowerPrior<-"World"
showMultiple(doMultiple(0,NULL,hypothesis=h1,design=d1),showType = "rp",orientation="horz")


########################

setBrawEnv("npoints",1001)

world<-getWorld("PsychF")
h1<-makeHypothesis(effect=makeEffect(world=world))
d1<-getDesign("Psych")
d1$Replication$On<-TRUE
d1$Replication$Keep="Cautious"
d1$Replication$PowerPrior<-"None"
showMultiple(doMultiple(0,NULL,hypothesis=h1,design=d1),showType = "rp",orientation="horz")

########################

setBrawEnv("npoints",1001)

world<-getWorld("PsychF")
h1<-makeHypothesis(effect=makeEffect(world=world))
d1<-getDesign("Psych")
d1$Replication$On<-TRUE
d1$Replication$Keep="MetaAnalysis"
d1$Replication$PowerPrior<-"None"
showMultiple(doMultiple(0,NULL,hypothesis=h1,design=d1),showType = "rp",orientation="horz")

########################

h1<-makeHypothesis(effect=makeEffect(0))
d1<-getDesign("Psych")
d1$Replication$On<-TRUE
d1$Replication$Keep="MetaAnalysis"
d1$Replication$PowerPrior<-"None"
showMultiple(doMultiple(1000,NULL,hypothesis=h1,design=d1),showType = "NHST",orientation="horz")

########################

setBrawEnv("minRp",0.3)

world<-makeWorld(TRUE,"Exp","z",0.3,0,0)
h0<-makeHypothesis(effect=makeEffect(world=world))
world<-makeWorld(TRUE,"Exp","z",0.3,0,0.5)
h1<-makeHypothesis(effect=makeEffect(world=world))
# d1<-getDesign("Psych")
showMultiple(doMultiple(500,hypothesis=h1,design=d1),showType = "wp",orientation="horz")


########################

rp<-0.0
minN<-20
maxN<-250
rule<-"w"
rule<-"se"
rcrit<-0.01

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
  rs<-doAnalysis(sample)
  w<-rn2w(rs$rIV,n)
  res<-rbind(res,data.frame(rs=rs$rIV,n=n,w=w))
  d1$sN<-max(minN,min(maxN,rw2n(rs$rIV,0.8)-n))
  s1$dv<-sample$dv
  if (nrow(res)>5) 
  switch(rule,
         "w"={if ((w<0.15) || (w>0.9)) break},
         "se"={
           if (abs(rs$rIV)-rs$rFullse*2 > rcrit) { outcome<-TRUE; break}
           if (abs(rs$rIV)+rs$rFullse*2 < rcrit) { outcome<-FALSE; break}
           }
         )
}
print(c(brawFormat(n),brawFormat(rs$rIV,digits=2),brawFormat(w,digits=2),outcome))
# plot(1:length(res$w),res$w,"l")
mainRes<-c(mainRes,rs$rIV)
}

hist(mainRes,breaks=seq(-1,1,0.01))
