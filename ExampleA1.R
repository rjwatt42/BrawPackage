
hypothesis<-getHypothesis("2C")
design<-getDesign("Within")
explore<-makeExplore(nsims=100,NULL,"Usage",hypothesis=hypothesis,design=design)



explore<-makeExplore(nsims=10,NULL,"n")

hypothesis=getHypothesis("Psych")
hypothesis$effect$world$populationNullp<-0
explore<-makeExplore(nsims=1000,NULL,"n",max_n=1000,xlog=TRUE,hypothesis=hypothesis)

expect<-makeExpected(nsims=100000,hypothesis=hypothesis,evidence=makeEvidence(shortHand=TRUE))
expect<-makeExpected(nsims=100000,hypothesis=makeHypothesis(effect=makeEffect(rIV=0.3)),evidence=makeEvidence(shortHand=TRUE))


expect<-makeExpected(nsims=1000,doingNull=TRUE)
showExpected(expect)
showExpected(expect,dimension="2D")


hypothesis=getHypothesis("Psych")
expect<-makeExpected(nsims=1000,hypothesis=hypothesis)
showExpected(expect,"fDR")

hypothesis=getHypothesis("III")
explore<-makeExplore(nsims=1000,NULL,"rIVIV2",hypothesis=hypothesis)
showExplore(explore,showType="r",effectType="unique")

