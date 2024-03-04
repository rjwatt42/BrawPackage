
hypothesis<-getHypothesis("2C")
design<-getDesign("Within")

explore<-makeExplore(nsims=1000,NULL,"Usage",hypothesis=hypothesis,design=design)



explore<-makeExplore(nsims=10,NULL,"n")

hypothesis=getHypothesis("Psych")
explore<-makeExplore(nsims=1000,NULL,"n",max_n=1000,xlog=TRUE,hypothesis=hypothesis)



expect<-makeExpected(nsims=1000,doingNull=TRUE)
showExpected(expect)


expect<-makeExpected(nsims=1000,hypothesis=getHypothesis("III"))
showExpected(expect)

hypothesis=getHypothesis("III")
explore<-makeExplore(nsims=10,NULL,"n",max_n=1000,xlog=TRUE,hypothesis=hypothesis)
showExplore(explore,showType="r")

