###########################################
#

source("StartUp.R")

###########################################
#

hypothesis1<-makeHypothesis(effect=makeEffect(rIV=0.3))
hypothesis2<-getHypothesis("Psych")
design2<-getDesign("Psych")
evidence2<-makeEvidence(shortHand = TRUE)

###########################################
#

expectedResult1<-makeExpected(nsim=1000,hypothesis=hypothesis1,
                              autoShow=TRUE)

expectedResult2<-makeExpected(nsim=1000,hypothesis=hypothesis1,doingNull=TRUE,
                              autoShow=TRUE,showType="NHSTErrors")

expectedResult3<-makeExpected(nsim=1000,hypothesis=hypothesis2,design=design2,
                              autoShow=TRUE)

###########################################
#

exploreResult1<-makeExplore(1000,exploreType="n",hypothesis = hypothesis1,
                            autoShow = TRUE, showType="p(sig)")

exploreResult2<-makeExplore(1000,exploreType="n",hypothesis = hypothesis1,doingNull=TRUE,
                            autoShow = TRUE, showType="NHSTErrors")

exploreResult4<-makeExplore(1000,exploreType="n",hypothesis2,design2,evidence2,
                           autoShow = TRUE, showType = "p(sig)")

exploreResult5<-makeExplore(1000,exploreType="n",hypothesis2,design2,doingNull=TRUE,
                           autoShow = TRUE, showType="NHSTErrors")

exploreResult6<-makeExplore(1000,exploreType="n",hypothesis2,design2,doingNull=TRUE,
                           min_n=10,max_n=1000,xlog=TRUE,
                           autoShow = TRUE, showType="NHSTErrors")
