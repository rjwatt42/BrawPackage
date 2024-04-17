###########################################
#

source("StartUp.R")

###########################################
#

hypothesis1<-makeHypothesis(IV=makeVariable("IV","Categorical"),effect=makeEffect(rIV=0.3))
design1<-makeDesign(sIV1Use="Between")
design2<-makeDesign(sIV1Use="Within")

sample<-makeSample(hypothesis1,design1)
showSample(sample)
analysis<-doAnalysis(sample=sample)
showDescription(analysis)

###########################################
#

expectedResult1<-doExpected(nsim=1000,hypothesis=hypothesis1,
                              autoShow=TRUE)

expectedResult2<-doExpected(nsim=1000,hypothesis=hypothesis1,doingNull=TRUE,
                              autoShow=TRUE,showType="NHST")

expectedResult3<-doExpected(nsim=1000,hypothesis=hypothesis2,design=design2,
                              autoShow=TRUE)

###########################################
#

exploreResult1<-doExplore(1000,exploreType="n",hypothesis = hypothesis1,
                            autoShow = TRUE, showType="p(sig)")

exploreResult2<-doExplore(1000,exploreType="n",hypothesis = hypothesis1,doingNull=TRUE,
                            autoShow = TRUE, showType="NHST")

exploreResult4<-doExplore(1000,exploreType="n",hypothesis2,design2,evidence2,
                           autoShow = TRUE, showType = "p(sig)")

exploreResult5<-doExplore(1000,exploreType="n",hypothesis2,design2,doingNull=TRUE,
                           autoShow = TRUE, showType="NHST")

exploreResult6<-doExplore(1000,exploreType="n",hypothesis2,design2,doingNull=TRUE,
                           min_n=10,max_n=1000,xlog=TRUE,
                           autoShow = TRUE, showType="NHST")
