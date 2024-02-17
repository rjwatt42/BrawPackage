
hypothesis<-makeHypothesis(effect=makeEffect(rIV=0.3))
expectedResult1<-makeExpected(nsim=1000,hypothesis=hypothesis,
                              autoShow=TRUE)

expectedResult2<-makeExpected(nsim=1000,hypothesis=hypothesis,doingNull=TRUE,
                              autoShow=TRUE,type="NHSTErrors")

hypothesis<-getHypothesis("Psych")
design<-getDesign("Psych")
expectedResult3<-makeExpected(nsim=1000,hypothesis=hypothesis,design=design,
                              autoShow=TRUE)

showExpected(expectedResult3,type="NHSTErrors")

evidence<-makeEvidence(shortHand = TRUE)

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis,design,evidence,
                           autoShow = TRUE, Explore_show = "p(sig)")

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis,design,evidence,doingNull=TRUE,
                           autoShow = TRUE, Explore_show = "NHSTErrors")
